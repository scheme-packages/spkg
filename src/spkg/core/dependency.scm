(define (get-cache-dir)
  (define env (get-environment-variable "XDG_CACHE_HOME"))
  (if env
      env
      (string-append (get-environment-variable "HOME") "/.cache")))

(define cache-dir (string-append (get-cache-dir) "/spkg/dependencies"))
(define git-dir (string-append cache-dir "/git"))
(define git-src-dir (string-append git-dir "/src"))

(define cache-roots-initialized? #f)
(define resolved-git-sources '())

(define (shell-quote str)
  (let loop ((idx 0) (acc ""))
    (if (= idx (string-length str))
        (string-append "'" acc "'")
        (let* ((ch (string-ref str idx))
               (fragment (if (char=? ch #\') "'\"'\"'" (string ch))))
          (loop (+ idx 1) (string-append acc fragment))))))

(define (system* cmd)
  (let ((code (system cmd)))
    (unless (zero? code)
      (error "Command failed" cmd code))))

(define (create-directory-recursively path)
  (when path
    (system* (string-append "mkdir -p " (shell-quote path)))))

(define (ensure-directory path)
  (unless (file-exists? path)
    (create-directory-recursively path)))

(define (ensure-cache-roots!)
  (unless cache-roots-initialized?
    (for-each ensure-directory (list cache-dir git-dir git-src-dir))
    (set! cache-roots-initialized? #t)))

(define (delete-tree path)
  (when (and path (file-exists? path))
    (system* (string-append "rm -rf " (shell-quote path)))))

(define (copy-directory src dst)
  (create-directory-recursively (dirname dst))
  (system* (string-append "cp -R " (shell-quote src) " " (shell-quote dst))))

(define (capture-required-line cmd)
  (let* ((proc (process cmd))
         (stdout (list-ref proc 0))
         (stdin (list-ref proc 1))
         (pid (list-ref proc 2)))
    (close-output-port stdin)
    (let ((status (process-wait pid))
          (line (read-line stdout)))
      (close-input-port stdout)
      (unless (zero? status)
        (error "Command failed" cmd status))
      (if (or (eof-object? line) (string=? line ""))
          (error "Command produced no output" cmd)
          line))))

(define filesystem-checksum-script
  "import hashlib, os, sys\npath = os.path.abspath(sys.argv[1])\nexcludes = set(os.path.normpath(p) for p in sys.argv[2:])\nif not os.path.exists(path):\n    raise SystemExit('missing path: %s' % path)\nh = hashlib.sha256()\n\ndef feed_file(target):\n    with open(target, 'rb') as fh:\n        while True:\n            chunk = fh.read(131072)\n            if not chunk:\n                break\n            h.update(chunk)\nif os.path.isfile(path):\n    relname = os.path.basename(path)\n    if relname not in excludes:\n        feed_file(path)\nelse:\n    for root, dirs, files in os.walk(path):\n        dirs[:] = [d for d in dirs if d != '.git' and os.path.relpath(os.path.join(root, d), path) not in excludes]\n        dirs.sort()\n        files.sort()\n        rel = os.path.relpath(root, path)\n        if rel in excludes:\n            continue\n        h.update(rel.encode())\n        for name in files:\n            fp = os.path.join(root, name)\n            relpath = os.path.relpath(fp, path)\n            if relpath in excludes:\n                continue\n            h.update(relpath.encode())\n            feed_file(fp)\nprint(h.hexdigest())\n")

(define (filesystem-checksum path . maybe-excludes)
  (define excludes (if (null? maybe-excludes) '() (car maybe-excludes)))
  (define exclude-args
    (if (null? excludes)
        ""
        (string-append " " (string-join (map shell-quote excludes) " "))))
  (capture-required-line
    (string-append "python3 -c \"" filesystem-checksum-script "\" "
                   (shell-quote path)
                   exclude-args)))

(define (make-temp-dir)
  (capture-required-line "mktemp -d /tmp/spkg.XXXXXX"))

(define (with-temp-dir proc)
  (let ((dir (make-temp-dir)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc dir))
      (lambda () (delete-tree dir)))))



(define (name->path name)
  (cond 
    ((symbol? name) (symbol->string name))
    ((list? name)
      ;; (foo bar baz) -> "foo/bar/baz"
      (string-join (map symbol->string name) "/"))))

(define (name->string name)
  (cond 
    ((symbol? name) (symbol->string name))
    ((list? name)
      ;; (foo bar baz) -> "foo-bar-baz"
      (string-join (map symbol->string name) "-"))))

(define (name->root name)
  (cond 
    ((symbol? name) name)
    ((list? name)
      (car name))))

(define (git-head-checksum dir)
  (capture-required-line
    (string-append "cd " (shell-quote dir) " && git rev-parse HEAD")))

(define (relative-cache-name dep checksum)
  (string-append (name->path (git-dependency-name dep)) "-" checksum))
  
(define (stage-git-cache! dep checkout-target expected-checksum)
  (with-temp-dir
    (lambda (tmp-dir)
      (system* (string-append "git clone --quiet " (shell-quote (git-dependency-url dep))
                              " " (shell-quote tmp-dir)))
      (when checkout-target
        (system* (string-append "cd " (shell-quote tmp-dir)
                                " && git checkout --quiet " (shell-quote checkout-target))))
      (system* (string-append "cd " (shell-quote tmp-dir)
                              " && git submodule update --quiet --init --recursive"))
      (let ((checksum (git-head-checksum tmp-dir)))
        (when (and expected-checksum (not (string=? expected-checksum checksum)))
          (error "Lockfile checksum mismatch" expected-checksum checksum (git-dependency-name dep)))
        (let* ((relative (relative-cache-name dep checksum))
               (final-path (string-append git-src-dir "/" relative)))
          (delete-tree final-path)
          (copy-directory tmp-dir final-path)
          (info "Downloaded" " ~a (~a)" (git-dependency-name dep) checksum)
          (list relative checksum final-path))))))

(define (remember-git-source! dep path)
  (define name (git-dependency-name dep))
  (define existing (assoc name resolved-git-sources))
  (if existing
      (set-cdr! existing path)
      (set! resolved-git-sources (cons (cons name path) resolved-git-sources))))

(define (git-dependency-source-root dep)
  (let ((pair (assoc (git-dependency-name dep) resolved-git-sources)))
    (if pair
        (cdr pair)
        (string-append git-src-dir "/" (name->path (git-dependency-name dep))))))

(define (git-dependency-manifest-path dep)
  (define base (git-dependency-source-root dep))
  (define subpath (if (git-dependency-subpath dep)
                      (string-append "/" (git-dependency-subpath dep))
                      ""))
  (string-append base subpath "/spkg.scm"))
(define (path-dependency-manifest-path dep)
  (string-append (path-dependency-path dep) "/spkg.scm"))

(define (git-dependency-install-dir dep)
  (git-dependency-source-root dep))

(define (lock-entry->path entry)
  (string-append git-src-dir "/" (lock-entry-source entry)))

(define (git-lock-entry-compatible? entry dep)
  (and (eq? 'git (lock-entry-type entry))
       (string=? (lock-entry-url entry) (git-dependency-url dep))
       (equal? (lock-entry-subpath entry) (git-dependency-subpath dep))))

(define (build-lock-entry dep checksum source-name rev)
  (make-lock-entry
    (git-dependency-name dep)
    'git
    (git-dependency-url dep)
    rev
    checksum
    (git-dependency-subpath dep)
    source-name))

(define (path-lock-entry-mode dep)
  (if (path-dependency-raw? dep) "raw" "package"))

(define (build-path-lock-entry dep checksum)
  (make-lock-entry
    (path-dependency-name dep)
    'path
    (path-dependency-path dep)
    #f
    checksum
    #f
    (path-lock-entry-mode dep)))

(define (path-lock-entry-compatible? entry dep)
  (and (eq? 'path (lock-entry-type entry))
       (string=? (lock-entry-url entry) (path-dependency-path dep))
       (string=? (or (lock-entry-source entry) "")
                 (path-lock-entry-mode dep))))

(define (git-dependency-needs-recompile? dep entry)
  (cond
    ((not entry) #t)
    ((not (git-lock-entry-compatible? entry dep)) #t)
    ((let ((target (git-dependency-target dep))
           (locked-rev (lock-entry-rev entry)))
       (and target
            (or (not locked-rev)
                (not (string=? target locked-rev)))))
      #t)
    ((let ((source-path (lock-entry->path entry)))
       (not (and source-path (file-exists? source-path)))))
    (else #f)))

(define (path-dependency-needs-recompile? dep entry . maybe-checksum)
  (define path (path-dependency-path dep))
  (unless (file-exists? path)
    (error (string-append
             "Path dependency '"
             (path-dependency-name dep)
             "' path '"
             path
             "' does not exist.")))
  (cond
    ((not entry) #t)
    ((not (path-lock-entry-compatible? entry dep)) #t)
    (else
      (let ((checksum (if (null? maybe-checksum)
                          (filesystem-checksum path '())
                          (car maybe-checksum))))
        (not (string=? (lock-entry-checksum entry) checksum))))))

(define (dependency-needs-recompile? dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (cond
    ((not lock) #t)
    ((git-dependency? dep)
      (git-dependency-needs-recompile?
        dep
        (lockfile-ref lock (git-dependency-name dep))))
    ((path-dependency? dep)
      (path-dependency-needs-recompile?
        dep
        (lockfile-ref lock (path-dependency-name dep))))
    (else
      (error "Unknown dependency type" dep))))

(define (install-new-git-dependency dep lock)
  (let* ((stage (stage-git-cache! dep (git-dependency-target dep) #f))
         (source-name (car stage))
         (checksum (cadr stage))
         (path (caddr stage)))
    (when lock
      (lockfile-set-entry! lock (build-lock-entry dep checksum source-name (git-dependency-target dep))))
    path))

(define (restore-git-dependency dep entry lock)
  (let* ((stage (stage-git-cache! dep (lock-entry-checksum entry) (lock-entry-checksum entry)))
         (source-name (car stage))
         (checksum (cadr stage))
         (path (caddr stage)))
    (when lock
      (lockfile-set-entry! lock (build-lock-entry dep checksum source-name (lock-entry-rev entry))))
    path))

(define (ensure-entry-source dep entry lock)
  (let ((path (lock-entry->path entry)))
    (if (file-exists? path)
        path
        (restore-git-dependency dep entry lock))))

(define (git-dependency->runops dep install-path needs-recompile?)
  (define path (if (git-dependency-subpath dep)
                   (string-append install-path "/" (git-dependency-subpath dep))
                   install-path))
  (cond
    ((file-exists? (string-append path "/spkg.scm"))
      (unless (file-exists? (string-append path "/src"))
        (error (string-append
                 "Dependency '"
                 (name->string (git-dependency-name dep))
                 "' does not contain 'src' directory.")))
      (when needs-recompile? 
        (info "Build" " Recompiling ~a" (name->string (git-dependency-name dep))))

      (runops (list (string-append path "/src")) '() needs-recompile?))
    (else
      (warn "Warning" ": not an spkg package, including raw source path: ~a" path)
      (runops (list path) '() needs-recompile?))))

(define (git-dependency-install dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (ensure-cache-roots!)
  (define entry (and lock (lockfile-ref lock (git-dependency-name dep))))
  (define needs-recompile? (git-dependency-needs-recompile? dep entry))
  (define install-path
    (if (and entry (git-lock-entry-compatible? entry dep))
        (ensure-entry-source dep entry lock)
        (install-new-git-dependency dep lock)))
  (remember-git-source! dep install-path)
  (git-dependency->runops dep install-path needs-recompile?))

(define (git-dependency-update! dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (ensure-cache-roots!)
  (let ((install-path (install-new-git-dependency dep lock)))
    (remember-git-source! dep install-path)
    install-path))

(define (path-dependency-install dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (define path (path-dependency-path dep))
  (unless (file-exists? path)
    (error (string-append 
             "Path dependency '"
             (path-dependency-name dep)
             "' path '"
             path
             "' does not exist.")))
  (define entry (and lock (lockfile-ref lock (path-dependency-name dep))))
  (define checksum (filesystem-checksum path '()))
  (define needs-recompile?
    (path-dependency-needs-recompile? dep entry checksum))
  (when lock
    (when (and entry
               (path-lock-entry-compatible? entry dep)
               (not (string=? (lock-entry-checksum entry) checksum)))
      (info "Path" " ~a checksum updated (~a -> ~a)"
        (path-dependency-name dep)
        (lock-entry-checksum entry)
        checksum))
    (lockfile-set-entry! lock (build-path-lock-entry dep checksum)))
  (when needs-recompile?
    (info "Build" " Recompiling ~a" (name->string (path-dependency-name dep))))
  (cond
    ((path-dependency-raw? dep)
      (runops (list path) '() needs-recompile?))
    ((not (file-exists? (path-dependency-manifest-path dep)))
      (error (string-append 
               "Path dependency '"
               (path-dependency-name dep)
               "' path '"
               path
               "' is not an spkg package (missing spkg.scm).")))
    (else
      (runops (list (string-append path "/src")) '() needs-recompile?))))

