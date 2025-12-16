;; NOTE: Common filesystem/process helpers are provided by (spkg core util).

(define (get-cache-dir)
  (define env (get-environment-variable "XDG_CACHE_HOME"))
  (if env
      env
      (string-append (get-environment-variable "HOME") "/.cache")))

(define cache-dir (string-append (get-cache-dir) "/spkg/dependencies"))
(define git-dir (string-append cache-dir "/git"))
(define git-src-dir (string-append git-dir "/src"))

(define oci-dir (string-append cache-dir "/oci"))
(define oci-src-dir (string-append oci-dir "/src"))

(define cache-roots-initialized? #f)
(define resolved-git-sources '())
(define resolved-oci-sources '())

(define (ensure-cache-roots!)
  (unless cache-roots-initialized?
    (for-each ensure-directory (list cache-dir git-dir git-src-dir oci-dir oci-src-dir))
    (set! cache-roots-initialized? #t)))

(define (make-temp-dir)
  (capture-required-line "mktemp -d /tmp/spkg.XXXXXX"))

(define (with-temp-dir proc)
  (let ((dir (make-temp-dir)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc dir))
      (lambda () (delete-tree dir)))))


(define (oci-ref dep . maybe-entry)
  (define entry (if (null? maybe-entry) #f (car maybe-entry)))
  (define base (oci-dependency-url dep))
  (define tag
    (cond
      ((oci-dependency-target dep) (oci-dependency-target dep))
      ((and entry (lock-entry-rev entry)) (lock-entry-rev entry))
      (else #f)))
  (if tag
      (string-append base ":" tag)
      base))

(define (oci-stage-ref->tarpath pull-dir)
  ;; Our publish command pushes a file named spkg.tar.gz.
  ;; Be lenient on pull in case a different name is used.
  (define preferred (string-append pull-dir "/spkg.tar.gz"))
  (cond
    ((file-exists? preferred) preferred)
    (else
      ;; Fallback: find the first *.tar.gz or *.tgz or *.tar
      (let ((candidate
              (capture-required-line
                (string-append
                  "sh -c "
                  (shell-quote
                    (string-append
                      "set -e; cd " (shell-quote pull-dir)
                      " && (ls -1 *.tar.gz *.tgz *.tar 2>/dev/null | head -n 1)"))))))
        (string-append pull-dir "/" candidate)))))

(define (oci-relative-cache-name dep checksum)
  (string-append (name->path (oci-dependency-name dep)) "-" checksum))

(define (stage-oci-cache! dep expected-checksum . maybe-entry)
  (define entry (if (null? maybe-entry) #f (car maybe-entry)))
  (with-temp-dir
    (lambda (tmp-dir)
      (define pull-dir (string-append tmp-dir "/pull"))
      (define unpack-dir (string-append tmp-dir "/unpack"))
      (ensure-directory pull-dir)
      (ensure-directory unpack-dir)

      (define ref (oci-ref dep entry))
      ;; Pull artifacts into pull-dir
      (system* (string-append "oras pull " (shell-quote ref) " -o " (shell-quote pull-dir)))

      (define tarpath (oci-stage-ref->tarpath pull-dir))
      (unless (file-exists? tarpath)
        (error "OCI pull did not produce expected archive" ref tarpath))

      ;; Extract
      (system* (string-append "tar -xzf " (shell-quote tarpath) " -C " (shell-quote unpack-dir)))

      ;; Compute checksum of extracted tree (ignore spkg.lock if present)
      (define checksum (filesystem-checksum unpack-dir '("spkg.lock")))
      (when (and expected-checksum (not (string=? expected-checksum checksum)))
        (error "Lockfile checksum mismatch" expected-checksum checksum (oci-dependency-name dep)))

      (let* ((relative (oci-relative-cache-name dep checksum))
             (final-path (string-append oci-src-dir "/" relative)))
        (delete-tree final-path)
        (copy-directory unpack-dir final-path)
        (info "Downloaded" " ~a (oci, ~a)" (oci-dependency-name dep) checksum)
        (list relative checksum final-path)))))

(define (oci-lock-entry-compatible? entry dep)
  (and (eq? 'oci (lock-entry-type entry))
       (string=? (lock-entry-url entry) (oci-dependency-url dep))
       (equal? (lock-entry-subpath entry) (oci-dependency-subpath dep))))

(define (build-oci-lock-entry dep checksum source-name rev)
  (make-lock-entry
    (oci-dependency-name dep)
    'oci
    (oci-dependency-url dep)
    rev
    checksum
    (oci-dependency-subpath dep)
    source-name
    #f))

(define (oci-dependency-needs-recompile? dep entry)
  (cond
    ((not entry) #t)
    ((not (oci-lock-entry-compatible? entry dep)) #t)
    ((let ((target (oci-dependency-target dep))
           (locked-rev (lock-entry-rev entry)))
       (and target
            (or (not locked-rev)
                (not (string=? target locked-rev)))))
     #t)
    ((let ((source-path (lock-entry->path entry)))
       (not (and source-path (file-exists? source-path)))))
    (else #f)))

(define (install-new-oci-dependency dep lock)
  (let* ((stage (stage-oci-cache! dep #f))
         (source-name (car stage))
         (checksum (cadr stage))
         (path (caddr stage))
         (rev (oci-dependency-target dep)))
    (when lock
      (lockfile-set-entry! lock (build-oci-lock-entry dep checksum source-name rev)))
    path))

(define (restore-oci-dependency dep entry lock)
  (let* ((stage (stage-oci-cache! dep (lock-entry-checksum entry) entry))
         (source-name (car stage))
         (checksum (cadr stage))
         (path (caddr stage)))
    (when lock
      (lockfile-set-entry! lock (build-oci-lock-entry dep checksum source-name (lock-entry-rev entry))))
    path))

(define (ensure-oci-entry-source dep entry lock)
  (let ((path (lock-entry->path entry)))
    (if (file-exists? path)
        path
        (restore-oci-dependency dep entry lock))))

(define (oci-dependency->runops dep install-path needs-recompile?)
  (define path (if (oci-dependency-subpath dep)
                   (string-append install-path "/" (oci-dependency-subpath dep))
                   install-path))
  (cond
    ((file-exists? (string-append path "/spkg.scm"))
      (unless (file-exists? (string-append path "/src"))
        (error (string-append
                 "Dependency '"
                 (name->string (oci-dependency-name dep))
                 "' does not contain 'src' directory.")))
      (when needs-recompile?
        (info "Build" " Recompiling ~a" (name->string (oci-dependency-name dep))))
      (runops (list (string-append path "/src")) '() needs-recompile?))
    (else
      (warn "Warning" ": not an spkg package, including raw source path: ~a" path)
      (runops (list path) '() needs-recompile?))))

(define (oci-dependency-has-manifest? dep install-path)
  (define p (if (oci-dependency-subpath dep)
                (string-append install-path "/" (oci-dependency-subpath dep))
                install-path))
  (file-exists? (string-append p "/spkg.scm")))

(define (oci-dependency-install dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (ensure-cache-roots!)
  (define entry (and lock (lockfile-ref lock (oci-dependency-name dep))))
  (define needs-recompile? (oci-dependency-needs-recompile? dep entry))
  (define install-path
    (if (and entry (oci-lock-entry-compatible? entry dep))
        (ensure-oci-entry-source dep entry lock)
        (install-new-oci-dependency dep lock)))
  (remember-oci-source! dep install-path)
  ;; Persist whether this oci dep is a raw source (no manifest).
  (when lock
    (let* ((current (lockfile-ref lock (oci-dependency-name dep)))
           (raw? (and current (not (oci-dependency-has-manifest? dep install-path)))))
      (when (and current (not (equal? (lock-entry-raw? current) raw?)))
        (lockfile-set-entry! lock
          (make-lock-entry
            (lock-entry-name current)
            (lock-entry-type current)
            (lock-entry-url current)
            (lock-entry-rev current)
            (lock-entry-checksum current)
            (lock-entry-subpath current)
            (lock-entry-source current)
            raw?)))))
  (oci-dependency->runops dep install-path needs-recompile?))

(define (oci-dependency-update! dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (ensure-cache-roots!)
  (let ((install-path (install-new-oci-dependency dep lock)))
    (remember-oci-source! dep install-path)
    install-path))



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

(define (remember-oci-source! dep path)
  (define name (oci-dependency-name dep))
  (define existing (assoc name resolved-oci-sources))
  (if existing
      (set-cdr! existing path)
      (set! resolved-oci-sources (cons (cons name path) resolved-oci-sources))))

(define (git-dependency-source-root dep)
  (let ((pair (assoc (git-dependency-name dep) resolved-git-sources)))
    (if pair
        (cdr pair)
        (string-append git-src-dir "/" (name->path (git-dependency-name dep))))))

(define (oci-dependency-source-root dep)
  (let ((pair (assoc (oci-dependency-name dep) resolved-oci-sources)))
    (if pair
        (cdr pair)
        (string-append oci-src-dir "/" (name->path (oci-dependency-name dep))))))

(define (git-dependency-manifest-path dep)
  (define base (git-dependency-source-root dep))
  (define subpath (if (git-dependency-subpath dep)
                      (string-append "/" (git-dependency-subpath dep))
                      ""))
  (string-append base subpath "/spkg.scm"))

(define (oci-dependency-manifest-path dep)
  (define base (oci-dependency-source-root dep))
  (define subpath (if (oci-dependency-subpath dep)
                      (string-append "/" (oci-dependency-subpath dep))
                      ""))
  (string-append base subpath "/spkg.scm"))
(define (path-dependency-manifest-path dep)
  (string-append (path-dependency-path dep) "/spkg.scm"))

(define (git-dependency-install-dir dep)
  (git-dependency-source-root dep))

(define (oci-dependency-install-dir dep)
  (oci-dependency-source-root dep))

(define (lock-entry->path entry)
  (case (lock-entry-type entry)
    ((git) (string-append git-src-dir "/" (lock-entry-source entry)))
    ((oci) (string-append oci-src-dir "/" (lock-entry-source entry)))
    (else #f)))

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
    source-name
    #f))

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
    (path-lock-entry-mode dep)
    (path-dependency-raw? dep)))

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
    ((oci-dependency? dep)
      (oci-dependency-needs-recompile?
        dep
        (lockfile-ref lock (oci-dependency-name dep))))
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

(define (git-dependency-has-manifest? dep install-path)
  (define p (if (git-dependency-subpath dep)
                (string-append install-path "/" (git-dependency-subpath dep))
                install-path))
  (file-exists? (string-append p "/spkg.scm")))

(define (git-dependency-install dep . maybe-lock)
  (define lock (if (null? maybe-lock) #f (car maybe-lock)))
  (ensure-cache-roots!)
  (define entry (and lock (lockfile-ref lock (git-dependency-name dep))))
  (define needs-recompile? (git-dependency-needs-recompile? dep entry))
  (define install-path
    (if (and entry (git-lock-entry-compatible? entry dep))
        (ensure-entry-source dep entry lock)
        (install-new-git-dependency dep lock)))
  ;; Persist whether this git dep is a raw source (no manifest).
  (when lock
    (let* ((current (lockfile-ref lock (git-dependency-name dep)))
           (raw? (and current (not (git-dependency-has-manifest? dep install-path)))))
      (when (and current (not (equal? (lock-entry-raw? current) raw?)))
        (lockfile-set-entry! lock
          (make-lock-entry
            (lock-entry-name current)
            (lock-entry-type current)
            (lock-entry-url current)
            (lock-entry-rev current)
            (lock-entry-checksum current)
            (lock-entry-subpath current)
            (lock-entry-source current)
            raw?)))))
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

