
(define (normalize-impl-name name)
  (define lname (string-downcase name))

  (cond 
    ((or (string=? lname "capy")
         (string=? lname "capyscheme")
         (string=? lname "capy-scheme"))
     'capyscheme)
    ((or (string=? lname "chibi")
         (string=? lname "chibi-scheme")
         (string=? lname "chibischeme"))
     'chibischeme)
    ((or (string=? lname "gambit")
         (string=? lname "gambitscheme")
         (string=? lname "gambit-scheme"))
     'gambit)
    ((or (string=? lname "guile"))
     'guile)
    ((or (string=? lname "mit")
         (string=? lname "mitscheme")
         (string=? lname "mit-scheme"))
     'mit-scheme)
    ((or (string=? lname "racket"))
     'racket)
    ((or (string=? lname "gauche"))
     'gauche)
    ((or (string=? lname "scheme48")
         (string=? lname "scheme-48")
         (string=? lname "s48"))
     'scheme48)
    ((or (string=? lname "ypsilon"))
     'ypsilon)
    ((or (string=? lname "chicken")
         (string=? lname "chicken-scheme")
         (string=? lname "chickenscheme"))
     'chicken-scheme)
    ((or (string=? lname "chez")
         (string=? lname "chezscheme")
         (string=? lname "chez-scheme"))
     'chezscheme)
    (else
     (error "Unknown Scheme implementation" name))))



(define (get-implementation-name)
  (define var (get-environment-variable "SCHEME"))
  (if var 
    (normalize-impl-name var)
    (begin 
      (let ((err (current-error-port)))
        (display "Warning: SCHEME environment variable not set; defaulting to 'capyscheme'\n" err)
        'capyscheme))))

(define (implementation->binary-name impl)
  (case impl 
    ((capyscheme) "capy")
    ((chibischeme) "chibi-scheme")
    ((gambit) "gambit")
    ((guile) "guile")
    ((mit-scheme) "mit-scheme")
    ((racket) "racket")
    ((gauche) "gosh")
    ((scheme48) "s48")
    ((ypsilon) "ypsilon")
    ((chicken-scheme) "csi")
    ((chezscheme) "scheme")
    (else 
      (error "Unknown Scheme implementation" impl))))

(define current-implementation (make-parameter (get-implementation-name)))

(define (make-version-tracker)
  (vector '()))

(define (version-tracker-entries tracker)
  (vector-ref tracker 0))

(define (version-tracker-entries-set! tracker entries)
  (vector-set! tracker 0 entries))

(define (register-git-version! tracker dep)
  (define entries (version-tracker-entries tracker))
  (define name (git-dependency-name dep))
  (define url (git-dependency-url dep))
  (define target (git-dependency-target dep))
  (let ((existing (assq name entries)))
    (if existing
        (let* ((info (cdr existing))
               (existing-url (car info))
               (existing-target (if (and (pair? (cdr info)))
                                    (cadr info)
                                    #f)))
          (unless (string=? existing-url url)
            (error "Conflicting git dependency sources"
                   name existing-url url))
          (when (and existing-target target (not (string=? existing-target target)))
            (error "Conflicting git dependency revisions"
                   name existing-target target))
          (when (and (not existing-target) target)
            (error "One dependency specifies a target revision, the other does not"
                   name existing-target target))
          (when (and (not existing-target) target)
            (set-cdr! existing (list existing-url target))))
        (version-tracker-entries-set! tracker
          (cons (cons name (list url target)) entries)))))


(define (install-dependencies 
  manifest
  dev-deps?
  tracker
  visited
  lock)
  ;; A dependency is considered "direct" when installing the root manifest.
  ;; For determinism, missing lock entries are allowed ONLY for direct deps.
  ;; Transitive deps must already be recorded in the lockfile (typically via `spkg update`).
  (define direct? (null? visited))

  (define (ensure-locked! dep)
    (when (and lock (not direct?))
      (cond
        ((git-dependency? dep)
         (unless (lockfile-ref lock (git-dependency-name dep))
           (raise-lockfile-error
             "Missing lockfile entry for transitive dependency; run `spkg update`"
             (git-dependency-name dep))))
        ((oci-dependency? dep)
         (unless (lockfile-ref lock (oci-dependency-name dep))
           (raise-lockfile-error
             "Missing lockfile entry for transitive dependency; run `spkg update`"
             (oci-dependency-name dep))))
        ((path-dependency? dep)
         (unless (lockfile-ref lock (path-dependency-name dep))
           (raise-lockfile-error
             "Missing lockfile entry for transitive dependency; run `spkg update`"
             (path-dependency-name dep))))
        ((system-dependencies? dep)
          (for-each 
            (lambda (name)
              (unless (system-has-library? name)
                (raise-lockfile-error
                  "Missing system dependency"
                  name)))
            (system-dependencies-names dep)))
        
        (else #t))))

  (define (install-one dep manifest tracker visited)
    (ensure-locked! dep)
    (cond 
      ((git-dependency? dep)
        (register-git-version! tracker dep)
        (let* ((ops (git-dependency-install dep lock))
               (manifest-path (git-dependency-manifest-path dep)))
          (cond 
            ((file-exists? manifest-path)
    (let* ((dep-manifest (read-manifest manifest-path))
      (nested-deps (install-dependencies dep-manifest dev-deps? tracker visited lock)))
                (runops-merge ops nested-deps)))
                ;(let loop ((rest nested-deps) (ops ops))
                ;  (if (null? rest)
                ;    ops 
                ;    (loop (cdr rest) (runops-merge ops (car rest)))))))
            (else 
              ;; no manifset: raw dependency
              ops))))
      ((oci-dependency? dep)
        (let* ((ops (oci-dependency-install dep lock))
               (manifest-path (oci-dependency-manifest-path dep)))
          (cond
            ((file-exists? manifest-path)
    (let* ((dep-manifest (read-manifest manifest-path))
      (nested-deps (install-dependencies dep-manifest dev-deps? tracker visited lock)))
                (runops-merge ops nested-deps)))
            (else ops))))
      ((path-dependency? dep)
        (let ((ops (path-dependency-install dep lock)))
          (if (path-dependency-raw? dep)
              ops
              (let ((manifest-path (path-dependency-manifest-path dep)))
                (if (file-exists? manifest-path)
          (let* ((dep-manifest (read-manifest manifest-path))
            (nested-deps (install-dependencies dep-manifest dev-deps? tracker visited lock)))
                      (runops-merge ops nested-deps))
                    ops)))))
      ((system-dependencies? dep)
        (for-each 
          (lambda (name)
            (unless (system-has-library? name)
              (raise-lockfile-error
                "Missing system dependency"
                name)))
          (system-dependencies-names dep))
        (runops '() '() #f))
      (else ops)))
  
  (define deps
      (if dev-deps?
          (append (manifest-dependencies manifest)
                  (manifest-dev-dependencies manifest))
          (manifest-dependencies manifest)))

  (fold 
    (lambda (dep acc)
      (runops-merge acc (install-one dep manifest tracker visited)))
    (runops '() '() #f)
    deps))

(define (manifest-root-dir manifest)
  (manifest-root-directory manifest))

(define (manifest-lockfile-path manifest)
  (define dir (manifest-root-dir manifest))
  (if (or (not dir) (string=? dir "") (string=? dir "."))
      default-lockfile-name
      (string-append dir "/" default-lockfile-name)))

(define (manifest-root-checksum manifest)
  (filesystem-checksum (manifest-root-dir manifest) '("spkg.lock")))

(define (manifest-root-checksum-mismatch? manifest lock)
  (define stored (lockfile-root-checksum lock))
  (define current (manifest-root-checksum manifest))
  (or (not stored) (not (string=? stored current))))

(define (force-runops-recompile ops)
  (define new (runops (runops-append-path ops)
          (runops-prepend-path ops)
          #t))
  (verboseln "DEBUG " "forced recompile in runops=~a" (runops-recompile? new))
  new)

(define (lockfile-set-root-checksum! lock manifest)
  (lockfile-root-checksum-set! lock (manifest-root-checksum manifest)))

(define (manifest-install-dependencies manifest dev-deps?)
  (define lock (load-lockfile (manifest-lockfile-path manifest)))
  (define tracker (make-version-tracker))
  (define root-mismatch? (manifest-root-checksum-mismatch? manifest lock))
  (define ops (install-dependencies manifest dev-deps? tracker '() lock))

  (define final-ops (if root-mismatch? (force-runops-recompile ops) ops))
  (when root-mismatch? 
    (info "Build" " Root directory modified, forcing recompilation."))
  (lockfile-set-root-checksum! lock manifest)
  (save-lockfile! lock)
  
  final-ops)


(define (manifest-needs-recompile? manifest)
  (define lock (load-lockfile (manifest-lockfile-path manifest)))
  (define deps (manifest-dependencies manifset))
  (or (manifest-root-checksum-mismatch? manifest lock)
      (ormap (lambda (dep)
               (dependency-needs-recompile? dep lock))
             deps)))

(define (update-dependencies manifest dev-deps? tracker visited lock enabled-features disabled-features)
  (define enabled (effective-feature-set manifest enabled-features disabled-features))
  (define deps
      (if dev-deps?
          (append (manifest-dependencies manifest)
                  (manifest-dev-dependencies manifest))
          (manifest-dependencies manifest)))

  (fold
    (lambda (dep acc)
      (append acc (update-dependency-entry dep dev-deps? tracker visited lock)))
    '()
    deps))

(define (update-dependency-entry dep dev-deps? tracker visited lock)
  (cond
    ((git-dependency? dep)
      (register-git-version! tracker dep)
      (update-git-dependency dep dev-deps? tracker visited lock))
    ((oci-dependency? dep)
      (update-oci-dependency dep dev-deps? tracker visited lock))
    ((path-dependency? dep)
      (update-path-dependency dep dev-deps? tracker visited lock))
    (else '())))

(define (update-oci-dependency dep dev-deps? tracker visited lock)
  (define entry (lockfile-ref lock (oci-dependency-name dep)))
  (define old-checksum (and entry (lock-entry-checksum entry)))
  (define new-checksum old-checksum)
  (define updated? #f)
  ;; For OCI deps, we re-pull and re-lock (tag may advance).
  (oci-dependency-update! dep lock)
  (let ((current (lockfile-ref lock (oci-dependency-name dep))))
    (set! new-checksum (and current (lock-entry-checksum current))))
  (set! updated? (not (and old-checksum new-checksum (string=? old-checksum new-checksum))))
  (cond
    ((and updated? old-checksum new-checksum)
      (info "Update" " ~a advanced (~a -> ~a)"
        (oci-dependency-name dep)
        old-checksum
        new-checksum))
    ((and updated? new-checksum)
      (info "Update" " ~a locked to ~a"
        (oci-dependency-name dep)
        new-checksum))
    ((and new-checksum)
      (info "Update" " ~a already up to date (~a)"
        (oci-dependency-name dep)
        new-checksum))
    (else
      (info "Update" " ~a fetched" (oci-dependency-name dep))))

  (let ((manifest-file (oci-dependency-manifest-path dep)))
    (append (if updated? (list (oci-dependency-name dep)) '())
            (update-nested-manifest manifest-file dev-deps? tracker lock visited))))

(define (update-git-dependency dep dev-deps? tracker visited lock)
  (define entry (lockfile-ref lock (git-dependency-name dep)))
  (define old-checksum (and entry (lock-entry-checksum entry)))
  (define new-checksum old-checksum)
  (define updated? #f)
  (define target (git-dependency-target dep))
  (if target
      (git-dependency-install dep lock)
      (begin
        (git-dependency-update! dep lock)
        (let ((current (lockfile-ref lock (git-dependency-name dep))))
          (set! new-checksum (and current (lock-entry-checksum current))))
        (set! updated? (not (and old-checksum new-checksum (string=? old-checksum new-checksum))))
        (cond
          ((and updated? old-checksum new-checksum)
            (info "Update" " ~a advanced (~a -> ~a)"
              (git-dependency-name dep)
              old-checksum
              new-checksum))
          ((and updated? new-checksum)
            (info "Update" " ~a locked to ~a"
              (git-dependency-name dep)
              new-checksum))
          ((and new-checksum)
            (info "Update" " ~a already up to date (~a)"
              (git-dependency-name dep)
              new-checksum))
          (else
            (info "Update" " ~a fetched latest head"
              (git-dependency-name dep))))))
  (let ((manifest-file (git-dependency-manifest-path dep)))
    (append (if (and (not target) updated?)
                (list (git-dependency-name dep))
                '())
            (update-nested-manifest manifest-file dev-deps? tracker lock visited))))

(define (update-path-dependency dep dev-deps? tracker visited lock)
  (define path (path-dependency-path dep))
  (unless (file-exists? path)
    (error (string-append 
             "Path dependency '"
             (path-dependency-name dep)
             "' path '"
             path
             "' does not exist.")))
  (cond
    ((path-dependency-raw? dep) '())
    ((not (file-exists? (path-dependency-manifest-path dep)))
      (error (string-append 
               "Path dependency '"
               (path-dependency-name dep)
               "' path '"
               path
               "' is not an spkg package (missing spkg.scm).")))
    (else
      (update-nested-manifest (path-dependency-manifest-path dep) dev-deps? tracker lock visited))))

(define (update-nested-manifest manifest-file dev-deps? tracker lock visited)
  (if (or (not manifest-file) (not (file-exists? manifest-file)))
      '()
      (let* ((nested (read-manifest manifest-file))
             (canonical (manifest-path nested)))
        (if (and canonical (member canonical visited))
            '()
            (update-dependencies nested dev-deps?
                                 tracker
                                 (if canonical (cons canonical visited) visited)
                                 lock)))))

(define (manifest-update-dependencies manifest dev-deps?)
  (define lock (load-lockfile (manifest-lockfile-path manifest)))
  (define start (manifest-path manifest))
  (define tracker (make-version-tracker))

  ;; First, update entries (advance untargeted git deps, refresh path checksums).
  (define updates (update-dependencies manifest dev-deps?
                                      tracker
                                      (if start (list start) '())
                                      lock))

  ;; Then, ensure the lockfile is complete by installing the full transitive
  ;; closure while allowing missing entries to be created.
  ;; This keeps installs deterministic: afterwards, `manifest-install-dependencies`
  ;; will not need to resolve any transitive deps.
  (install-dependencies manifest dev-deps? (make-version-tracker) '() lock enabled-features disabled-features)

  (lockfile-set-root-checksum! lock manifest)
  (save-lockfile! lock)
  updates)


;; Convert runops to command line arguments for specific Scheme implementation
(define (ops->runargs ops src-dir for-install? manifest)
  (define package (manifest-package manifest))
  (define rnrs (package-rnrs package))
  (define impl (current-implementation))
  (define recompile? (and (not for-install?) (runops-recompile? ops)))
  (define append-paths (runops-append-path ops))
  (define prepend-paths  
    (if (zero? (string-length src-dir))
        (runops-prepend-path ops)
        (cons src-dir (runops-prepend-path ops))))

  (case impl 
    ((capyscheme)
      ;; install will force recompilation anyways
        `(
          ,(case rnrs 
            ((r7rs) "--r7rs")
            ((r6rs) "--r6rs")
            (else ""))
          ,@(if (not (null? prepend-paths)) `("--load-path" ,(string-join prepend-paths ",")) '())
          ,@(if (not (null? append-paths)) `("--append-load-path" ,(string-join append-paths ",")) '())
          ,(if recompile? "--fresh-auto-compile" "")))
    ((guile)
      ;; guile has no --append-load-path, concat paths and then use --load-path
        `(
          ,(case rnrs 
            ((r7rs) "--r7rs")
            ((r6rs) "--r6rs")
            (else ""))
          ,@(map (lambda (p) (string-append "-L " p)) prepend-paths)
          ,@(map (lambda (p) (string-append "-L " p)) append-paths)
          ,(if recompile? "--fresh-auto-compile" "")))
    ((chibischeme)
      
        (unless (eq? rnrs 'r7rs)
          (error "Chibi-Scheme only supports R7RS mode"))
        `(
          ,@(map (lambda (p) (string-append "-I" p)) prepend-paths)
          ,@(map (lambda (p) (string-append "-A" p)) append-paths)))
    ((gauche)
        `(
          ,(case rnrs 
            ((r7rs) "-r7")
            (else (error "Gauche only supports R7RS mode")))
          ,@(map (lambda (p) (string-append "-I" p)) prepend-paths)
          ,@(map (lambda (p) (string-append "-A" p)) append-paths)
          ))
    (else 
      (error "Not yet supported Scheme implementation" impl))))


;; Convert package to a command-line argument list for running it as a program
(define (path->scriptarg path for-install?)
  (define impl (current-implementation))
  (case impl 
    ((capyscheme)
      `("--script" ,path))
    ((guile)
      `("-s" ,path))
    ((chibischeme)
      `(,path))
    ((gauche)
      `(,path))
    (else 
      (error "Not yet supported Scheme implementation" impl))))




;; Given a library name check if current-implementation has it installed.
(define (system-has-library? lib)
  (define libname (string-append "(" (string-join (map 
    (lambda (name)
      (cond 
        ((number? name) (number->string name))
        ((symbol? name) (symbol->string name))))
    lib) " ") ")"))
  
  (verboseln "DEBUG " "Checking system for library ~a" lib)

  (with-temp-dir 
    (lambda (dir)
      (define file (string-append dir "/test.scm"))
      (call-with-output-file file
        (lambda (out)
          (write-string "(import (scheme base) (scheme write) (scheme process-context))\n" out)
          (write-string
            (string-append 
              "(guard (c (else (display \"NOT FOUND\\n\") (exit 0))) (cond-expand\n"
              "  ((library " libname ")\n"
              "   (display \"FOUND\\n\"))\n"
              "  (else\n"
              "   (display \"NOT FOUND\\n\")))\n"
              "(exit 0))\n")
            out)
          (flush-output-port out)
          (let ((line (capture-required-line 
                        (string-append 
                          (implementation->binary-name (current-implementation))
                          " "
                          (string-join (path->scriptarg file #f) " ")))))
            (verboseln "DEBUG " "Library check output: ~a" line)
            (string=? line "FOUND")))))))

;; Check if the current implementation can compile code. If not, checksum is always "invalid"
;; when running with interpreted implementations so that when you change SCHEME it forces a recompile.
(define (implementation-can-compile? impl)
  (case impl
    ((guile capyscheme) #t)
    (else #f)))