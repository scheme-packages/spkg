;;; test.sld -- spkg command to run all test files under ./tests

(define-library (spkg cmd test)
  (import
    (srfi 1)
    (srfi 130)
    (scheme base)
    (scheme file)
    (scheme process-context)
    (spkg core compat)
    (spkg core errors)
    (spkg core log)
    (spkg core manager)
    (spkg core manifest)
    (args grammar)
    (args runner)
    (args results))
  (export spkg-test-command)

  (begin
    (define grammar (make-grammar))

    (grammar-add-option! grammar "directory"
      'help: "Directory containing tests (default: ./tests)."
      'value-help: "DIR"
      'defaults-to: "tests")

    (define (scheme-file? path)
      (or (string-suffix? ".scm" path)
          (string-suffix? ".sps" path)
          (string-suffix? ".sld" path)
          (string-suffix? ".sls" path)))

    ;; Recursively collect test files using `directory-list`.
    ;; Returns paths as strings (relative/absolute depending on input).
    (define (collect-test-files dir)
      (let* ((dir* (canonicalize-path-string dir))
             (join (lambda (base name)
                     (string-append base "/" name))))
        (unless (file-exists? dir*)
          (raise-user-error (string-append "Tests directory not found: " dir)))
        (unless (file-directory? dir*)
          (raise-user-error (string-append "Tests path is not a directory: " dir)))

        (let walk ((d dir*))
          (fold
            (lambda (entry acc)
              (let ((p (join d entry)))
                (cond
                  ((file-directory? p)
                   ;; skip hidden dirs to avoid cache noise
                   (if (and (> (string-length entry) 0)
                            (char=? (string-ref entry 0) #\.))
                       acc
                       (append (walk p) acc)))
                  ((and (file-exists? p) (scheme-file? p))
                   (cons p acc))
                  (else acc))))
            '()
            (directory-list d)))))

    (define (run-one-test ops project-src-dir file manifest)
      (define impl (implementation->binary-name (current-implementation)))
      (define cmd
        (string-append
          impl
          " "
          (string-join (ops->runargs ops  project-src-dir #f manifest) " ")
          " "
          (string-join (path->scriptarg file project-src-dir '()) " ")))
      (system cmd))

    (define (run-test command)
      (unless (file-exists? "spkg.scm")
        (raise-manifest-error "No spkg.scm manifest found in the current directory."))
      (let* ((results (command-results command))
             (top-results (command-global-results command))
             (top-flags (argument-results-flags top-results))
             (option (argument-results-options results))
             (tests-dir (option "directory"))
             (m (read-manifest "spkg.scm"))
             ;; include dev deps for tests
             (ops (manifest-install-dependencies m #t))
             (mpath (manifest-path m))
             (project-src-dir (string-append (dirname mpath) "/" (manifest-source-path m)))
             (files (collect-test-files tests-dir)))

        (when (top-flags "verbose")
          (log-level log-level:debug))

        (when (null? files)
          (info "Test" " No test files found under ~a" tests-dir)
          (exit 0))

        (info "Test" " Running ~a test file(s)" (length files))
        (for-each
          (lambda (f)
            (info "Test" " ~a" f)
            (let ((status (run-one-test ops project-src-dir f m)))
              (unless (zero? status)
                (raise-user-error (string-append "Test failed: " f)))))
          files)

        (info "Test" " All tests executed.")))

    (define spkg-test-command
      (command "test"
        'description: "Run all test files under ./tests."
        'grammar: grammar
        'run: run-test))))
