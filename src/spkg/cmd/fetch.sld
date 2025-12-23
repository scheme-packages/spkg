(define-library (spkg cmd fetch)
  (import 
    (spkg core manifest)
    (spkg core manager)
    (spkg core log)
    (spkg core dependency)
    (scheme base)
    (args grammar)
    (args runner))
  (export spkg-fetch-command)
  (begin
    (define grammar (make-grammar))
    (define (run-fetch command)
      (unless (file-exists? "spkg.scm")
        (raise-manifest-error "No spkg.scm manifest found in the current directory."))
      (define results (command-results command))
      (define option (argument-results-options results))
      
      (define m (read-manifest "spkg.scm"))
      (define ops (manifest-install-dependencies m #t))

      ;; Note: installs are lockfile-authoritative. If a transitive dependency
      ;; is missing from the lockfile, the user should run `spkg update` to
      ;; populate it deterministically.
      (info "INFO" " All dependencies fetched and installed (lockfile-authoritative)."))
    (define spkg-fetch-command (command "fetch"
      'description: "Fetch and install all dependencies."
      'grammar: grammar
      'run: run-fetch))))