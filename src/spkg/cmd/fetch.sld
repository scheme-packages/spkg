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
    (define (run-fetch command)
      (define m (read-manifest "spkg.scm"))
      (define ops (manifest-install-dependencies m #t))
    
      (info "INFO" " All dependencies fetched and installed."))
    (define spkg-fetch-command (command "fetch"
      'description: "Fetch and install all dependencies."
      'run: run-fetch))))