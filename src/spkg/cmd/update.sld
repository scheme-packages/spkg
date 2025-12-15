;; `spkg update`: Update dependencies in the lockfile

(define-library (spkg cmd update)
  (import 
    (spkg core manifest)
    (spkg core manager)
    (spkg core log)
    (scheme base)
    (args grammar)
    (args runner))

  (export spkg-update-command)

  (begin 
    (define (run-update command)
      (define m (read-manifest "spkg.scm"))
      (manifest-update-dependencies m #t)
      (info "INFO" " Dependencies updated and lockfile completed (spkg.lock)."))
    

    (define spkg-update-command (command "update"
      'description: "Update dependencies in the lockfile."
      'run: run-update)))
)