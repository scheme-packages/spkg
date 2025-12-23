;;; run.sld -- spkg command to run a package's main script

(define-library (spkg cmd run)
  (import 
    (srfi 130)
    (spkg core manifest)
    (spkg core manager)
    (spkg core log)
  (spkg core errors)
    (spkg core compat)
    (spkg core dependency)
    (scheme base)
    (scheme file)
    (scheme write)
    (scheme process-context)
    (args grammar)
    (args runner)
    (args results))
  (export spkg-run-command)
  (begin
    
    (define grammar (make-grammar))

    (define (run-run command)
      (unless (file-exists? "spkg.scm")
        (raise-manifest-error "No spkg.scm manifest found in the current directory."))
      (let* ((results (command-results command))
             (top-results (command-global-results command))
             (top-flags (argument-results-flags top-results))
             (rest (argument-results-rest results))
             (m (read-manifest "spkg.scm"))
             (ops (manifest-install-dependencies m #t))
             (mpath (manifest-path m))
             (src-dir (string-append (dirname mpath) "/src"))
             (main-script (string-append src-dir "/main.scm")))
        (when (top-flags "verbose")
          (log-level log-level:debug))
        (unless (file-exists? (string-append (dirname mpath) "/src/main.scm"))
          (raise-manifest-error "Package has no 'src/main.scm' file, cannot run."))
        (info "INFO" " Running '~a'" main-script)
        (let ((cmd (string-append
                     (implementation->binary-name (current-implementation))
                     " "
                     (string-join (ops->runargs ops src-dir #f m) " ")
                     " "
                     (string-join (path->scriptarg main-script src-dir rest) " "))))
          (verboseln "DEBUG" " Executing command: '~a'" cmd)
          (system cmd))))

    (define spkg-run-command (command "run"
      'description: "Run the package's main script."
      'grammar: grammar
      'run: run-run))
    
))