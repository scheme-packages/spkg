;; `spkg new`: Create a new spkg package template in the specified directory.

(define-library (spkg cmd new)
  (import 
    (spkg core compat)
    (spkg core log)
    (spkg core errors)
    (scheme base)
    (scheme read)
    (scheme file)
    (scheme write)
    (args results)
    (args grammar)  
    (args runner))
  (export spkg-new-command)


  (begin 
    (define (make-manifest name is-lib?)
      (string-append 
        "(package\n"
        "  (name (" name "))\n"
        (if is-lib?
            (string-append "  (libraries (" name "))\n")
            "")
        "  (version \"0.1.0\")\n"
        "  (description \"A new Scheme package.\")\n"
        "  (rnrs r7rs))\n\n"
        "(dependencies)\n"))
    
    (define (make-main)
      (string-append 
        "(import (scheme base) (scheme r5rs))\n\n"
        "(define (main)\n"
        "  (display \"Hello, World!\\n\"))\n\n"
        "(main)\n"))
    
    (define (make-lib name)
      (string-append 
        "(define-library " name "\n"
        "  (export add1)\n"
        "  (import (scheme base))\n\n"
        "  (begin\n"
        "    (define (add1 x)\n"
        "      (+ x 1))\n"
        "  ))\n"))

    (define grammar (make-grammar))
    (grammar-add-flag! grammar "lib"
      'help: "Create a library package instead of an application."
      'hide-negated-usage?: #t)
    
    (define (string->sexpr str)
      (define port (open-input-string str))
      (define sexpr (read port))
      (close-input-port port)
      sexpr)

    (define (run-new command)
      (define results (command-results command))
      (define rest (argument-results-rest results))
      (define flag (argument-results-flags results))
      (when (null? rest)
        (raise-user-error "Please specify a directory to create the new package in."))
      (define target-dir (string->sexpr (car rest)))
      (unless (symbol? target-dir)
        (raise-user-error "Directory name must be a symbol." target-dir))

      (define target (symbol->string target-dir))
      (when (file-exists? target)
        (raise-user-error (string-append "Target directory '" target "' already exists.")))
      (create-directory target)
      (define manifest-path (string-append target "/spkg.scm"))
      (define main-path (string-append target "/src/main.scm"))
      (define lib-path (string-append target "/src/" (symbol->string target-dir) ".sld"))
      (create-directory (string-append target "/src"))
      (call-with-output-file manifest-path
        (lambda (out)
          (write-string (make-manifest (symbol->string target-dir) (flag "lib")) out)))
      (call-with-output-file lib-path 
        (lambda (out)
          (write-string (make-lib (symbol->string target-dir)) out)))
      
      (unless (flag "lib")
        (call-with-output-file main-path
          (lambda (out)
            (write-string (make-main) out)))))
    (define spkg-new-command (command "new" 
      'description: "Create a new spkg package template in the specified directory."
      'grammar: grammar
      'run: run-new))))