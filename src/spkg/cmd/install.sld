;; `spkg install`: Install program as a binary package in the specified directory or in `~/.spkg/bin`.

(define-library (spkg cmd install)
  (import 
    (srfi 130)
    (spkg core manifest)
    (spkg core manager)
    (spkg core log)
    (spkg core compat)
    (spkg core dependency)
    (scheme base)
    (scheme file)
    (scheme write)
    (scheme process-context)
    (args grammar)
    (args runner)
    (args results))
  (export spkg-install-command)

  (begin 

    (define (get-spkg-home)
      (or (get-environment-variable "SPKG_HOME")
          (string-append (get-environment-variable "HOME") "/.spkg")))

    (define (home->bin)
      (cond 
        ((get-spkg-home)
         (string-append (get-spkg-home) "/bin"))
        (else #f)))

    (define grammar (make-grammar))
    (grammar-add-option! grammar "directory"
      'help: "Directory to install the binary into."
      'value-help: "DIR"
      'defaults-to: (home->bin))
    (grammar-add-option! grammar "name"
      'help: "Name of the binary to install."
      'value-help: "NAME")
    
    (define (run-install command)
      (define option (argument-results-options (command-results command)))
      (define dir (option "directory"))

      (unless dir 
        (error "Installation directory not specified or could not be determined."))
      
      (define m (read-manifest "spkg.scm"))
      (define ops (manifest-install-dependencies m #t))
      (define mpath (manifest-path m))
   
      (unless (file-exists? (string-append (dirname mpath) "/src/main.scm"))
        (error "Package has no 'src/main.scm' file, cannot install binary."))
     
      (define main (string-append mpath "/src/main.scm"))
      (system (string-append "mkdir -p " dir))
      (define bin-name 
        (or (option "name")
            (package-name (manifest-package m))))
      (when (list? bin-name)
        (error "Package is a nested name, please specify a single symbol for the binary name using '--name' option."))
      (define bin-path (string-append (canonicalize-path-string dir) "/" (symbol->string bin-name)))
     
      (info "INFO" " Installing binary '~a' to '~a'..." bin-name bin-path)
      ;; 1) Copy sources into `<cache-dir>/install/<name>/src`
      (define install-src-dir (string-append cache-dir "/install/" (symbol->string bin-name) "/src"))
      (system (string-append "rm -rf " install-src-dir "/*"))
      (system (string-append "mkdir -p " install-src-dir))
      
      (system (string-append "cp -r src " (string-append cache-dir "/install/" (symbol->string bin-name))))
      ;; 2) Create launcher script
      (call-with-output-file bin-path 
        (lambda (out)
          (write-string "#!/bin/sh" out)
          (newline out)
          (write-string
            (string-append 
              (implementation->binary-name (current-implementation)) 
              " " 
              (string-join (ops->runargs ops install-src-dir) " ") 
              " "
              (string-join (path->scriptarg (string-append install-src-dir "/main.scm") install-src-dir) " ")
              " -- $@")
            out)
          (newline out)
          (flush-output-port out)))
      (system (string-append "chmod +x " bin-path))
      (info "INFO" " Binary '~a' installed successfully." bin-name))
      
  (define spkg-install-command (command "install"
      'description: "Install the package as a binary."
      'grammar: grammar
      'run: run-install))
))