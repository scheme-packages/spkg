;; `spkg install`: Install program as a binary package in the specified directory or in `~/.spkg/bin`.

(define-library (spkg cmd install)
  (import 
    (srfi 130)
    (spkg core manifest)
    (spkg core manager)
    (spkg core log)
  (spkg core errors)
    (spkg core compat)
    (spkg core dependency)
    (spkg core util)
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

    (define (home->lib)
      (cond
        ((get-spkg-home)
         (string-append (get-spkg-home) "/lib"))
        (else #f)))

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

    ;; Copy a directory into SPKG_HOME/lib and return the new path.
    ;; We keep stable paths so installed binaries can refer to them.
    (define (install-lib-copy! src-path rel)
      (define lib-root (or (home->lib)
                           (raise-user-error "SPKG_HOME not set and HOME unavailable; cannot determine library directory.")))
      (define dst (string-append (canonicalize-path-string lib-root) "/" rel))
      (system (string-append "mkdir -p " lib-root))
      (system (string-append "rm -rf " dst))
      (system (string-append "mkdir -p " (dirname dst)))
      ;; cp -R preserves directory structure.
      (system (string-append "cp -R " src-path " " dst))
      dst)

    (define (string-prefix? prefix s)
      (and (<= (string-length prefix) (string-length s))
           (string=? prefix (substring s 0 (string-length prefix)))))

    ;; Map dependency cache paths into SPKG_HOME/lib so launchers don't depend on cache.
    (define (relocate-runops-to-home-lib! ops)
      (define (relocate-path p)
        ;; Dependency install paths are typically:
        ;;   <cache-dir>/git/src/<name>-<checksum>/src
        ;;   <cache-dir>/oci/src/<name>-<checksum>/src
        ;; We mirror the subtree under: $SPKG_HOME/lib/dependencies/...
        (let* ((p* (canonicalize-path-string p))
               (cache* (canonicalize-path-string cache-dir))
               (prefix (string-append cache* "/")))
          (if (string-prefix? prefix p*)
              (let* ((rel (substring p* (string-length prefix) (string-length p*)))
                     (dst-rel (string-append "dependencies/" rel))
                     (dst (install-lib-copy! p* dst-rel)))
                dst)
              ;; Non-cache path deps (local paths) remain as-is.
              p)))

      (runops
        (map relocate-path (runops-append-path ops))
        (map relocate-path (runops-prepend-path ops))
        (runops-recompile? ops)))
    
    (define (run-install command)
      (let* ((option (argument-results-options (command-results command)))
             (dir (or (option "directory")
                      (raise-user-error "Installation directory not specified.")))
             (m (or (and (file-exists? "spkg.scm") (read-manifest "spkg.scm"))
                    (raise-manifest-error "No spkg.scm manifest found in the current directory.")))
             (ops (manifest-install-dependencies m #t))
             (mpath (manifest-path m))
             (_main (or
                      (and (file-exists? (string-append (dirname mpath) "/src/main.scm"))
                           (string-append (dirname mpath) "/src/main.scm"))
                      (raise-manifest-error "Package has no 'src/main.scm' file, cannot install binary.")))
             (raw-bin-name (or (option "name")
                               (package-name (manifest-package m))))
             (bin-name (if (list? raw-bin-name)
                           ;; Package names are now lists like (foo bar). Default binary name uses root.
                           (name->root raw-bin-name)
                           raw-bin-name))
             (bin-path (string-append (canonicalize-path-string dir) "/" (symbol->string bin-name)))
       ;; 1) Copy sources into `$SPKG_HOME/lib/bin/<name>/src`
       (install-root (string-append (or (home->lib)
                       (raise-user-error "SPKG_HOME not set and HOME unavailable; cannot determine library directory."))
                     "/bin/" (symbol->string bin-name)))
       (install-src-dir (string-append install-root "/src"))
       ;; 2) Copy dependency sources into `$SPKG_HOME/lib/dependencies/...`
       (installed-ops (relocate-runops-to-home-lib! ops)))

        (system (string-append "mkdir -p " dir))

        (info "INFO" " Installing binary '~a' to '~a'..." bin-name bin-path)
        (system (string-append "rm -rf " install-src-dir "/*"))
        (system (string-append "mkdir -p " install-src-dir))

        (system (string-append "cp -r src " install-root))
        ;; 3) Create launcher script
        (call-with-output-file bin-path
          (lambda (out)
            (write-string "#!/bin/sh" out)
            (newline out)
            (write-string
              (string-append
                (string-join
                  (append 
                    (append
                      (list (implementation->binary-name (current-implementation)))
                      (ops->runargs installed-ops install-src-dir #t m)
                      )
                    (path->scriptarg
                        (string-append install-src-dir "/main.scm")
                        install-src-dir
                        '("$@")))
                  " ")
                )
              out)
            (newline out)
            (flush-output-port out)))
        (system (string-append "chmod +x " bin-path))
        (info "INFO" " Binary '~a' installed successfully." bin-name)))
      
  (define spkg-install-command (command "install"
      'description: "Install the package as a binary."
      'grammar: grammar
      'run: run-install))
))