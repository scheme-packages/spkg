;; `spkg publish`: Package the current project and push it as an OCI artifact.

(define-library (spkg cmd publish)
  (import
    (scheme base)
    (scheme file)
    (scheme write)
    (scheme process-context)
    (srfi 130)
    (args grammar)
    (args runner)
    (args results)
    (spkg core manifest)
    (spkg core log)
    (spkg core errors)
    (spkg core util)
    (spkg core compat))
  (export spkg-publish-command)

  (begin
    (define grammar (make-grammar))

    (grammar-add-option! grammar "ref"
      'help: "OCI reference to push to (e.g. ghcr.io/org/pkg:1.2.3). If no tag is provided, defaults to the package version."
      'value-help: "REF")

    

    (define (run-publish command)
      (unless (file-exists? "spkg.scm")
        (raise-manifest-error "No spkg.scm manifest found in the current directory."))
      (let* ((option (argument-results-options (command-results command)))
             (ref (option "ref")))
        (unless (and ref (string? ref) (not (string=? ref "")))
          (raise-user-error "Missing required option --ref"))

        (let* ((m (read-manifest "spkg.scm"))
               (pkg (manifest-package m))
               (root (manifest-root-directory m))
               (final-ref
                (begin
                  (unless (package-version pkg)
                    (raise-user-error
                      "No tag found in ref and package has no version; cannot determine final ref."
                      ref))
                  (string-append ref ":" (package-version pkg))))
               (tmp (capture-required-line "mktemp -d /tmp/spkg.publish.XXXXXX")))
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (let ((archive (string-append tmp "/spkg.tar.gz")))

                ;; Create archive of package root. Exclude lockfile and VCS dir.
                (system*
                  (string-append
                    "tar -czf " (shell-quote archive)
                    " --exclude=" (shell-quote "./.git")
                    " --exclude=" (shell-quote "./spkg.lock")
                    " -C " (shell-quote root)
                    " ."))

                (info "Publish" " Pushing ~a ..." final-ref)

                (system*
                  (string-append
                    "cd " (shell-quote tmp)
                    " && oras push " (shell-quote final-ref)
                    " " (shell-quote "spkg.tar.gz") ":application/vnd.spkg.package.v1.tar+gzip"
                    " --artifact-type application/vnd.spkg.package.v1"
                    " --disable-path-validation"))
                (info "Publish" " Done: ~a" final-ref)))
            (lambda ()
              (delete-tree tmp))))))

    (define spkg-publish-command
      (command "publish"
        'description: "Publish the current package as an OCI artifact (via oras)."
        'grammar: grammar
        'run: run-publish))))