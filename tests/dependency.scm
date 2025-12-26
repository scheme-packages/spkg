(import (scheme base)
        (scheme file)
        (scheme write)
        (srfi 64)
        (spkg core dependency)
        (spkg core compat))

(define (write-temp path content)
  (call-with-output-file path
    (lambda (out)
      (display content out))))

(define (mkdir-p path)
  ;; good enough for tests
  (system (string-append "mkdir -p " path)))

(test-begin "dependency")

;; Path dependency should read the dependency manifest's source-path.
(let* ((dep-root "./tmp/.tmp-dep-srcpath")
       (dep-src "mysrc")
       (manifest (string-append dep-root "/spkg.scm"))
       (libfile (string-append dep-root "/" dep-src "/dep.sld")))
  (when (file-exists? dep-root)
    (system (string-append "rm -rf " dep-root)))
  (mkdir-p (string-append dep-root "/" dep-src))
  (write-temp libfile "(define-library (dep) (export) (import (scheme base)) (begin))\n")
  (write-temp manifest
              (string-append
                "(source-path \"" dep-src "\")\n"
                "(package\n"
                "  (name (dep))\n"
                "  (rnrs r7rs))\n\n"
                "(dependencies)\n"))
  (let* ((dep (%path-dependency '(dep) dep-root #f))
         (ops (path-dependency-install dep)))
    (test-equal "runops includes custom source-path"
                (list (string-append dep-root "/" dep-src))
                (runops-append-path ops))))

(test-end "dependency")
