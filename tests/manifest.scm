(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme file)
        (spkg core compat)
        (srfi 64)
        (spkg core manifest))


(define (write-temp path content)
  (call-with-output-file path
    (lambda (out)
      (display content out))))

(define (mkdir-p path)
  ;; good enough for tests
  (system (string-append "mkdir -p " path)))

(define (read-all path)
  (call-with-input-file path
    (lambda (in)
      (let loop ((acc '()))
        (let ((x (read in)))
          (if (eof-object? x)
              (reverse acc)
              (loop (cons x acc))))))))

(test-begin "manifest")


(define tmp "./tmp/.tmp-manifest-test.scm")
;; Create a fake package layout so `manifest-verify` can find library files.
(mkdir-p "./tmp/src/foo/bar")
(mkdir-p "./tmp/src/foo/bar/baz")
(write-temp "./tmp/src/foo.sld" "(define-library (foo) (export) (import (scheme base)) (begin))\n")
(write-temp "./tmp/src/foo/bar.sld" "(define-library (foo bar) (export) (import (scheme base)) (begin))\n")
(write-temp "./tmp/src/foo/bar/baz.sld" "(define-library (foo bar baz) (export) (import (scheme base)) (begin))\n")
(write-temp tmp
            (string-append
              "(package\n"
              "  (name (foo))\n"
              "  (libraries (foo) (foo bar) (foo bar baz))\n"
              "  (rnrs r7rs))\n\n"
              "(dependencies)\n"))
(define m (read-manifest tmp))
(define pkg (manifest-package m))
(test-assert "package-name should be list" (list? (package-name pkg)))
(test-equal "package-name should be (foo)" '(foo) (package-name pkg))
(test-equal "libraries" '((foo) (foo bar) (foo bar baz)) (package-libraries pkg))
(delete-file tmp)


(define tmp "./tmp/.tmp-manifest-test2.scm")
(mkdir-p "./tmp/src")
(write-temp "./tmp/src/foo.sld" "(define-library (foo) (export) (import (scheme base)) (begin))\n")
(write-temp tmp
            (string-append
              "(package\n"
              "  (name foo)\n"
              "  (rnrs r7rs))\n\n"
              "(dependencies)\n"))
(define m (read-manifest tmp))
(define pkg (manifest-package m))
(test-equal "symbol name" '(foo) (package-name pkg))
(delete-file tmp)


(define tmp "./tmp/.tmp-manifest-test3.scm")
(mkdir-p "./tmp/src")
(write-temp "./tmp/src/foo.sld" "(define-library (foo) (export) (import (scheme base)) (begin))\n")
(write-temp tmp
            (string-append
              "(package\n"
              "  (name (foo))\n"
              "  (rnrs r7rs))\n\n"
              "(dependencies)\n"
              "(dev-dependencies\n"
              "  (git (name (args)) (url \"https://example.invalid/args\")))\n"))
(define m (read-manifest tmp))
(test-assert "dev-dependencies is list" (list? (manifest-dev-dependencies m)))
(test-equal "dev-dependencies length" 1 (length (manifest-dev-dependencies m)))
(delete-file tmp)


(define tmp "./tmp/.tmp-manifest-test4.scm")
(mkdir-p "./tmp/src")
(write-temp "./tmp/src/foo.sld" "(define-library (foo) (export) (import (scheme base)) (begin))\n")
(write-temp tmp
            (string-append
              "(package\n"
              "  (name (foo))\n"
              "  (rnrs r7rs))\n\n"
              "(dependencies\n"
              "  (oci (name (bar)) (url \"ghcr.io/example/bar\") (rev \"1.0.0\")))\n"))
(define m (read-manifest tmp))
(test-assert "dependencies is list" (list? (manifest-dependencies m)))
(test-equal "dependencies length" 1 (length (manifest-dependencies m)))
(delete-file tmp)


;; Custom source-path should be respected by manifest-verify and accessible via manifest-source-path.
(define tmp "./tmp/.tmp-manifest-test-source-path.scm")
(mkdir-p "./tmp/mysrc")
(write-temp "./tmp/mysrc/foo.sld" "(define-library (foo) (export) (import (scheme base)) (begin))\n")
(write-temp tmp
            (string-append
              "(source-path \"mysrc\")\n"
              "(package\n"
              "  (name (foo))\n"
              "  (rnrs r7rs))\n\n"
              "(dependencies)\n"))
(define m (read-manifest tmp))
(test-equal "manifest-source-path" "mysrc" (manifest-source-path m))
(delete-file tmp)


(test-end "manifest")
