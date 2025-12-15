(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme read)
        (srfi 1)
        (srfi 64)
        (spkg core lockfile))

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

(define (count-top-level-git-entries expr name-form)
  (define (git-entry? x)
    (and (pair? x)
         (symbol? (car x))
         (eq? (car x) 'git)
         (let ((name-pair (assoc 'name (cdr x))))
           (and name-pair (equal? (cadr name-pair) name-form)))))
  (let loop ((rest (cdr expr)) (n 0))
    (cond
      ((null? rest) n)
      ((git-entry? (car rest)) (loop (cdr rest) (+ n 1)))
      (else (loop (cdr rest) n)))))


(test-begin "lockfile")


(define lock-in "./spkg.lock")
(define lock-out "./.tmp-lockfile-test.lock")
(when (file-exists? lock-out) (delete-file lock-out))

(test-assert "repo spkg.lock exists" (file-exists? lock-in))

(define lock (load-lockfile lock-in))
(lockfile-root-checksum-set! lock (lockfile-root-checksum lock))

;; write to a temp path
(define tmp-lock (load-lockfile lock-out))
(lockfile-root-checksum-set! tmp-lock (lockfile-root-checksum lock))
(for-each (lambda (p) (lockfile-set-entry! tmp-lock (cdr p)))
          (lockfile-entries lock))
(save-lockfile! tmp-lock)

(define expr (car (read-all lock-out)))
(test-assert "should be lockfile" (and (pair? expr) (eq? (car expr) 'lockfile)))
(test-equal "0 git entries named args (symbol form)" 0
            (count-top-level-git-entries expr 'args))
(test-equal "1 git entry named (args) (list form)" 1
            (count-top-level-git-entries expr '(args)))

(delete-file lock-out)


(define lock-out "./.tmp-lockfile-sort-test.lock")
(when (file-exists? lock-out) (delete-file lock-out))

(define lock (load-lockfile lock-out))
;; Intentionally add out of order.
(lockfile-set-entry! lock (make-lock-entry '(zeta) 'git "https://example.invalid/z" #f "c" #f "z" #f))
(lockfile-set-entry! lock (make-lock-entry '(alpha) 'git "https://example.invalid/a" #f "b" #f "a" #f))
(lockfile-set-entry! lock (make-lock-entry '(beta) 'git "https://example.invalid/b" #f "a" #f "b" #f))
(save-lockfile! lock)

(define expr (car (read-all lock-out)))
(define entries
  (filter (lambda (x) (and (pair? x) (memq (car x) '(git path))))
          (cdr expr)))
(define names
  (map (lambda (entry)
          (let ((p (assoc 'name (cdr entry))))
            (cadr p)))
        entries))

(test-equal "names sorted" '((alpha) (beta) (zeta)) names)

(delete-file lock-out)

(test-end "lockfile")
