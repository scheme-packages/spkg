(import (scheme base)
        (srfi 64)
        (spkg core errors))


(test-begin "errors")


(define e (make-spkg-error 'manifest "bad manifest" '(details foo)))
(test-assert "spkg-error?" (spkg-error? e))
;; Should not raise.
(test-assert "print-condition-to-errlog doesn't raise" (begin (print-condition-to-errlog e) #t))
(test-end "errors")