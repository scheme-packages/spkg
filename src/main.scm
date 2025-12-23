(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (spkg core manifest)
        (spkg core manager)
        (spkg core dependency)
        (args grammar)
        (args runner)
        (scheme process-context)
        (spkg core log)
        (spkg core errors)
        (spkg core compat)
        (spkg cmd update)
        (spkg cmd fetch)
        (spkg cmd install)
        (spkg cmd new)
  (spkg cmd run)
        (spkg cmd test)
        (spkg cmd publish))
(define runner (make-command-runner "spkg" "Scheme Package Manager & Build System"))

(define grammar (command-runner-grammar runner))

(grammar-add-flag! grammar "verbose"
  'abbr: "v"
  'help: "Enable verbose logging."
  'hide-negated-usage?: #t)



(command-runner-add-command! runner spkg-update-command)
(command-runner-add-command! runner spkg-fetch-command)
(command-runner-add-command! runner spkg-install-command)
(command-runner-add-command! runner spkg-new-command)
(command-runner-add-command! runner spkg-run-command)
(command-runner-add-command! runner spkg-test-command)
(command-runner-add-command! runner spkg-publish-command)

(define (main args)
  (command-runner-run runner args))
(main (cdr (command-line)))
