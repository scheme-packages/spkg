(define-library (spkg core manager)
  (import 
    (spkg core manifest)
    (spkg core dependency)
    (spkg core log)
    (spkg core lockfile)
    (spkg core errors)
    (spkg core util)
    (spkg core compat)
    (scheme base)
    (scheme file)
    (srfi 1)
    (scheme char)
    (srfi 130)
    (scheme write)
    (scheme process-context))
  (export 
    manifest-install-dependencies
    manifest-needs-recompile?
    manifest-update-dependencies
    implementation->binary-name
    current-implementation
    ops->runargs
    path->scriptarg)
  (include "manager.scm"))
