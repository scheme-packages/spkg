(package 
  (name (spkg))
  (rnrs r7rs)
  (version "0.1.0")
  (libraries
    (spkg)))

(dependencies
  (system 
    (scheme base)
    (scheme write)
    (scheme file)
    (scheme read)
    (scheme process-context))

  (oci 
    (name (args))
    (url "ghcr.io/playx18/args")
    (rev "0.1.1")))
