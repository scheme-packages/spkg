(package 
  (name (spkg))
  (rnrs r7rs)
  (version "0.1.0")
  (libraries
    (spkg)))

(dependencies
  (git
    (name (args))
    (url "https://github.com/playx18/scm-args")))
