(package 
  (name (spkg))
  (rnrs r7rs)
  (libraries
    (spkg)))

(dependencies
  (git 
    (name (args))
    (url "https://github.com/playx18/scm-args")))