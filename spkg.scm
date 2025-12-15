(package 
  (name spkg)
  (rnrs r7rs))

(dependencies
  (git 
    (name args)
    (url "https://github.com/playx18/scm-args")
    (subpath "src/")))