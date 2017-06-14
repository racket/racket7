(meta-cond
 [(threaded?)
  (define internal-make-thread-parameter make-thread-parameter)]
 [else
  (define internal-make-thread-parameter chez:make-parameter)])
