
(define futures-table
  (make-primitive-table
   futures-enabled?
   processor-count
   future
   future?
   touch
   would-be-future
   current-future
   make-fsemaphore
   fsemaphore?
   fsemaphore-post
   fsemaphore-wait
   fsemaphore-try-wait?
   fsemaphore-count
   reset-future-logs-for-tracing!
   mark-future-trace-end!))
