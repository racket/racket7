#lang racket/base
(require "readtable-parameter.rkt")

(provide (struct-out read-config)
         (struct-out read-config-state)
         make-read-config
         port+config->srcloc
         reading-at)

(struct read-config (readtable
                     source
                     wrap          ; wrapper applied to each datum, intended for syntax objects
                     for-syntax?   ; impose restrictions on graphs, fxvectors, etc?
                     line
                     col
                     pos
                     indentations  ; stack of `indentation` records
                     parameter-override ; mash of parameter -> value
                     parameter-cache   ; hash of parameter -> value
                     st)) ; other shared mutable state

(struct read-config-state ([accum-str #:mutable] ; string-buffer cache
                           [graph #:mutable]))   ; #f or hash of number -> value

(define (make-read-config
         #:source [source #f]
         #:readtable [readtable (current-readtable)]
         #:wrap [wrap #f #;(lambda (s-exp srcloc) s-exp)]
         #:for-syntax? [for-syntax? #f])
  (read-config readtable
               source
               wrap
               for-syntax?
               #f ; line
               #f ; col
               #f ; pos
               null ; indentations
               #hasheq()     ; parameter-override
               (make-hasheq) ; parameter-cache
               (read-config-state #f    ; accum-str
                                  #f))) ; graph

(define (port+config->srcloc in config)
  (define-values (end-line end-col end-pos) (port-next-location in))
  (srcloc (read-config-source config)
          (read-config-line config)
          (read-config-col config)
          (read-config-pos config)
          (and (read-config-pos config) end-pos (- end-pos (read-config-pos config)))))

(define (reading-at config line col pos)
  (struct-copy read-config config
               [line line]
               [col col]
               [pos pos]))
