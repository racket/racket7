#lang racket/base
(require "../common/struct-star.rkt"
         "readtable-parameter.rkt")

(provide (struct*-out read-config)
         (struct-out read-config-state)
         make-read-config
         port+config->srcloc
         reading-at
         disable-wrapping)

(struct* read-config (readtable
                      for-syntax?   ; impose restrictions on graphs, fxvectors, etc?
                      source
                      wrap          ; wrapper applied to each datum, intended for syntax objects
                      read-compiled   ; for `#~`: input-port -> any/c
                      dynamic-require ; for reader extensions: module-path sym fail-k -> any
                      module-declared? ; for `#lang`: module-path -> any/c
                      coerce        ; coerce for syntax or not: any boolean -> any
                      * line
                      * col
                      * pos
                      * indentations  ; stack of `indentation` records
                      parameter-override ; mash of parameter -> value
                      parameter-cache   ; hash of parameter -> value
                      st)) ; other shared mutable state

(struct read-config-state ([accum-str #:mutable] ; string-buffer cache
                           [graph #:mutable]))   ; #f or hash of number -> value

(define (make-read-config
         #:source [source #f]
         #:for-syntax? [for-syntax? #f]
         #:readtable [readtable (current-readtable)]
         #:wrap [wrap #f #;(lambda (s-exp srcloc) s-exp)]
         #:read-compiled [read-compiled #f]
         #:dynamic-require [dynamic-require #f]
         #:module-declared? [module-declared? #f]
         #:coerce [coerce #f])
  (read-config readtable
               for-syntax?
               source
               wrap
               (or read-compiled
                   (lambda (in)
                     (error 'read "no `read-compiled` provided")))
               (or dynamic-require
                   (lambda (mod-path sym failure-k)
                     (error 'read "no `dynamic-require` provided")))
               (or module-declared?
                   (lambda (mod-path)
                     (error 'read "no `module-declare?` provided")))
               (or coerce
                   (lambda (for-syntax? v) v))
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
  (struct*-copy read-config config
                [line line]
                [col col]
                [pos pos]))

(define (disable-wrapping config)
  (struct*-copy read-config config
                [wrap #f]))
