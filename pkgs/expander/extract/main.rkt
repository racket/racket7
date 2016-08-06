#lang racket/base
(require "link.rkt"
         "linklet-info.rkt"
         "linklet.rkt"
         "get-linklet.rkt"
         "needed.rkt"
         "export.rkt"
         "check-and-report.rkt"
         "flatten.rkt"
         "gc-defn.rkt"
         "decompile.rkt"
         "save-and-report.rkt")

(provide extract)

;; Gather all of the linklets need to run phase 0 of the specified
;; module while keeping the module's variables that are provided from
;; phase 0. In other words, keep enogh to produce any value or affect
;; that `dynamic-require` would produce.
(define (extract start-mod-path cache
                 #:print-extracted-to print-extracted-to
                 #:as-c? as-c?
                 #:as-decompiled? as-decompiled?)
  ;; Located modules:
  (define compiled-modules (make-hash))

  ;; All linklets that find we based on module `requires` from the
  ;; starting module
  (define seen (make-hash)) ; link -> linklet-info

  ;; The subset of `seen` that have that non-empty linklets
  (define linklets (make-hash)) ; link -> linklet-info
  ;; The same linklets are referenced this list, but kept in reverse
  ;; order of instantiation:
  (define linklets-in-order (box null))

  ;; Which linklets (as represented by a "link") are actually needed to run
  ;; the code, which includes anything referenced by the starting
  ;; point's exports and any imported linklet that has a side effect:
  (define needed (make-hash)) ; link -> value for reason

  ;; Use the host Racket's module name resolver to normalize the
  ;; starting module path:
  (define start-name
    (resolved-module-path-name
     (module-path-index-resolve
      (module-path-index-join start-mod-path #f))))

  ;; We always start at phase 0
  (define start-link (link start-name 0))
  
  ;; Start with the given link, and follow dependencies
  (get-linklets! start-link
                 #:cache cache
                 #:compiled-modules compiled-modules
                 #:seen seen
                 #:linklets linklets
                 #:linklets-in-order linklets-in-order)
  
  ;; Compute which linklets are actually used as imports
  (needed! start-link 'start
           #:seen seen
           #:needed needed)
  
  ;; We also want the starting name's re-exports:
  (for ([ex-lnk (in-list (linklet-info-re-exports (hash-ref seen start-link)))])
    (needed! ex-lnk `(re-export ,start-link)
             #:seen seen
             #:needed needed))

  ;; Anything that shows up in `codes` with a side effect also counts
  (for ([(lnk li) (in-hash linklets)])
    (when (linklet-info-side-effects? li)
      (needed! lnk 'side-effect
               #:seen seen
               #:needed needed)))
  
  ;; Check for bootstrap obstacles, and report what we've found
  (check-and-report! #:compiled-modules compiled-modules
                     #:linklets linklets
                     #:linklets-in-order linklets-in-order
                     #:needed needed)
  
  ;; If we're in source mode, we can generate a single linklet
  ;; that combines all the ones we found
  (when (linklets-are-source-mode? linklets)
    ;; Get variables to be exported by a flattened linklet; all of the
    ;; module provides must refer to instance variables
    (define exports
      (get-module-export-variables start-link
                                   #:compiled-modules compiled-modules
                                   #:cache cache))
    
    ;; Generate the flattened linklet
    (define flattened-linklet-expr
      (flatten! start-link
                #:linklets linklets
                #:linklets-in-order linklets-in-order
                #:needed needed
                #:exports exports))
    
    ;; Remove unreferenced definitions
    (define gced-linklet-expr
      (garbage-collect-definitions flattened-linklet-expr))

    (cond
     [as-decompiled?
      (compile-and-decompile gced-linklet-expr print-extracted-to)]
     [else
      (save-and-report-flattened! gced-linklet-expr print-extracted-to
                                  #:as-c? as-c?)])))
