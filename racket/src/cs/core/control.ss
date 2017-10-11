;; The full continuation is a chain of metacontinuations. Each
;; metacontinuation contains a host Scheme continuation, and
;; every prompt is on a boundary between metacontinuations. When
;; a composable continuation is applied, the composition boundary
;; is also a metacontinuation boundary.

;; "Continuation" as exported from the core is "metacontinuation"
;; here. So, `call-with-current-continuation` defined here and
;; exported captures the current metacontinuation (up to a prompt).
;; The `call/cc` function is the host's notion of continuation, which
;; corresponds to a single metacontinuation frame.

;; A picture where the continuation grows down:

;;                   [root empty continuation]
;;                    --- empty-k
;; metacontinuation  |
;;     frame         |
;;                   |--- resume-k & resume/no-wind
;;                   |<-- tag represents this point
;;                    --- empty-k
;; metacontinuation  |
;;     frame         |
;;                   |
;;                   |--- resume-k & resume/no-wind
;;                   |<-- tag represents this point
;;                    --- empty-k
;;   current host    |
;;   continuation    |
;;                   v

;; Concretely, the metacontinuation is the current host continuation
;; plus the frames in the list `(current-metacontinuation)`, where the
;; shallowest (= lowest in the picture above) frame is first in the
;; list. The `empty-k` value of the current host continuation is
;; in `current-empty-k`.

;; The host Scheme implementation takes care of winders from
;; `dynamic-wind`, which means that things generally work if host
;; functions uses `dynamic-wind` (e.g., for `with-input-from-file`).
;; We assume that no host Scheme winders end up using continuations
;; (or calling client-provided code that can use continuations), so
;; that cross-frame jumps at the metacontinuation level are not
;; triggered by the host Scheme.

;; The continuation within a metacontinuation frame is kept in two
;; forms: one that has the frame's winders (from `dynamic-wind`)
;; attached, and one that doesn't. The one without winders is used to
;; reinstantiate the frame's continuation as the host continuation
;; when control returns to that frame. The one with winders is used to
;; compose a captured frame onto the current continuation. See
;; `call/cc-no-winders` for the way Chez internals are used to get a
;; continuation disconnected from winders. See also the use of
;; `#%$current-stack-link` to disconnect a fresh metacontinuation
;; frame's continuation from the formerly current continuation.

;; The shallowest metacontinuation frame's `empty-k` continuation is
;; used to detect when the current host continuation is empty (i.e.,
;; when it matches the `current-empty-k` value). When it's empty, then
;; calling a composable continuation doesn't need to add a new
;; metacontinuation frame, and the application gets the right "tail"
;; behavior.

;; A metacontinuation frame's `resume-k/no-wind` is called when
;; control returns or needs to escape through the frame:
;;
;; * When returning normally to a metacontinuation frame, the
;;   `resume-k/no-wind` continuation receives the values returned to the
;;   frame.
;;
;; * When aborting to a prompt tag, the `resume-k/no-wind`
;;   continination receives a special value that indicates an abort to a
;;   specific tag, and the frames will jump to the next metacontinuation
;;   frame (running the current frame's "out" winders) until a frame
;;   with the right tag is hit.
;;
;; * Calling a non-composable continuation is similar to aborting,
;;   except that the target prompt's abort handler is not called.
;;   In fact, the metacontinuation-frame unwinding process stops
;;   before the frame with the target prompt tag (since that prompt
;;   is meant to be preserved).
;;
;; * When composing a metacontinuation frame onto the current
;;   metacontinuation, `resume-k` is called instead of
;;   `resume-k/no-wind` so that the frame's "in" winders get run.

;; The continuation marks for the frame represented by the current
;; host continuation are kept in `current-mark-stack`. When a
;; metacontinuation frame is created, it takes the current
;; `current-mark-stack` value and `current-mark-stack` is set back to
;; empty. To keep winders and the mark stack in sync, `dynamic-wind`
;; is wrapped to reset the mark stack on entry to a pre or post thunk.

;; A metacontinuation frame has an extra cache slot to contain a list
;; of mark-stack lists down to the root continuation. When a delimited
;; sequence of metacontinuation frames are copied out of or into the
;; metacontinuation, the slot is flushed and will be reset on demand.

;; We're assuming for now that no handlers are installed during a
;; nested metacontinuation.

;; Continuations are used to implement engines, but it's important
;; that an engine doesn't get swapped out (or, more generally,
;; asynchronous signals are handled at the Racket level) while we're
;; manipulating the continuation representation. A bad time for a swap
;; is an "interrupted" region. The `begin-uninterrupted` and
;; `end-uninterrupted` functions bracket such regions dynamically. See
;; also "core-engine.ss" and "core-interrupt.ss"

(define current-metacontinuation (internal-make-thread-parameter '()))

(define current-empty-k (internal-make-thread-parameter #f))

;; The value of `current-cc-guard` is a callback installed by the
;; application of a non-composable continuation with an impersonated
;; prompt tag.
(define current-cc-guard (internal-make-thread-parameter values))

(define-record metacontinuation-frame (tag          ; continuation prompt tag or #f
                                       resume-k     ; delivers values to the prompt
                                       resume-k/no-wind ; same, but doesn't run winders jumping in
                                       empty-k      ; deepest end of this frame
                                       mark-stack   ; mark stack to restore
                                       mark-chain   ; #f or a cached list of mark-chain-frame or elem+cache
                                       traces       ; #f or a cached list of traces
                                       cc-guard))   ; cc-guard to restore

;; Messages to `resume-k[/no-wind]`:
(define-record appending (resume))  ; composing the frame, so run "in" winders
(define-record aborting (tag args wind?)) ; aborting, so run "out" winders --- unless not `wind?`
(define-record applying (c args))   ; applying a non-composable continuation

(define-record-type (continuation-prompt-tag create-continuation-prompt-tag authentic-continuation-prompt-tag?)
  (fields (mutable name))) ; mutable => constructor generates fresh instances

(define the-default-continuation-prompt-tag (create-continuation-prompt-tag 'default))

;; Not actually set, but allows access to the full continuation:
(define the-root-continuation-prompt-tag (create-continuation-prompt-tag 'root))

;; Detected to prevent some jumps:
(define the-barrier-prompt-tag (create-continuation-prompt-tag 'barrier))

(define/who make-continuation-prompt-tag
  (case-lambda
    [() (create-continuation-prompt-tag #f)]
    [(name)
     (check who symbol? name)
     (create-continuation-prompt-tag name)]))

(define (default-continuation-prompt-tag) the-default-continuation-prompt-tag)
(define (root-continuation-prompt-tag) the-root-continuation-prompt-tag)

;; To support special treatment of break parameterizations, and also
;; to initialize disabled breaks for `dynamic-wind` pre and post
;; thunks:
(define break-enabled-key (gensym 'break-enabled))

;; FIXME: add caching to avoid full traversal
(define/who (continuation-prompt-available? tag)
  (check who continuation-prompt-tag? tag)
  (let ([tag (strip-impersonator tag)])
    (or (eq? tag the-default-continuation-prompt-tag)
        (eq? tag the-root-continuation-prompt-tag)
        (let loop ([mc (current-metacontinuation)])
          (cond
           [(null? mc)
            #f]
           [(eq? tag (metacontinuation-frame-tag (car mc)))
            #t]
           [else (loop (cdr mc))])))))

(define/who (maybe-future-barricade tag)
  (when (future? (current-future)) ;; running in a future
    (check who continuation-prompt-tag? tag)
    (let ([fp (strip-impersonator (current-future-prompt))]
          [tag (strip-impersonator tag)])
      (cond
       [(eq? tag the-root-continuation-prompt-tag)
        (block)]
       [else
        (let loop ([mc (current-metacontinuation)])
          (cond
           [(null? mc) ;; I don't think this should ever happen.
            (block)] ;; not sure
           [(eq? tag (metacontinuation-frame-tag (car mc))) ;; found tag
            (void)]
           [(eq? (metacontinuation-frame-tag (car mc)) fp) ;; tag must be above future prompt.
            (block)]
           [else
            (loop (cdr mc))]))]))))

;; Should this need to ever block? Does it use the "current-continuation"? doesnt seem to.
(define/who call-with-continuation-prompt
  (case-lambda
    [(proc) (call-with-continuation-prompt proc the-default-continuation-prompt-tag #f)]
    [(proc tag) (call-with-continuation-prompt proc tag #f)]
    [(proc tag handler . args)
     (check who procedure? proc)
     (check who continuation-prompt-tag? tag)
     (check who :or-false procedure? handler)
     (start-uninterrupted 'prompt)
     (call-in-empty-metacontinuation-frame
      (strip-impersonator tag)
      (wrap-handler-for-impersonator
       tag
       (or handler (make-default-abort-handler tag)))
      (lambda ()
        (end-uninterrupted 'prompt)
        (compose-cc-guard-for-impersonator! tag)
        (call-with-values (lambda () (apply proc args))
          (lambda results
            (apply (current-cc-guard) results)))))]))

(define (make-default-abort-handler tag)
  (lambda (abort-thunk)
    (check 'default-continuation-prompt-handler (procedure-arity-includes/c 0) abort-thunk)
    (call-with-continuation-prompt abort-thunk tag #f)))

(define (resume-metacontinuation results)
  ;; pop a metacontinuation frame
  (cond
   [(null? (current-metacontinuation)) (engine-return)]
   [else
    (start-uninterrupted 'resume-mc)
    (let ([mf (car (current-metacontinuation))])
      (pop-metacontinuation-frame)
      ;; resume
      ((metacontinuation-frame-resume-k/no-wind mf) results))]))

(define (pop-metacontinuation-frame)
  (let ([mf (car (current-metacontinuation))])
    (current-metacontinuation (cdr (current-metacontinuation)))
    (current-mark-stack (metacontinuation-frame-mark-stack mf))
    (current-empty-k (metacontinuation-frame-empty-k mf))))

(define (call-in-empty-metacontinuation-frame tag handler proc)
  ;; Call `proc` in an empty metacontinuation frame, reifying the
  ;; current metacontinuation as needed (i.e., if non-empty) as a new
  ;; frame on `*metacontinuations*`; if the tag is #f and the
  ;; current metacontinuation frame is already empty, don't push more
  (assert-in-uninterrupted)
  (assert-not-in-system-wind)
  (call/cc
   (lambda (k)
     (cond
      [(and (not tag)
            (pair? (current-metacontinuation))
            (let ([current-mf (car (current-metacontinuation))])
              (and (eq? tag (metacontinuation-frame-tag current-mf))
                   (eq? k (current-empty-k))
                   current-mf)))
       =>
       (lambda (current-mf)
         ;; empty continuation in the current frame; don't push a
         ;; new metacontinuation frame --- and, in fact, keep the
         ;; current one if metadata hasn't changed; we assume that
         ;; there are no new winders and the handler is the same,
         ;; otherwise the continuation would be bigger
         (when (current-mark-stack)
           ;; update metacontinuation for new mark-stack elements:
           (current-metacontinuation
            (cons (metacontinuation-frame-merge current-mf (current-mark-stack))
                  (cdr (current-metacontinuation)))))
         (proc))]
      [else
       (let ([r ; a list of results, or a non-list for special handling
              (call/cc
               (lambda (k)
                 (call/cc-no-winders
                  ;; Not necessarily called in tail position, but that's ok:
                  (lambda (k/no-wind)
                    ;; At this point, the winders list is empty.
                    ;; Push another continuation frame so we can drop its `next`
                    (call-as-non-tail
                     (lambda ()
                       ;; drop the rest of the current continuation from the
                       ;; new metacontinuation frame:
                       (#%$current-stack-link #%$null-continuation)
                       (let-values ([results
                                     (call/cc
                                      ;; remember the "empty" continuation frame
                                      ;; that just continues the metacontinuation:
                                      (lambda (empty-k)
                                        (let ([mf (make-metacontinuation-frame tag
                                                                               k
                                                                               k/no-wind
                                                                               (current-empty-k)
                                                                               (current-mark-stack)
                                                                               #f
                                                                               #f
                                                                               (current-cc-guard))])
                                          (current-empty-k empty-k)
                                          (current-mark-stack #f)
                                          (current-cc-guard values)
                                          ;; push the metacontinuation:
                                          (current-metacontinuation (cons mf (current-metacontinuation)))
                                          ;; ready:
                                          (proc))))])
                         ;; continue normally; the metacontinuation could be different
                         ;; than when we captured this metafunction frame, though:
                         (resume-metacontinuation results))))))))])
         (cond
          [(or (null? r) (pair? r))
           ;; We're returning normally; the metacontinuation frame has
           ;; been popped already by `resume-metacontinuation`
           (end-uninterrupted 'resume)
           (if (and (pair? r) (null? (cdr r)))
               (car r)
               (apply values r))]
          [(appending? r)
           ;; We applied this metacontinuation frame just to run its "in" winders
           ((appending-resume r))]
          [(aborting? r)
           ;; We're aborting to a given tag
           (cond
            [(eq? tag (aborting-tag r))
             ;; Found the right tag. Remove the prompt as we call the handler:
             (pop-metacontinuation-frame)
             (end-uninterrupted 'handle)
             (apply handler
                    (aborting-args r))]
            [else
             ;; Aborting to an enclosing prompt, so keep going:
             (pop-metacontinuation-frame)
             (do-abort-current-continuation (aborting-tag r)
                                            (aborting-args r)
                                            (aborting-wind? r))])]
          [(applying? r)
           ;; We're applying a non-composable continuation --- past
           ;; this prompt, or else we would have stopped.
           ;; Continue escaping to an enclosing prompt:
           (pop-metacontinuation-frame)
           (apply-continuation (applying-c r)
                               (applying-args r))]))]))))

(define (call/cc-no-winders proc)
  (let ([prev-winders (#%$current-winders)])
    (cond
     [(null? prev-winders)
      (call/cc proc)]
     [else
      ;; drop winders before capturing continuation:
      (#%$current-winders '())
      (begin0
       (call/cc proc)
       (#%$current-winders prev-winders))])))

(define (call-as-non-tail proc)
  (proc)
  '(error 'call-as-non-tail "shouldn't get to frame that was meant to be discarded"))

;; Make a frame like `current-mf`, but with more of a mark stack appended
(define (metacontinuation-frame-merge current-mf mark-stack)
  (make-metacontinuation-frame (metacontinuation-frame-tag current-mf)
                               (metacontinuation-frame-resume-k current-mf)
                               (metacontinuation-frame-resume-k/no-wind current-mf)
                               (metacontinuation-frame-empty-k current-mf)
                               (mark-stack-append mark-stack
                                                  (metacontinuation-frame-mark-stack current-mf))
                               #f
                               #f
                               (metacontinuation-frame-cc-guard current-mf)))

;; ----------------------------------------

(define/who (abort-current-continuation tag . args)
  (check who continuation-prompt-tag? tag)
  (maybe-future-barricade tag)
  (check-prompt-tag-available 'abort-current-continuation (strip-impersonator tag))
  (start-uninterrupted 'abort)
  (let ([args (apply-impersonator-abort-wrapper tag args)]
        [tag (strip-impersonator tag)])
    (do-abort-current-continuation tag args #t)))

(define (unsafe-abort-current-continuation/no-wind tag arg)
  (start-uninterrupted 'abort)
  (let ([args (apply-impersonator-abort-wrapper tag (list arg))]
        [tag (strip-impersonator tag)])
    (do-abort-current-continuation tag args #f)))

(define (do-abort-current-continuation tag args wind?)
  (assert-in-uninterrupted)
  (cond
   [(null? (current-metacontinuation))
    ;; A reset handler must end the uninterrupted region:
    ((reset-handler))]
   [else
    (unless wind? (#%$current-winders '()))
    (let ([mf (car (current-metacontinuation))])
      ((metacontinuation-frame-resume-k/no-wind mf)
       ;; An `aborting` record tells the metacontinuation's continuation
       ;; to handle to continue jumping:
       (make-aborting tag args wind?)))]))

;; ----------------------------------------

(define/who (call-with-continuation-barrier p)
  (check who (procedure-arity-includes/c 0) p)
  (start-uninterrupted 'barrier)
  (call-in-empty-metacontinuation-frame
   the-barrier-prompt-tag ; <- recognized as a barrier by continuation capture or call
   #f
   (lambda ()
     (end-uninterrupted 'barrier)
     (|#%app| p))))

;; ----------------------------------------
;; Capturing and applying continuations

(define-record continuation ())
(define-record full-continuation continuation (k mark-stack empty-k mc))
(define-record composable-continuation full-continuation ())
(define-record composable-continuation/no-wind composable-continuation ())
(define-record non-composable-continuation full-continuation (tag))
(define-record escape-continuation continuation (tag))

(define/who call-with-current-continuation
  (case-lambda
    [(proc) (call-with-current-continuation proc
                                            the-default-continuation-prompt-tag)]
    [(proc tag)
     (check who (procedure-arity-includes/c 1) proc)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call-with-end-uninterrupted
      (lambda ()
        (call/cc
         (lambda (k)
           (|#%app|
            proc
            (make-non-composable-continuation
             k
             (current-mark-stack)
             (current-empty-k)
             (extract-metacontinuation 'call-with-current-continuation (strip-impersonator tag) #t)
             tag))))))]))

(define/who call-with-composable-continuation
  (case-lambda
    [(p) (call-with-composable-continuation p the-default-continuation-prompt-tag)]
    [(p tag)
     (check who (procedure-arity-includes/c 1) p)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call-with-composable-continuation* p tag #t)]))

(define (call-with-composable-continuation* p tag wind?)
  (call-with-end-uninterrupted
   (lambda ()
     ((if wind? call/cc call/cc-no-winders)
      (lambda (k)
        (|#%app|
         p
         ((if wind?
              make-composable-continuation
              make-composable-continuation/no-wind)
          k
          (current-mark-stack)
          (current-empty-k)
          (extract-metacontinuation 'call-with-composable-continuation (strip-impersonator tag) #f))))))))

(define (unsafe-call-with-composable-continuation/no-wind p tag)
  (call-with-composable-continuation* p tag #f))

(define/who (call-with-escape-continuation p)
  (check who (procedure-arity-includes/c 1) p)
  (let ([tag (make-continuation-prompt-tag)])
    (call-with-continuation-prompt
     (lambda ()
       (|#%app| p (make-escape-continuation tag)))
     tag
     values)))

;; Applying a continuation calls this internal function:
(define (apply-continuation c args)
  (assert-in-uninterrupted)
  (cond
   [(composable-continuation? c)
    ;; To compose the metacontinuation, first make sure the current
    ;; continuation is reified in `(current-metacontinuation)`:
    (call-in-empty-metacontinuation-frame
     #f
     fail-abort-to-delimit-continuation
     (lambda ()
       ;; The current metacontinuation frame has an
       ;; empty continuation, so we can "replace" that
       ;; with the composable one:
       (if (composable-continuation/no-wind? c)
           (apply-immediate-continuation/no-wind c args)
           (apply-immediate-continuation c (reverse (full-continuation-mc c)) args))))]
   [(non-composable-continuation? c)
    (let* ([tag (non-composable-continuation-tag c)])
      (let-values ([(common-mc   ; shared part of the current metacontinuation
                     rmc-append) ; non-shared part of the destination metacontinuation
                    ;; We check every time, just in case control operations
                    ;; change the current continuation out from under us.
                    (find-common-metacontinuation (full-continuation-mc c)
                                                  (current-metacontinuation)
                                                  (strip-impersonator tag))])
        (cond
         [(eq? common-mc (current-metacontinuation))
          ;; Add a cc guard if `tag` is impersonated:
          (wrap-cc-guard-for-impersonator! tag)
          ;; Replace the current metacontinuation frame's continuation
          ;; with the saved one; this replacement will take care of any
          ;; shared winders within the frame.
          (apply-immediate-continuation c rmc-append args)]
         [else
          ;; Jump back to the nearest prompt, then continue jumping
          ;; as needed from there:
          (let ([mf (car (current-metacontinuation))])
            ((metacontinuation-frame-resume-k/no-wind mf)
             ;; An `applying` record tells the metacontinuation's continuation
             ;; to continue jumping:
             (make-applying c args)))])))]
   [(escape-continuation? c)
    (let ([tag (escape-continuation-tag c)])
      (unless (continuation-prompt-available? tag)
        (end-uninterrupted 'escape-fail)
        (raise-continuation-error '|continuation application|
                                  "attempt to jump into an escape continuation"))
      (do-abort-current-continuation tag args #t))]))

;; Apply a continuation within the current metacontinuation frame:
(define (apply-immediate-continuation c rmc args)
  (call-with-appended-metacontinuation
   rmc
   (lambda ()
     (current-mark-stack (full-continuation-mark-stack c))
     (current-empty-k (full-continuation-empty-k c))
     (apply (full-continuation-k c) args))))

;; Like `apply-immediate-continuation`, but don't run metacontinuation
;; winders; the `(full-continuation-k c)` part should be a non-winding
;; variant, too:
(define (apply-immediate-continuation/no-wind c args)
  (current-metacontinuation (append
                             (map metacontinuation-frame-clear-cache (full-continuation-mc c))
                             (current-metacontinuation)))
  (current-mark-stack (full-continuation-mark-stack c))
  (current-empty-k (full-continuation-empty-k c))
  (apply (full-continuation-k c) args))

;; Used as a "handler" for a prompt without a tag, which is used for
;; composable continuations
(define (fail-abort-to-delimit-continuation . args)
  (error 'abort "trying to abort to a delimiter continuation frame"))

;; Find common metacontinuation to keep due to a combination of:
;; the metacontinuation is beyond the relevant prompt, or the
;; metacontinuation fragment before the prompt is also shared
;; with the composable continuation's metacontinuation (so we
;; should not unwind and rewind those metacontinuation frames)
(define (find-common-metacontinuation mc current-mc tag)
  (define-values (rev-current ; (list (cons mf mc) ...)
                  base-current-mc)
    ;; Get the reversed prefix of `current-mc` that is to be
    ;; replaced by `mc`:
    (let loop ([current-mc current-mc] [accum null])
      (cond
       [(null? current-mc)
        (unless (or (eq? tag the-default-continuation-prompt-tag)
                    (eq? tag the-root-continuation-prompt-tag))
          (raise-arguments-error 'apply-continuation
                                 "continuation includes no prompt with the given tag"
                                 "tag" tag))
        (values accum null)]
       [(eq? tag (metacontinuation-frame-tag (car current-mc)))
        (values accum current-mc)]
       [else
        (loop (cdr current-mc)
              ;; Accumulate this frame plus the chain that
              ;; we should keep if this frame is shared:
              (cons (cons (car current-mc) current-mc)
                    accum))])))
  (define rev-mc (reverse mc))
  ;; Work from the tail backwards (which is forward in the reverse
  ;; lists): If the continuations are the same for the two frames,
  ;; then the metacontinuation frame should not be unwound
  (let loop ([rev-current rev-current]
             [rev-mc rev-mc]
             [base-current-mc base-current-mc])
    (cond
     [(null? rev-mc) (values base-current-mc '())]
     [(null? rev-current)
      (check-for-barriers rev-mc)
      ;; Return the shared part plus the unshared-to-append part
      (values base-current-mc rev-mc)]
     [(eq? (metacontinuation-frame-resume-k (car rev-mc))
           (metacontinuation-frame-resume-k (caar rev-current)))
      ;; Matches, so update base and look shallower
      (loop (cdr rev-current)
            (cdr rev-mc)
            (cdar rev-current))]
     [else
      ;; Doesn't match, so we've found the shared part;
      ;; check for barriers that we'd have to reintroduce
      (check-for-barriers rev-mc)
      ;; Return the shared part plus the unshared-to-append part
      (values (cdar rev-current) rev-mc)])))

(define (check-for-barriers rev-mc)
  (let loop ([rev-mc rev-mc])
    (unless (null? rev-mc)
      (when (eq? (metacontinuation-frame-tag (car rev-mc))
                 the-barrier-prompt-tag)
        (end-uninterrupted 'hit-barrier)
        (raise-continuation-error '|continuation application|
                                  "attempt to cross a continuation barrier"))
      (loop (cdr rev-mc)))))

(define (call-with-end-uninterrupted thunk)
  ;; Using `call/cm` with a key of `none` ensures that we have an
  ;; `(end-uninterrupted)` in the immediate continuation, but
  ;; keeping the illusion that `thunk` is called in tail position.
  (call/cm none #f thunk))

(define (set-continuation-applicables!)
  (let ([add (lambda (rtd)
               (struct-property-set! prop:procedure
                                     rtd
                                     (lambda (c . args)
                                       (start-uninterrupted 'continue)
                                       (apply-continuation c args))))])
    (add (record-type-descriptor composable-continuation))
    (add (record-type-descriptor composable-continuation/no-wind))
    (add (record-type-descriptor non-composable-continuation))
    (add (record-type-descriptor escape-continuation))))

;; ----------------------------------------
;; Metacontinuation operations for continutions

;; Extract a prefix of `(current-metacontinuation)` up to `tag`
(define (extract-metacontinuation who tag barrier-ok?)
  (define (check-barrier-ok saw-barrier?)
    (when (and saw-barrier? (not barrier-ok?))
      (raise-continuation-error who "cannot capture past continuation barrier")))
  (let loop ([mc (current-metacontinuation)] [saw-barrier? #f])
    (cond
     [(null? mc)
      (unless (or (eq? tag the-root-continuation-prompt-tag)
                  (eq? tag the-default-continuation-prompt-tag))
        (raise-arguments-error who "continuation includes no prompt with the given tag"
                               "tag" tag))
      (check-barrier-ok saw-barrier?)
      '()]
     [else
      (let ([a-tag (metacontinuation-frame-tag (car mc))])
        (cond
         [(eq? a-tag tag)
          (check-barrier-ok saw-barrier?)
          '()]
         [else
          (cons (metacontinuation-frame-clear-cache (car mc))
                (loop (cdr mc) (or saw-barrier?
                                   (eq? a-tag the-barrier-prompt-tag))))]))])))

(define (check-prompt-tag-available who tag)
  (unless (continuation-prompt-available? tag)
    (raise-arguments-error who "continuation includes no prompt with the given tag"
                           "tag" tag)))

(define (call-with-appended-metacontinuation rmc proc)
  ;; Assumes that the current metacontinuation frame is ready to be
  ;; replaced with `mc` plus `proc`.
  ;; In the simple case of no winders and an empty frame immediate
  ;;  metacontinuation fame, we could just
  ;;  (current-metacontinuation (append mc (current-metacontinuation)))
  ;; But, to run winders and replace anything in the current frame,
  ;; we proceed frame-by-frame in `mc`.
  (let loop ([rmc rmc])
    (cond
     [(null? rmc) (proc)]
     [else
      (let ([mf (metacontinuation-frame-clear-cache (car rmc))]
            [rmc (cdr rmc)])
        (cond
         [(eq? (metacontinuation-frame-resume-k mf)
               (metacontinuation-frame-resume-k/no-wind mf))
          ;; No winders in this metacontinuation frame, so take a shortcut
          (current-metacontinuation (cons mf (current-metacontinuation)))
          (loop rmc)]
         [else
          ((metacontinuation-frame-resume-k mf)
           (make-appending (lambda ()
                             ;; resuming appended winders, but we'll keep
                             ;; them in the metacontinuation, instead:
                             (#%$current-winders '())
                             ;; add frame:
                             (current-metacontinuation (cons mf (current-metacontinuation)))
                             ;; next...
                             (loop rmc))))]))])))

(define (metacontinuation-frame-clear-cache mf)
  (metacontinuation-frame-merge mf #f))

;; Get/cache a converted list of marks for a metacontinuation
(define (metacontinuation-marks mc)
  (cond
   [(null? mc) null]
   [else (let ([mf (car mc)])
           (or (metacontinuation-frame-mark-chain mf)
               (let* ([r (metacontinuation-marks (cdr mc))]
                      [l (cons (make-mark-chain-frame
                                (metacontinuation-frame-tag mf)
                                (mark-stack-to-marks
                                 (metacontinuation-frame-mark-stack mf)))
                               r)])
                 (set-metacontinuation-frame-mark-chain! mf l)
                 l)))]))

;; ----------------------------------------
;; Continuation marks

(define-record continuation-mark-set (mark-chain traces))
(define-record mark-stack-frame (prev   ; prev frame
                                 k      ; continuation for this frame
                                 table  ; intmap mapping keys to values
                                 flat)) ; #f or cached list that contains only tables and elem+caches

;; A mark stack is made of marks-stack frames:
(define current-mark-stack (internal-make-thread-parameter #f))

(define ($current-mark-stack) (current-mark-stack))

;; See copy in "expander.sls"
(define-syntax with-continuation-mark
  (syntax-rules ()
    [(_ key val body)
     (call/cm key val (lambda () body))]))

;; Sets a continuation mark.
;; Using `none` as a key ensures that a
;; stack-restoring frame is pushed without
;; adding a key--value mapping.
(define (call/cm key val proc)
  (call/cc
   (lambda (k)
     (if (and (current-mark-stack)
              (eq? k (mark-stack-frame-k (current-mark-stack))))
         (begin
           (unless (eq? key none)
             (set-mark-stack-frame-table! (current-mark-stack)
                                          (intmap-set/cm-key (mark-stack-frame-table (current-mark-stack))
                                                             key
                                                             val))
             (set-mark-stack-frame-flat! (current-mark-stack) #f))
           (proc))
         (begin0
          (call/cc
           (lambda (k)
             (current-mark-stack
              (make-mark-stack-frame (current-mark-stack)
                                     k
                                     (if (eq? key none)
                                         empty-hasheq
                                         (intmap-set/cm-key empty-hasheq key val))
                                     #f))
             (proc)))
          (current-mark-stack (mark-stack-frame-prev (current-mark-stack)))
          ;; To support exiting an uninterrupted region on resumption of
          ;; a continuation (see `call-with-end-uninterrupted`):
          (when (current-in-uninterrupted)
            (pariah (end-uninterrupted/call-hook 'cm))))))))

;; For internal use, such as `dynamic-wind` pre thunks:
(define (call/cm/nontail key val proc)
  (current-mark-stack
   (make-mark-stack-frame (current-mark-stack)
                          #f
                          (hasheq key val)
                          #f))
  (proc)
  ;; If we're in an escape process, then `(current-mark-stack)` might not
  ;; match, and that's ok; it doesn't matter what we set the mark stack to
  ;; in that case, so we do something that's right for the non-escape case
  (when (current-mark-stack)
    (current-mark-stack (mark-stack-frame-prev (current-mark-stack)))))

(define (current-mark-chain)
  (get-current-mark-chain (current-mark-stack) (current-metacontinuation)))

(define (mark-stack-to-marks mark-stack)
  (let loop ([mark-stack mark-stack])
    (cond
     [(not mark-stack) null]
     [(mark-stack-frame-flat mark-stack) => (lambda (l) l)]
     [else
      (let ([l (cons (mark-stack-frame-table mark-stack)
                     (loop (mark-stack-frame-prev mark-stack)))])
        (set-mark-stack-frame-flat! mark-stack l)
        l)])))

(define-record mark-chain-frame (tag marks))

(define (get-current-mark-chain mark-stack mc)
  (cons (make-mark-chain-frame
         #f ; no tag
         (mark-stack-to-marks mark-stack))
        (metacontinuation-marks mc)))

(define (prune-mark-chain-prefix tag mark-chain)
  (cond
   [(eq? tag (mark-chain-frame-tag (elem+cache-strip (car mark-chain))))
    mark-chain]
   [else
    (prune-mark-chain-prefix tag (cdr mark-chain))]))

(define (prune-mark-chain-suffix tag mark-chain)
  (cond
   [(null? mark-chain) null]
   [(eq? tag (mark-chain-frame-tag (elem+cache-strip (car mark-chain))))
    null]
   [else
    (let ([rest-mark-chain (prune-mark-chain-suffix tag (cdr mark-chain))])
      (if (eq? rest-mark-chain (cdr mark-chain))
          mark-chain
          (cons (car mark-chain)
                rest-mark-chain)))]))

;; ----------------------------------------
;; Continuation-mark caching

;; A `elem+cache` can replace a plain table in a "flat" variant of the
;; mark stack within a metacontinuation frame, or in a mark-stack
;; chain for a metacontinuation. The cache is a table that records
;; results found later in the list, which allows
;; `continuation-mark-set-first` to take amortized constant time.
(define-record elem+cache (elem cache))
(define (elem+cache-strip t) (if (elem+cache? t) (elem+cache-elem t) t))

(define/who call-with-immediate-continuation-mark
  (case-lambda
    [(key proc) (call-with-immediate-continuation-mark key proc #f)]
    [(key proc default-v)
     (check who (procedure-arity-includes/c 1) proc)
     (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'call-with-immediate-continuation-mark key)])
       (cond
        [(not (current-mark-stack)) (|#%app| proc default-v)]
        [else
         (call/cc (lambda (k)
                    (if (eq? k (mark-stack-frame-k (current-mark-stack)))
                        (|#%app| proc (let ([v (intmap-ref (mark-stack-frame-table (current-mark-stack))
                                                           key
                                                           none)])
                                        (if (eq? v none)
                                            default-v
                                            (wrapper v))))
                        (|#%app| proc default-v))))]))]))

(define/who continuation-mark-set-first
  (case-lambda
    [(marks key) (continuation-mark-set-first marks key #f)]
    [(marks key none-v)
     (continuation-mark-set-first marks key none-v
                                  ;; Treat `break-enabled-key` and `parameterization-key`, specially
                                  ;; so that things like `current-break-parameterization` work without
                                  ;; referencing the root continuation prompt tag
                                  (if (or (eq? key break-enabled-key)
                                          (eq? key parameterization-key))
                                      the-root-continuation-prompt-tag
                                      the-default-continuation-prompt-tag))]
    [(marks key none-v prompt-tag)
     (check who continuation-mark-set? :or-false marks)
     (check who continuation-prompt-tag? prompt-tag)
     (maybe-future-barricade prompt-tag)
     (let ([prompt-tag (strip-impersonator prompt-tag)])
       (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'continuation-mark-set-first key)])
         (let ([v (marks-search (or (and marks
                                         (continuation-mark-set-mark-chain marks))
                                    (current-mark-chain)) ;; because of this.
                                key
                                ;; elem-stop?:
                                (lambda (mcf)
                                  (eq? (mark-chain-frame-tag mcf) prompt-tag))
                                ;; elem-ref:
                                (lambda (mcf key none)
                                  ;; Search within a metacontinuation frame
                                  (let ([marks (mark-chain-frame-marks mcf)])
                                    (marks-search marks
                                                  key
                                                  ;; elem-stop?:
                                                  (lambda (t) #f)
                                                  ;; elem-ref:
                                                  intmap-ref
                                                  ;; fail-k:
                                                  (lambda () none)
                                                  ;; strip & combine:
                                                  (lambda (v) v)
                                                  (lambda (v old) v))))
                                ;; fail-k:
                                (lambda () none)
                                ;; strip & combine --- cache results at the metafunction
                                ;; level should depend on the prompt tag, so make the cache
                                ;; value another table level mapping the prompt tag to the value:
                                (lambda (v) (hash-ref v prompt-tag none2))
                                (lambda (v old) (intmap-set (if (eq? old none2) empty-hasheq old) prompt-tag v)))])
           (cond
            [(eq? v none)
             ;; More special treatment of built-in keys
             (cond
              [(eq? key parameterization-key)
               empty-parameterization]
              [(eq? key break-enabled-key)
               (current-engine-init-break-enabled-cell none-v)]
              [else
               none-v])]
            [else (wrapper v)]))))]))

;; To make `continuation-mark-set-first` constant-time, if we traverse
;; N elements to get an answer, then cache the answer at N/2 elements.
(define (marks-search elems key elem-stop? elem-ref fail-k
                      strip-cache-result combine-cache-result)
  (let loop ([elems elems] [elems/cache-pos elems] [cache-step? #f])
    (cond
     [(or (null? elems)
          (elem-stop? (elem+cache-strip (car elems))))
      ;; Not found
      (cache-result! elems elems/cache-pos key none combine-cache-result)
      (fail-k)]
     [else
      (let ([t (car elems)])
        (define (check-elem t)
          (let ([v (elem-ref t key none)])
            (cond
             [(eq? v none)
              ;; Not found at this point; keep looking
              (loop (cdr elems)
                    (if cache-step? (cdr elems/cache-pos) elems/cache-pos)
                    (not cache-step?))]
             [else
              ;; Found it
              (cache-result! elems elems/cache-pos key v combine-cache-result)
              v])))
        (cond
         [(elem+cache? t)
          (let ([v (intmap-ref (elem+cache-cache t) key none2)])
            (cond
             [(eq? v none2)
              ;; No mapping in cache, so try the element and continue:
              (check-elem (elem+cache-elem t))]
             [else
              (let ([v (strip-cache-result v)])
                (cond
                 [(eq? v none2)
                  ;; Strip filtered this cache entry away, so try the element:
                  (check-elem (elem+cache-elem t))]
                 [(eq? v none)
                  ;; The cache records that it's not in the rest:
                  (cache-result! elems elems/cache-pos key none combine-cache-result)
                  (fail-k)]
                 [else
                  ;; The cache provides a value from the rest:
                  (cache-result! elems elems/cache-pos key v combine-cache-result)
                  v]))]))]
         [else
          ;; Try the element:
          (check-elem t)]))])))

;; To make `continuation-mark-set-first` constant-time, cache
;; a key--value mapping at a point that's half-way in
(define (cache-result! marks marks/cache-pos key v combine-cache-result)
  (unless (eq? marks marks/cache-pos)
    (let* ([t (car marks/cache-pos)]
           [new-t (if (elem+cache? t)
                      t
                      (make-elem+cache t empty-hasheq))])
      (unless (eq? t new-t)
        (set-car! marks/cache-pos new-t))
      (let ([old (intmap-ref (elem+cache-cache new-t) key none2)])
        (set-elem+cache-cache! new-t (intmap-set (elem+cache-cache new-t)
                                               key
                                               (combine-cache-result v old)))))))

(define/who continuation-mark-set->list
  (case-lambda
    [(marks key) (continuation-mark-set->list marks key the-default-continuation-prompt-tag)]
    [(marks key prompt-tag)
     (check who continuation-mark-set? :or-false marks)
     (check who continuation-prompt-tag? prompt-tag)
     (maybe-future-barricade prompt-tag)
     (let ([prompt-tag (strip-impersonator prompt-tag)])
       (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'continuation-mark-set->list key)])
         (let chain-loop ([mark-chain (or (and marks
                                               (continuation-mark-set-mark-chain marks))
                                          (current-mark-chain))])
           (cond
            [(null? mark-chain)
             null]
            [else
             (let* ([mcf (elem+cache-strip (car mark-chain))])
               (cond
                [(eq? (mark-chain-frame-tag mcf) prompt-tag)
                 null]
                [else
                 (let loop ([marks (mark-chain-frame-marks mcf)])
                   (cond
                    [(null? marks)
                     (chain-loop (cdr mark-chain))]
                    [else
                     (let* ([v (intmap-ref (elem+cache-strip (car marks)) key none)])
                       (if (eq? v none)
                           (loop (cdr marks))
                           (cons (wrapper v) (loop (cdr marks)))))]))]))]))))]))

(define/who continuation-mark-set->list*
  (case-lambda
    [(marks keys) (continuation-mark-set->list* marks keys the-default-continuation-prompt-tag #f)]
    [(marks keys prompt-tag) (continuation-mark-set->list* marks keys prompt-tag #f)]
    [(marks keys prompt-tag none-v)
     (check who continuation-mark-set? :or-false marks)
     (check who list? keys)
     (check who continuation-prompt-tag? prompt-tag)
     (maybe-future-barricade prompt-tag)
     (let ([prompt-tag (strip-impersonator prompt-tag)])
       (let-values ([(keys wrappers) (map/2-values (lambda (k)
                                                     (extract-continuation-mark-key-and-wrapper 'continuation-mark-set->list* k))
                                                   keys)])
         (let* ([n (length keys)]
                [tmp (make-vector n)])
           (let chain-loop ([mark-chain (or (and marks
                                                 (continuation-mark-set-mark-chain marks))
                                            (current-mark-chain))])
             (cond
              [(null? mark-chain)
               null]
              [else
               (let* ([mcf (elem+cache-strip (car mark-chain))])
                 (cond
                  [(eq? (mark-chain-frame-tag mcf) prompt-tag)
                   null]
                  [else
                   (let loop ([marks (mark-chain-frame-marks mcf)])
                     (cond
                      [(null? marks)
                       (chain-loop (cdr mark-chain))]
                      [else
                       (let ([t (elem+cache-strip (car marks))])
                         (let key-loop ([keys keys] [wrappers wrappers] [i 0] [found? #f])
                           (cond
                            [(null? keys)
                             (if found?
                                 (let ([vec (vector-copy tmp)])
                                   (cons vec (loop (cdr marks))))
                                 (loop (cdr marks)))]
                            [else
                             (let ([v (intmap-ref t (car keys) none)])
                               (cond
                                [(eq? v none)
                                 (vector-set! tmp i none-v)
                                 (key-loop (cdr keys) (cdr wrappers) (add1 i) found?)]
                                [else
                                 (vector-set! tmp i ((car wrappers) v))
                                 (key-loop (cdr keys) (cdr wrappers) (add1 i) #t)]))])))]))]))])))))]))

(define/who (continuation-mark-set->context marks)
  (check who continuation-mark-set? marks)
  (traces->context (continuation-mark-set-traces marks)))

(define/who current-continuation-marks
  (case-lambda
    [() (current-continuation-marks the-default-continuation-prompt-tag)]
    [(tag)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call/cc
      (lambda (k)
        (make-continuation-mark-set (prune-mark-chain-suffix (strip-impersonator tag) (current-mark-chain))
                                    (cons (continuation->trace k)
                                          (get-metacontinuation-traces (current-metacontinuation))))))]))

(define/who continuation-marks
  (case-lambda
    [(k) (continuation-marks k (default-continuation-prompt-tag))]
    [(k tag)
     (check who continuation? :or-false k)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (let ([tag (strip-impersonator tag)])
       (cond
        [(full-continuation? k)
         (make-continuation-mark-set
          (prune-mark-chain-suffix
           tag
           (get-current-mark-chain (full-continuation-mark-stack k)
                                   (full-continuation-mc k)))
          k)]
        [(escape-continuation? k)
         (unless (continuation-prompt-available? (escape-continuation-tag k))
           (raise-arguments-error '|continuation application|
                                  "escape continuation not in the current continuation"))
         (make-continuation-mark-set
          (prune-mark-chain-suffix
           tag
           (prune-mark-chain-prefix (escape-continuation-tag k) (current-mark-chain)))
          k)]
        [else
         (make-continuation-mark-set null #f)]))]))

(define (mark-stack-append a b)
  (cond
   [(not a) b]
   [(not b) a]
   [else
    (make-mark-stack-frame (mark-stack-append (mark-stack-frame-prev a) b)
                           (mark-stack-frame-k a)
                           (mark-stack-frame-table a)
                           #f)]))

(define (get-metacontinuation-traces mc)
  (cond
   [(null? mc) '()]
   [(metacontinuation-frame-traces (car mc))
    => (lambda (traces) traces)]
   [else
    (let ([traces
           (cons (continuation->trace (metacontinuation-frame-resume-k (car mc)))
                 (get-metacontinuation-traces (cdr mc)))])
      (set-metacontinuation-frame-traces! (car mc) traces)
      traces)]))

;; ----------------------------------------
;; Continuation-mark keys: impersonators, and chaperones

(define-record-type (continuation-mark-key create-continuation-mark-key authentic-continuation-mark-key?)
  (fields (mutable name))) ; `mutable` ensures that `create-...` allocates

(define-record continuation-mark-key-impersonator impersonator (get set))
(define-record continuation-mark-key-chaperone chaperone (get set))

(define make-continuation-mark-key
  (case-lambda
   [() (make-continuation-mark-key (gensym))]
   [(name) (create-continuation-mark-key name)]))

(define (continuation-mark-key? v)
  (or (authentic-continuation-mark-key? v)
      (and (impersonator? v)
           (authentic-continuation-mark-key? (impersonator-val v)))))

;; Like `intmap-set`, but handles continuation-mark-key impersonators
(define (intmap-set/cm-key ht k v)
  (cond
   [(and (impersonator? k)
         (authentic-continuation-mark-key? (impersonator-val k)))
    (let loop ([k k] [v v])
      (cond
       [(or (continuation-mark-key-impersonator? k)
            (continuation-mark-key-chaperone? k))
        (let ([new-v (|#%app|
                      (if (continuation-mark-key-impersonator? k)
                          (continuation-mark-key-impersonator-set k)
                          (continuation-mark-key-chaperone-set k))
                      v)])
          (unless (or (continuation-mark-key-impersonator? k)
                      (chaperone-of? new-v v))
            (raise-chaperone-error 'with-continuation-mark "value" v new-v))
          (loop (impersonator-next k) new-v))]
       [(impersonator? k)
        (loop (impersonator-next k) v)]
       [else
        (intmap-set ht k v)]))]
   [else (intmap-set ht k v)]))

;; Extracts the key and converts the wrapper functions into
;; a single function:
(define (extract-continuation-mark-key-and-wrapper who k)
  (cond
   [(and (impersonator? k)
         (authentic-continuation-mark-key? (impersonator-val k)))
    (let loop ([k k])
      (cond
       [(or (continuation-mark-key-impersonator? k)
            (continuation-mark-key-chaperone? k))
        (let ([get (if (continuation-mark-key-impersonator? k)
                       (continuation-mark-key-impersonator-get k)
                       (continuation-mark-key-chaperone-get k))]
              [get-rest (loop (impersonator-next k))])
          (lambda (v)
            (let* ([v (get-rest v)]
                   [new-v (|#%app| get v)])
              (unless (or (continuation-mark-key-impersonator? k)
                          (chaperone-of? new-v v))
                (raise-chaperone-error who "value" v new-v))
              new-v)))]
       [(impersonator? k)
        (loop (impersonator-next k))]
       [else
        (lambda (v) v)]))]
   [else
    (values k (lambda (v) v))]))

(define (map/2-values f l)
  (cond
   [(null? l) (values '() '())]
   [else
    (let-values ([(a b) (f (car l))])
      (let-values ([(a-r b-r) (map/2-values f (cdr l))])
        (values (cons a a-r) (cons b b-r))))]))

(define (impersonate-continuation-mark-key key get set . props)
  (do-impersonate-continuation-mark-key 'impersonate-continuation-mark-key
                                        key get set props
                                        make-continuation-mark-key-impersonator))

(define (chaperone-continuation-mark-key key get set . props)
  (do-impersonate-continuation-mark-key 'chaperone-continuation-mark-key
                                        key get set props
                                        make-continuation-mark-key-chaperone))

(define (do-impersonate-continuation-mark-key who
                                              key get set props
                                              make-continuation-mark-key-impersonator)
  (check who continuation-mark-key? key)
  (check who (procedure-arity-includes/c 1) get)
  (check who (procedure-arity-includes/c 1) set)
  (make-continuation-mark-key-impersonator (strip-impersonator key)
                                           key
                                           (add-impersonator-properties who
                                                                        props
                                                                        (if (impersonator? key)
                                                                            (impersonator-props key)
                                                                            empty-hasheq))
                                           get
                                           set))

;; ----------------------------------------
;; Continuation prompt tags: impersonators, and chaperones

(define (continuation-prompt-tag? v)
  (or (authentic-continuation-prompt-tag? v)
      (and (impersonator? v)
           (authentic-continuation-prompt-tag? (impersonator-val v)))))

(define-record continuation-prompt-tag-impersonator impersonator (procs))
(define-record continuation-prompt-tag-chaperone chaperone (procs))

(define-record continuation-prompt-tag-procs (handler abort cc-guard cc-impersonate))

(define (continuation-prompt-tag-impersonator-or-chaperone? tag)
  (or (continuation-prompt-tag-impersonator? tag)
      (continuation-prompt-tag-chaperone? tag)))

(define (continuation-prompt-tag-impersonator-or-chaperone-procs tag)
  (if (continuation-prompt-tag-impersonator? tag)
      (continuation-prompt-tag-impersonator-procs tag)
      (continuation-prompt-tag-chaperone-procs tag)))

(define (impersonate-prompt-tag tag handler abort . args)
  (do-impersonate-prompt-tag 'impersonate-prompt-tag tag handler abort args
                             make-continuation-prompt-tag-impersonator))

(define (chaperone-prompt-tag tag handler abort . args)
  (do-impersonate-prompt-tag 'chaperone-prompt-tag tag handler abort args
                             make-continuation-prompt-tag-chaperone))

(define (do-impersonate-prompt-tag who tag handler abort args
                                   make-continuation-prompt-tag-impersonator)
  (check who continuation-prompt-tag? tag)
  (check who procedure? handler)
  (check who procedure? abort)
  (let* ([cc-guard (and (pair? args)
                        (procedure? (car args))
                        (car args))]
         [args (if cc-guard (cdr args) args)]
         [callcc-impersonate (and (pair? args)
                                  (procedure? (car args))
                                  (car args))]
         [args (if callcc-impersonate (cdr args) args)])
    (when callcc-impersonate
      (check who (procedure-arity-includes/c 1) abort))
    (make-continuation-prompt-tag-impersonator
     (strip-impersonator tag)
     tag
     (add-impersonator-properties who
                                  args
                                  (if (impersonator? tag)
                                      (impersonator-props tag)
                                      empty-hasheq))
     (make-continuation-prompt-tag-procs handler abort cc-guard (or callcc-impersonate values)))))

(define (apply-prompt-tag-interposition who at-when what
                                        wrapper args chaperone?)
  (call-with-values (lambda () (apply wrapper args))
    (lambda new-args
      (unless (= (length args) (length new-args))
        (raise-result-arity-error at-when (length args) new-args))
      (when chaperone?
        (for-each (lambda (arg new-arg)
                    (unless (chaperone-of? new-arg arg)
                      (raise-chaperone-error who what arg new-arg)))
                  args new-args))
      new-args)))

(define (wrap-handler-for-impersonator tag handler)
  (let loop ([tag tag])
    (cond
     [(continuation-prompt-tag-impersonator-or-chaperone? tag)
      (let ([handler (loop (impersonator-next tag))]
            [h (continuation-prompt-tag-procs-handler
                (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
            [chaperone? (continuation-prompt-tag-chaperone? tag)])
        (lambda args
          (apply handler
                 (apply-prompt-tag-interposition 'call-with-continuation-prompt
                                                 "use of prompt-handler redirecting procedure"
                                                 "prompt-handler argument"
                                                 h args chaperone?))))]
     [(impersonator? tag)
      (loop (impersonator-next tag))]
     [else handler])))

(define (apply-impersonator-abort-wrapper tag args)
  (let loop ([tag tag] [args args])
    (cond
     [(continuation-prompt-tag-impersonator-or-chaperone? tag)
      (let ([a (continuation-prompt-tag-procs-abort
                (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
            [chaperone? (continuation-prompt-tag-chaperone? tag)])
        (loop (impersonator-next tag)
              (apply-prompt-tag-interposition 'abort-current-continuation
                                               "use of prompt-abort redirecting procedure"
                                               "prompt-abort argument"
                                               a args chaperone?)))]
     [(impersonator? tag)
      (loop (impersonator-next tag) args)]
     [else args])))

(define (compose-cc-guard-for-impersonator! tag)
  (let loop! ([tag tag])
    (cond
     [(continuation-prompt-tag-impersonator-or-chaperone? tag)
      (let ([cc-guard (continuation-prompt-tag-procs-cc-guard
                       (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
            [chaperone? (continuation-prompt-tag-chaperone? tag)])
        (loop! (impersonator-next tag))
        (when cc-guard
          (let ([old-cc-guard (current-cc-guard)])
            (current-cc-guard
             (lambda args
               (apply old-cc-guard
                      (apply-prompt-tag-interposition 'call-with-continuation-prompt
                                                      "use of `call/cc` result guard"
                                                      "prompt-result argument"
                                                      cc-guard args chaperone?)))))))]
     [(impersonator? tag)
      (loop! (impersonator-next tag))]
     [else (void)])))

(define (wrap-cc-guard-for-impersonator! tag)
  (let loop! ([tag tag])
    (cond
     [(continuation-prompt-tag-impersonator-or-chaperone? tag)
      (let ([cc-impersonate (continuation-prompt-tag-procs-cc-impersonate
                             (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
            [chaperone? (continuation-prompt-tag-chaperone? tag)])
        (loop! (impersonator-next tag))
        (let ([new-cc-guard (|#%app| cc-impersonate (current-cc-guard))])
          (when chaperone?
            (unless (chaperone-of? new-cc-guard (current-cc-guard))
              (raise-chaperone-error 'call-with-current-continuation
                                     "continuation-result guard"
                                     (current-cc-guard)
                                     new-cc-guard)))
          (current-cc-guard new-cc-guard)))]
     [(impersonator? tag)
      (loop! (impersonator-next tag))]
     [else (void)])))

;; ----------------------------------------

;; Wrap `dynamic-wind` for three tasks:

;; 1. set the mark stack on entry and exit to the saved mark stack.
;;    The saved mark stack is confined to the current continuation
;;    frame, so it's ok to use it if the current continuation is later
;;    applied to a different metacontinuation.

;; 2. Start and end uninterrupted regions on the boundaries of
;;    transitions between thunks.

;; 3. Perform a built-in `(parameterize-break #f ...)` around the pre
;;    and post thunks. This break parameterization needs to be built
;;    in so that it's put in place before exiting the uninterrupted region,
;;    but it assumes a particular implementation of break
;;    parameterizations.

(define (dynamic-wind pre thunk post)
  (let ([saved-mark-stack (current-mark-stack)])
    (define-syntax with-saved-mark-stack/non-break
      (syntax-rules ()
        [(_ who e ...)
         (let ([dest-mark-stack (current-mark-stack)])
           (current-mark-stack saved-mark-stack)
           (call/cm/nontail
            break-enabled-key (make-thread-cell #f #t)
            (lambda ()
              (end-uninterrupted who)
              e ...
              (start-uninterrupted who)))
           (current-mark-stack dest-mark-stack))]))
    (start-uninterrupted 'dw)
    (begin0
     (#%dynamic-wind
      (lambda ()
        (with-saved-mark-stack/non-break 'dw-pre
          (pre)))
      (lambda ()
        (end-uninterrupted/call-hook 'dw-body)
        (begin0
         (thunk)
         (start-uninterrupted 'dw-body)))
      (lambda ()
        (with-saved-mark-stack/non-break 'dw-post
          (post))))
     (end-uninterrupted/call-hook 'dw))))

;; ----------------------------------------

(define (raise-continuation-error who msg)
  (raise
   (|#%app|
    exn:fail:contract:continuation
    (string-append (symbol->string who) ": " msg)
    (current-continuation-marks))))

;; ----------------------------------------
;; Breaks

(define (call-with-break-disabled thunk)
  (call/cm
   break-enabled-key (make-thread-cell #f #t)
   thunk))

;; Some points where we jump out of uninterrupted mode are also points
;; where we might jump to a context where breaks are allowed. The
;; `continuation-mark-change-hook` function allows a thread scheduler to
;; inject a check at those points.
(define (end-uninterrupted/call-hook who)
  (end-uninterrupted who)
  (break-enabled-transition-hook))

(define break-enabled-transition-hook void)

(define (set-break-enabled-transition-hook! proc)
  (set! break-enabled-transition-hook proc))

;; ----------------------------------------
;; Metacontinuation swapping for engines

(define-record saved-metacontinuation (mc exn-state))

(define empty-metacontinuation (make-saved-metacontinuation '() (create-exception-state)))

;; Similar to `call-with-current-continuation` plus
;; applying an old continuation, but does not run winders;
;; this operation makes sense for thread or engine context
;; switches
(define (swap-metacontinuation saved proc)
  (cond
   [(current-system-wind-start-k)
    => (lambda (k) (swap-metacontinuation-with-system-wind saved proc k))]
   [else
    (call-in-empty-metacontinuation-frame
     #f
     fail-abort-to-delimit-continuation
     (lambda ()
       (let ([now-saved (make-saved-metacontinuation
                         (current-metacontinuation)
                         (current-exception-state))])
         (current-metacontinuation (saved-metacontinuation-mc saved))
         (current-exception-state (saved-metacontinuation-exn-state saved))
         (proc now-saved))))]))

;; ----------------------------------------

;; In "system-wind" mode for the current metacontinuation frame, run
;; the frame's winders when jumping out of the frame or back in,
;; because the frame uses host-Scheme parameters and/or `fluid-let`.
;; For example, jumping out/in the host compiler needs to save/restore
;; compiler state.
(define current-system-wind-start-k (internal-make-thread-parameter #f))

;; During `call-with-system-wind`, the current metacontinuation frame
;; must remain as the most recent one, so that `swap-metacontinuation`
;; can capture the system-wind part
(define (call-with-system-wind proc)
  ((call/cc
    (lambda (k)
      (current-system-wind-start-k k)
      (call-with-values
          proc
        (lambda args
          (lambda ()
            (current-system-wind-start-k #f)
            (apply values args))))))))

(define (swap-metacontinuation-with-system-wind saved proc start-k)
  (current-system-wind-start-k #f)
  (call/cc
   (lambda (system-wind-k) ; continuation with system `dynamic-wind` behavior
     ;; escape to starting point, running winders, before
     ;; capturing the rest of the metacontinuation:
     (start-k (lambda ()
                (let ([prefix (swap-metacontinuation saved proc)])
                  (current-system-wind-start-k start-k)
                  (system-wind-k prefix)))))))

(define (assert-not-in-system-wind)
  (CHECK-uninterrupted
   (when (current-system-wind-start-k)
     (internal-error 'not-in-system-wind "assertion failed"))))
