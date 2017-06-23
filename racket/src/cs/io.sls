(library (io)
  (export)
  (import (chezpart)
          (rename (only (chezscheme)
                        read-char peek-char
                        current-directory
                        format
                        error
                        input-port? output-port?
                        file-position flush-output-port
                        file-symbolic-link?)
                  [input-port? chez:input-port?]
                  [output-port? chez:output-port?]
                  [flush-output-port flush-output])
          (core)
          (thread))

  ;; ----------------------------------------
  ;; Tie knots:

  (define (path? v) (is-path? v))
  (define (path->string v) (1/path->string v))
  (define path->complete-path
    (case-lambda
     [(v) (1/path->complete-path v)]
     [(v wrt) (1/path->complete-path v wrt)]))
  (define (absolute-path? v) (1/absolute-path? v))
  (define (relative-path? v) (1/relative-path? v))

  ;; ----------------------------------------

  (define-syntax (who stx)
    (syntax-error stx "not bound"))

  (define-syntax (define/who stx)
    (syntax-case stx ()
      [(define/who (id . args) body ...)
       #'(define id
           (fluid-let-syntax ([who (lambda (stx)
                                     #''id)])
             (lambda args body ...)))]
      [(define/who id rhs)
       #'(define id
           (fluid-let-syntax ([who (lambda (stx)
                                     #''id)])
             rhs))]))

  (meta define (convert-type t)
        (syntax-case t (ref * rktio_bool_t)
          [(ref . _) #'uptr]
          [(* ._) #'u8*]
          [rktio_bool_t #'boolean]
          [else t]))

  (define-ftype intptr_t iptr)
  (define-ftype uintptr_t uptr)
  (define-ftype rktio_int64_t integer-64)
  (define _uintptr _uint64)
  
  (define-syntax (define-type stx)
    (syntax-case stx ()
      [(_ type old-type)
       (with-syntax ([old-type (convert-type #'old-type)])
         #'(define-ftype type old-type))]))

  (define-syntax (define-struct-type stx)
    (syntax-case stx ()
      [(_ type ([old-type field] ...))
       (with-syntax ([(old-type ...) (map convert-type #'(old-type ...))])
         #'(define-ftype type (struct [field old-type] ...)))]))

  (meta define (convert-function stx)
        (syntax-case stx ()
          [(_ ret-type name ([arg-type arg-name] ...))
           (with-syntax ([ret-type (convert-type #'ret-type)]
                         [(arg-type ...) (map convert-type #'(arg-type ...))])
             #'(foreign-procedure (rktio-lookup 'name)
                                  (arg-type ...)
                                  ret-type))]))

  (define-syntax (define-function stx)
    (syntax-case stx ()
      [(_ _ name . _)
       (with-syntax ([rhs (convert-function stx)])
         #'(define name rhs))]))

  (define-syntax (define-function/errno stx)
    (syntax-case stx ()
      [(_ ret-type name ([_ arg] ...))
       (with-syntax ([rhs (convert-function stx)])
         #'(define name
             (let ([proc rhs])
               (lambda (who ok-result? arg ...)
                 (let ([v (proc arg ...)])
                   (if (ok-result? v)
                       v
                       (error who "failed")))))))]))

  (define-syntax (define-function/errno+step stx)
    (syntax-case stx ()
      [(_ _ name . _)
       (with-syntax ([rhs (convert-function stx)])
         #'(define name rhs))]))
  
  (define (rktio-lookup name)
    (load-shared-object "../../lib/librktio.dylib")
    (foreign-entry (symbol->string name)))

  (define (<< a b) (bitwise-arithmetic-shift-left a b))

  (include "../build/so-rktio/rktio.rktl")

  (define (ok-pointer? v) (not (zero? v)))
  (define (ok? v) v)
  (define (ok-not? bad) (lambda (v) (not (eqv? bad v))))

  (define (cast v from to)
    (let ([p (malloc from)])
      (ptr-set! p from v)
      (ptr-ref p to)))

  (define rktio (rktio_init 'init ok-pointer?))
  
  ;; ----------------------------------------

  (define string-locale-downcase string-downcase)

  (define (char-blank? v) (char-whitespace? v))
  (define (char-graphic? v) #t)
  
  (define-syntax with-exn:fail:filesystem
    (syntax-rules ()
      [(_ expr)
       (guard
        (x [else (convert-exception-to-exn:fail:filesystem x)])
        expr)]))

  (define (convert-exception-to-exn:fail:filesystem v)
    (raise
     (|#%app|
      (if (i/o-file-already-exists-error? v)
          exn:fail:filesystem:exists
          exn:fail:filesystem)
      (string-append
       (if (who-condition? v)
           (chez:format "~a: " (condition-who v))
           "")
       (cond
        [(format-condition? v)
         (apply chez:format
                (condition-message v)
                (condition-irritants v))]
        [(message-condition? v)
         (condition-message v)]
        [else
         (chez:format "~s" v)]))
      (current-continuation-marks))))

  (define file-stream-buffer-mode
    (case-lambda
     [(p)
      (if (chez:input-port? p)
          (if (zero? (binary-port-input-size p))
              'none
              'block)
          (if (zero? (binary-port-output-size p))
              'none
              'block))]
     [(p mode)
      (if (chez:input-port? p)
          (set-binary-port-input-buffer! p (if (eq? mode 'block)
                                               (make-bytevector 4096)
                                               (make-bytevector 0)))
          (set-binary-port-output-buffer! p (if (eq? mode 'block)
                                                (make-bytevector 4096)
                                                (make-bytevector 0))))]))
  (define peek-byte lookahead-u8)

  ;; Host's notion of path is a byte string, but
  ;; we have to add a nul terminator
  (define (bytes->path bstr)
    (bytes-append bstr '#vu8(0)))
  (define (path->bytes p)
    p)

  (define (system-path-convention-type) 'unix)

  (define (directory-exists? p)
    (rktio_directory_exists rktio p))
  
  (define (file-exists? p)
    (rktio_file_exists rktio p))
  
  (define (link-exists? p)
    (rktio_link_exists rktio p))

  (define/who (directory-list p)
    (let ([dl (rktio_directory_list_start who ok-pointer? rktio p)])
      (let loop ()
        (let ([n (rktio_directory_list_step who ok-pointer? rktio dl)])
          (if (zero? (foreign-ref 'unsigned-8 n 0))
              null
              (cons
               (let ([bstr (cast n _uintptr _bytes)])
                 (rktio_free n)
                 bstr)
               (loop)))))))

  (define/who (make-directory p)
    (rktio_make_directory who ok? rktio p))

  (define/who (delete-file p)
    (rktio_delete_file who ok? rktio p 0))
  
  (define/who (delete-directory p)
    (rktio_delete_directory who ok? rktio p p 0))
  
  (define/who file-or-directory-modify-seconds
    (case-lambda
     [(p)
      (let* ([p (rktio_get_file_modify_seconds who ok-pointer? rktio p)]
             [v (foreign-ref 'iptr p 0)])
        (rktio_free p)
        v)]
     [(p secs)
      (if secs
          (rktio_set_file_modify_seconds who ok? rktio p secs)
          (file-or-directory-modify-seconds p))]
     [(p secs fail)
      (if secs
          (file-or-directory-modify-seconds p secs)
          (let* ([p (rktio_get_file_modify_seconds who (lambda (x) x) rktio p)])
            (if (zero? p)
                (fail)
                (let ([v (foreign-ref 'iptr p 0)])
                  (rktio_free p)
                  v))))]))

  (define (file-or-directory-permissions path mode)
    (cond
     [(eq? 'bits mode) (get-mode path #f)]
     [(not mode)
      (let ([bits (get-mode path #f)])
        (append
         (if (zero? (bitwise-and #o100  bits)) '() '(read))
         (if (zero? (bitwise-and #o200  bits)) '() '(write))
         (if (zero? (bitwise-and #o400  bits)) '() '(execute))))]
     [else
      (chmod path mod)]))

  (define (rename-file-or-directory old-pathname new-pathname exists-ok?)
    (with-exn:fail:filesystem
     (rename-file old-pathname new-pathname)))

  (define file-identities (make-hashtable equal-hash-code equal?))
  (define (file-or-directory-identity p as-link?)
    (let ([key (1/path->string (1/simplify-path (1/path->complete-path p)))])
      (let ([n (hashtable-ref file-identities key #f)])
        (or n
            (let ([n (hashtable-size file-identities)])
              (hashtable-set! file-identities key n)
              n)))))

  (define (file-size p)
    (1/error 'file-size "not yet supported"))

  (define (copy-file src dest exists-ok?)
    (1/error 'copy-file "not yet supported"))

  (define (make-file-or-directory-link to path)
    (1/error 'make-file-or-directory-link "not yet supported"))

  (define (filesystem-root-list)
    (1/error 'filesystem-root-list "not yet supported"))
  
  (define (resolve-path p) p)

  (define/who (current-input-port)
    (rktio_std_fd who ok-pointer? rktio RKTIO_STDIN))
  (define/who (current-output-port)
    (rktio_std_fd who ok-pointer? rktio RKTIO_STDOUT))
  (define/who (current-error-port)
    (rktio_std_fd who ok-pointer? rktio RKTIO_STDERR))
  
  (define/who open-input-file
    (case-lambda
     [(path mode mode2)
      (rktio_open who ok-pointer? rktio path RKTIO_OPEN_READ)]
     [(path mode)
      (open-input-file path mode #f)]
     [(path)
      (open-input-file path #f #f)]))

  (define/who open-output-file
    (case-lambda
     [(path) (open-output-file path #f #f)]
     [(path mode) (open-output-file path mode #f)]
     [(path mode mode2)
      (let ([mode? (lambda (s) (or (eq? mode s) (eq? mode2 s)))])
        (rktio_open who ok-pointer? rktio path
                    (+ RKTIO_OPEN_WRITE
                       (cond
                        [(mode? 'truncate) (+ RKTIO_OPEN_TRUNCATE RKTIO_OPEN_CAN_EXIST)]
                        [(mode? 'must-truncate) (+ RKTIO_OPEN_TRUNCATE RKTIO_OPEN_MUST_EXIST)]
                        [(mode? 'update) RKTIO_OPEN_MUST_EXIST]
                        [(mode? 'can-update) RKTIO_OPEN_CAN_EXIST]
                        [(mode? 'replace) (+ RKTIO_OPEN_TRUNCATE RKTIO_OPEN_CAN_EXIST)]
                        [(mode? 'truncate/replace) (+ RKTIO_OPEN_TRUNCATE RKTIO_OPEN_CAN_EXIST)]
                        [(mode? 'append) RKTIO_OPEN_APPEND]
                        [else 0]))))]))

  (define/who (close-input-port in)
    (rktio_close who ok? rktio in))

  (define/who (close-output-port out)
    (rktio_close who ok? rktio out))

  (define/who (read-byte in)
    (let ([bstr (make-bytes 1)])
      (let ([v (rktio_read who (ok-not? RKTIO_READ_ERROR) rktio in bstr 1)])
        (cond
         [(eqv? v RKTIO_READ_EOF) eof]
         [(eqv? v 0) (error 'read-byte "we'd have to spin")]
         [else (bytes-ref bstr 0)]))))
  
  (define/who (read-bytes-avail!* bstr in start-pos end-pos)
    (let ([to-bstr (if (zero? start-pos)
                       bstr
                       (make-bytes (- end-pos start-pos)))])
      (let ([v (rktio_read who (ok-not? RKTIO_READ_ERROR) rktio in to-bstr (- end-pos start-pos))])
        (cond
         [(eqv? v RKTIO_READ_EOF) eof]
         [(eq? bstr to-bstr) v]
         [else
          (bytes-copy! bstr start-pos to-bstr 0 v)
          v]))))
  
  (define/who (write-bytes-avail* bstr out start-pos end-pos)
    (let ([from-bstr (if (zero? start-pos)
                         bstr
                         (let ([new-bstr (make-bytes (- end-pos start-pos))])
                           (bytes-copy! new-bstr 0 bstr start-pos (- end-pos start-pos))
                           new-bstr))])
      (let ([v (rktio_write who (ok-not? RKTIO_WRITE_ERROR) rktio out from-bstr (- end-pos start-pos))])
        v)))
  
  (define (write-bytes bstr out)
    (let ([len (bytes-length bstr)])
      (let loop ([i 0])
        (let ([v (write-bytes-avail* bstr out i (- len i))])
          (let ([i (+ i v)])
            (if (fx= i len)
                len
                (loop i)))))))

  (define file-truncate truncate-file)

  (define (primitive-table key)
    (case key
      [(|#%evt|) |#%evt-instance|]
      [else #f]))

  (define (terminal-port? p) #f)

  (include "compiled/io.scm")
  
  ;; Initialize:
  (|#%app| 1/current-directory (current-directory)))
