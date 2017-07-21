(library (io)
  (export)
  (import (except (chezpart)
                  close-port)
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

  (module (|#%rktio-instance|)
    (meta define (convert-type t)
          (syntax-case t (ref * rktio_bool_t)
            [(ref . _) #'uptr]
            [(*ref ._) #'u8*]
            [rktio_bool_t #'boolean]
            [else t]))

    (define-ftype intptr_t iptr)
    (define-ftype uintptr_t uptr)
    (define-ftype rktio_int64_t integer-64)
    (define _uintptr _uint64)
    (define NULL 0)

    (define (<< a b) (bitwise-arithmetic-shift-left a b))

    (define-syntax define-constant
      (syntax-rules ()
        [(_ id expr) (define id expr)]))
    
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

    (define-syntax (define-function*/errno stx)
      (syntax-case stx ()
        [(_ err-val err-expr ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (with-syntax ([rhs (convert-function
                             #'(define-function ret-type name ([rktio-type rktio] [arg-type arg] ...)))])
           #'(define name
               (let ([proc rhs])
                 (lambda (rktio arg ...)
                   (let ([v (proc rktio arg ...)])
                     (if (eqv? v err-val)
                         err-expr
                         v))))))]))

    (define-syntax define-function/errno
      (syntax-rules ()
        [(_ err-val ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (define-function*/errno err-val
           (vector (rktio_get_last_error_kind rktio)
                   (rktio_get_last_error rktio))
           ret-type name ([rktio-type rktio] [arg-type arg] ...))]))
    
    (define-syntax define-function/errno+step
      (syntax-rules ()
        [(_ err-val ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (define-function*/errno err-val
           (vector (rktio_get_last_error_kind rktio)
                   (rktio_get_last_error rktio)
                   (rktio_get_last_error_step rktio))
           ret-type name ([rktio-type rktio] [arg-type arg] ...))]))

    (define loaded-librktio
      (load-shared-object (string-append "../../lib/librktio"
                                         (utf8->string (system-type 'so-suffix)))))

    (define (rktio-lookup name)
      (foreign-entry (symbol->string name)))

    (include "../io/compiled/rktio.rktl")

    (define (rktio_filesize_ref fs)
      (ftype-ref rktio_filesize_t () (make-ftype-pointer rktio_filesize_t fs) 0))
    (define (rktio_timestamp_ref fs)
      (ftype-ref rktio_timestamp_t () (make-ftype-pointer rktio_timestamp_t fs) 0))
    (define (rktio_is_timestamp v)
      (let ([radix (arithmetic-shift 1 (sub1 (* 8 (ftype-sizeof rktio_timestamp_t))))])
        (<= (- radix) v (sub1 radix))))

    (define (rktio_identity_to_vector p)
      (let ([p (make-ftype-pointer rktio_identity_t p)])
        (vector
         (ftype-ref rktio_identity_t (a) p 0)
         (ftype-ref rktio_identity_t (b) p 0)
         (ftype-ref rktio_identity_t (c) p 0)
         (ftype-ref rktio_identity_t (a_bits) p 0)
         (ftype-ref rktio_identity_t (b_bits) p 0)
         (ftype-ref rktio_identity_t (c_bits) p 0))))
    
    (define (rktio_convert_result_to_vector p)
      (let ([p (make-ftype-pointer rktio_convert_result_t p)])
        (vector
         (ftype-ref rktio_convert_result_t (in_consumed) p 0)
         (ftype-ref rktio_convert_result_t (out_produced) p 0)
         (ftype-ref rktio_convert_result_t (converted) p 0))))

      (define (cast v from to)
        (let ([p (malloc from)])
          (ptr-set! p from v)
          (ptr-ref p to)))

    (define (rktio_to_bytes fs)
      (cast fs _uintptr _bytes))

    (define (rktio_to_shorts fs)
      (cast fs _uintptr _short_bytes))

    ;; Unlike `rktio_to_bytes`, frees the array and strings
    (define (rktio_to_bytes_list lls)
      (begin0
       (let loop ([i 0])
         (define bs (ptr-ref lls _bytes i))
         (if bs
             (cons (begin0
                    (cast bs _uintptr _bytes)
                    (rktio_free bs))
                   (loop (add1 i)))
             null))
       (rktio_free lls)))

    (define (rktio_do_install_os_signal_handler rktio)
      (rktio_install_os_signal_handler rktio))

    (define (rktio_get_ctl_c_handler)
      (get-ctl-c-handler))

    (define |#%rktio-instance|
      (let ()
        (define-syntax extract-functions
          (syntax-rules (define-constant
                          define-type
                          define-struct-type
                          define-function
                          define-function/errno
                          define-function/errno+step)
            [(_ accum) (hasheq . accum)]
            [(_ accum (define-constant . _) . rest)
             (extract-functions accum . rest)]
            [(_ accum (define-type . _) . rest)
             (extract-functions accum . rest)]
            [(_ accum (define-struct-type . _) . rest)
             (extract-functions accum . rest)]
            [(_ accum (define-function _ id . _) . rest)
             (extract-functions ('id id . accum) . rest)]
            [(_ accum (define-function/errno _ _ id . _) . rest)
             (extract-functions ('id id . accum) . rest)]
            [(_ accum (define-function/errno+step _ _ id . _) . rest)
             (extract-functions ('id id . accum) . rest)]))
        (define-syntax begin
          (syntax-rules ()
            [(begin form ...)
             (extract-functions ['rktio_NULL
                                 NULL
                                 'rktio_filesize_ref rktio_filesize_ref
                                 'rktio_timestamp_ref rktio_timestamp_ref
                                 'rktio_is_timestamp rktio_is_timestamp
                                 'rktio_identity_to_vector rktio_identity_to_vector
                                 'rktio_convert_result_to_vector rktio_convert_result_to_vector
                                 'rktio_to_bytes rktio_to_bytes
                                 'rktio_to_bytes_list rktio_to_bytes_list
                                 'rktio_to_shorts rktio_to_shorts
                                 'rktio_do_install_os_signal_handler rktio_do_install_os_signal_handler
                                 'rktio_get_ctl_c_handler rktio_get_ctl_c_handler]
                                form ...)]))
        (include "../io/compiled/rktio.rktl"))))
  
  ;; ----------------------------------------

  (define string-locale-downcase string-downcase)

  (define (char-blank? v) (char-whitespace? v))
  (define (char-graphic? v) #t)

  (define (system-path-convention-type) 'unix)

  (define (primitive-table key)
    (case key
      [(|#%thread|) |#%thread-instance|]
      [(|#%rktio|) |#%rktio-instance|]
      [else #f]))

  (define (terminal-port? p) #f)

  (include "compiled/io.scm")
  
  ;; Initialize:
  (|#%app| 1/current-directory (current-directory)))
