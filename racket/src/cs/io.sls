(library (io)
  (export)
  (import (chezpart)
          (rename (only (chezscheme)
                        read-char peek-char
                        standard-input-port standard-output-port standard-error-port
                        close-input-port close-output-port
                        current-directory
                        format
                        error)
                  [standard-input-port current-input-port]
                  [standard-output-port current-output-port]
                  [standard-error-port current-error-port])
          (core)
          (thread))
  ;; Tie knots:
  (define (path? v) (is-path? v))
  (define (path->string v) (1/path->string v))
  (define path->complete-path
    (case-lambda
     [(v) (1/path->complete-path v)]
     [(v wrt) (1/path->complete-path v wrt)]))
  (define (absolute-path? v) (1/absolute-path? v))
  (define (relative-path? v) (1/relative-path? v))

  (define string-locale-downcase string-downcase)

  (define (char-blank? v) (char-whitespace? v))
  (define (char-graphic? v) #t)
  
  (define (read-byte in) (get-u8 in))
  (define (read-bytes-avail!* bstr in start-pos end-pos)
    (if (input-port-ready? in)
        (get-bytevector-some! in bstr start-pos (- end-pos start-pos))
        0))
  (define (write-bytes-avail* bstr out start-pos end-pos)
    (define len (- end-pos start-pos))
    (put-bytevector out bstr start-pos len)
    (flush-output-port out)
    len)
  (define peek-byte lookahead-u8)
  (define (->string p)
    (if (1/path? p) (1/path->string p) p))
  (define (open-input-file path mode mode2)
    (open-file-input-port (->string path)))
  (define (open-output-file path mode mode2)
    (open-file-output-port (->string path)))
  (define (directory-exists? p)
    (file-directory? (->string p)))
  (define (resolve-path p) p)
  (define (system-path-convention-type) 'unix)

  (include "compiled/io.scm")

  ;; Initialize:
  (1/current-directory (current-directory)))
