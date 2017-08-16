#lang racket/base
(require "../common/check.rkt"
         "../string/convert.rkt"
         "../string/integer.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../port/close.rkt"
         "../port/fd-port.rkt"
         "tcp-port.rkt"
         "tcp-listen.rkt")

(provide tcp-addresses)

(define/who (tcp-addresses p [port-numbers? #f])
  (check who (lambda (p) (or (tcp-port? p) (tcp-listener? p)))
         #:contract "(or/c tcp-port? tcp-listener? udp?)"
         p)
  (start-atomic)
  (define-values (local-address peer-address)
    (cond
      [(tcp-listener? p)
       (cond
         [(tcp-listener-closed? p)
          (end-atomic)
          (raise-arguments-error who
                                 "listener is closed"
                                 "listener" p)]
         [else
          (values (rktio_listener_address rktio (tcp-listener-lnr p))
                  #f)])]
      [else
       (cond
         [(port-closed? p)
          (end-atomic)
          (raise-arguments-error who
                                 "port is closed"
                                 "port" p)]
         [else
          (define fd (fd-port-fd p))
          (values (rktio_socket_address rktio fd)
                  (rktio_socket_peer_address rktio fd))])]))
  (define local-address-bytes (rktio_to_bytes_list local-address 2))
  (define peer-address-bytes (and peer-address (rktio_to_bytes_list peer-address 2)))
  (end-atomic)

  (define (convert bstr) (bytes->string/utf-8 bstr #\?))
  (define local-hostname (convert (car local-address-bytes)))
  (define peer-hostname (if peer-address-bytes
                            (convert (car peer-address-bytes))
                            "0.0.0.0"))

  (cond
    [port-numbers?
     (values local-hostname
             (string->integer (convert (cadr local-address-bytes)))
             peer-hostname
             (if peer-address-bytes
                 (string->integer (convert (cadr peer-address-bytes)))
                 0))]
    [else
     (values local-hostname peer-hostname)]))
