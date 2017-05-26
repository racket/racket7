
(define (udp? v) #f)
(define (tcp-listener? v) #f)
(define (tcp-port? v) #f)

(define-syntax (define-network-ids stx)
  (syntax-case stx ()
    [(_ id ...)
     #'(begin
         (define (id v)
           (raise-unsupported-error 'id))
         ...)]))

(define-network-ids
  tcp-abandon-port
  tcp-accept
  tcp-accept-evt
  tcp-accept-ready?
  tcp-accept/enable-break
  tcp-addresses
  tcp-close
  tcp-connect
  tcp-connect/enable-break
  tcp-listen
  udp-bind!
  udp-bound?
  udp-close
  udp-connect!
  udp-connected?
  udp-multicast-interface
  udp-multicast-join-group!
  udp-multicast-leave-group!
  udp-multicast-loopback?
  udp-multicast-set-interface!
  udp-multicast-set-loopback!
  udp-multicast-set-ttl!
  udp-multicast-ttl
  udp-open-socket
  udp-receive!
  udp-receive!*
  udp-receive!-evt
  udp-receive!/enable-break
  udp-receive-ready-evt
  udp-send
  udp-send*
  udp-send-evt
  udp-send-ready-evt
  udp-send-to
  udp-send-to*
  udp-send-to-evt
  udp-send-to/enable-break
  udp-send/enable-break)
