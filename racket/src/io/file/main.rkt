#lang racket/base
(require "../common/check.rkt"
         "../path/path.rkt"
         "host.rkt"
         (only-in racket/base
                  [directory-exists? host:directory-exists?]
                  [file-exists? host:file-exists?]
                  [link-exists? host:link-exists?]
                  [make-directory host:make-directory]
                  [delete-file host:delete-file]
                  [delete-directory host:delete-directory]
                  [rename-file-or-directory host:rename-file-or-directory]
                  [file-or-directory-modify-seconds host:file-or-directory-modify-seconds]
                  [file-or-directory-permissions host:file-or-directory-permissions]
                  [file-or-directory-identity host:file-or-directory-identity]
                  [file-size host:file-size]
                  [copy-file host:copy-file]
                  [make-file-or-directory-link host:make-file-or-directory-link]
                  [resolve-path host:resolve-path]
                  [filesystem-root-list host:filesystem-root-list])
         (only-in '#%kernel
                  [directory-list host:directory-list]))

(provide directory-exists?
         file-exists?
         link-exists?
         make-directory
         directory-list
         current-force-delete-permissions
         delete-file
         delete-directory
         rename-file-or-directory
         file-or-directory-modify-seconds
         file-or-directory-permissions
         file-or-directory-identity
         file-size
         copy-file
         make-file-or-directory-link
         resolve-path
         expand-user-path
         filesystem-root-list)

(define/who (directory-exists? p)
  (check who path-string? p)
  (host:directory-exists? (->host p)))

(define/who (file-exists? p)
  (check who path-string? p)
  (host:file-exists? (->host p)))

(define/who (link-exists? p)
  (check who path-string? p)
  (host:link-exists? (->host p)))

(define/who (make-directory p)
  (check who path-string? p)
  (host:make-directory (->host p)))

(define/who (directory-list p)
  (check who path-string? p)
  (for/list ([p (in-list (host:directory-list (->host p)))])
    (host-> p)))

(define current-force-delete-permissions
  (make-parameter #t (lambda (v) (and v #t))))

(define/who (delete-file p)
  (check who path-string? p)
  (host:delete-file (->host p)))

(define/who (delete-directory p)
  (check who path-string? p)
  (host:delete-directory (->host p)))

(define/who (rename-file-or-directory old new [exists-ok? #f])
  (check who path-string? old)
  (check who path-string? new)
  (host:rename-file-or-directory (->host old) (->host new) exists-ok?))

(define/who file-or-directory-modify-seconds
  (case-lambda
    [(p)
     (check who path-string? p)
     (host:file-or-directory-modify-seconds (->host p))]
    [(p secs)
     (check who path-string? p)
     (check who exact-integer? secs)
     (host:file-or-directory-modify-seconds (->host p) secs)]
    [(p secs fail)
     (check who path-string? p)
     (check who #:or-false exact-integer? secs)
     (check who (procedure-arity-includes/c 0) fail)
     (host:file-or-directory-modify-seconds (->host p) secs fail)]))

(define/who (file-or-directory-permissions p [mode #f])
  (check who path-string? p)
  (check who (lambda (m)
               (or (not m)
                   (eq? m 'bits)
                   (and (exact-integer? m)
                        (<= 0 m 65535))))
         #:contract "(or/c #f 'bits (integer-in 0 65535))"
         mode)
  (host:file-or-directory-permissions (->host p) mode))

(define/who (file-or-directory-identity p [as-link? #f])
  (check who path-string? p)
  (host:file-or-directory-identity p as-link?))

(define/who (file-size p)
  (check who path-string? p)
  (host:file-size p))

(define/who (copy-file src dest [exists-ok? #f])
  (check who path-string? src)
  (check who path-string? src)
  (host:copy-file (->host src) (->host dest) exists-ok?))

(define/who (make-file-or-directory-link to path)
  (check who path-string? to)
  (check who path-string? path)
  (host:make-file-or-directory-link (->host/as-is to) (->host path)))

(define/who (resolve-path p)
  (check who path-string? p)
  (host-> (host:resolve-path (->host p))))

(define (expand-user-path p) p)

(define (filesystem-root-list)
  (for/list ([p (in-list (host:filesystem-root-list))])
    (host-> p)))
