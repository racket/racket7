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

(define (directory-exists? p)
  (check 'directory-exists? path-string? p)
  (host:directory-exists? (->host p)))

(define (file-exists? p)
  (check 'file-exists? path-string? p)
  (host:file-exists? (->host p)))

(define (link-exists? p)
  (check 'link-exists? path-string? p)
  (host:link-exists? (->host p)))

(define (make-directory p)
  (check 'maked-irectory path-string? p)
  (host:make-directory (->host p)))

(define (directory-list p)
  (check 'directory-list path-string? p)
  (for/list ([p (in-list (host:directory-list (->host p)))])
    (host-> p)))

(define current-force-delete-permissions
  (make-parameter #t (lambda (v) (and v #t))))

(define (delete-file p)
  (check 'delete-file path-string? p)
  (host:delete-file (->host p)))

(define (delete-directory p)
  (check 'delete-directory path-string? p)
  (host:delete-directory (->host p)))

(define (rename-file-or-directory old new [exists-ok? #f])
  (check 'rename-file-or-directory path-string? old)
  (check 'rename-file-or-directory path-string? new)
  (host:rename-file-or-directory (->host old) (->host new) exists-ok?))

(define file-or-directory-modify-seconds
  (case-lambda
    [(p)
     (check 'file-or-directory-modify-seconds path-string? p)
     (host:file-or-directory-modify-seconds (->host p))]
    [(p secs)
     (check 'file-or-directory-modify-seconds path-string? p)
     (check 'file-or-directory-modify-seconds exact-integer? secs)
     (host:file-or-directory-modify-seconds (->host p) secs)]
    [(p secs fail)
     (check 'file-or-directory-modify-seconds path-string? p)
     (check 'file-or-directory-modify-seconds (lambda (n) (or (not n) (exact-integer? n)))
            #:contract "(or/c #f exact-integer?)"
            secs)
     (check 'file-or-directory-modify-seconds (lambda (p)
                                                (and (procedure? p)
                                                     (procedure-arity-includes? p 0)))
            #:contract "(procedure-arity-includes/c 0)"
            fail)
     (host:file-or-directory-modify-seconds (->host p) secs fail)]))

(define (file-or-directory-permissions p [mode #f])
  (check 'file-or-directory-modify-seconds path-string? p)
  (check 'file-or-directory-modify-seconds (lambda (m)
                                             (or (not m)
                                                 (eq? m 'bits)
                                                 (and (exact-integer? m)
                                                      (<= 0 m 65535))))
         #:contract "(or/c #f 'bits (integer-in 0 65535))"
         mode)
  (host:file-or-directory-permissions (->host p) mode))

(define (file-or-directory-identity p [as-link? #f])
  (check 'file-or-directory-identity path-string? p)
  (host:file-or-directory-identity p as-link?))

(define (file-size p)
  (check 'file-size path-string? p)
  (host:file-size p))

(define (copy-file src dest [exists-ok? #f])
  (check 'copy-file path-string? src)
  (check 'copy-file path-string? src)
  (host:copy-file (->host src) (->host dest) exists-ok?))

(define (make-file-or-directory-link to path)
  (check 'make-file-or-directory-link path-string? to)
  (check 'make-file-or-directory-link path-string? path)
  (host:make-file-or-directory-link (->host/as-is to) (->host path)))

(define (resolve-path p)
  (check 'resolve-path path-string? p)
  (host-> (host:resolve-path (->host p))))

(define (expand-user-path p) p)

(define (filesystem-root-list)
  (for/list ([p (in-list (host:filesystem-root-list))])
    (host-> p)))
