#lang racket/base

;;; For reading/writing cache files.

(require
  (only-in racket/file file->value)
  racket/serialize)

;;; If #f, do not cache anything
(define *CACHE?* (make-parameter #t))

;;; Print a warning when reading from a cache raises an exception.
;;; This could raise an exception too.
(define ((cache-read-error cache-file) exn)
  (printf "[WARNING] Failed to read cachefile '~a', got exception:\n~a" cache-file (exn-message exn))
  #f)

;;; (: with-cache (All (A B) (->* [Path-String (-> A)] [#:read (-> B (Option A)) #:write (-> A B)] A)))
(define (with-cache cache-file thunk #:read [read-proc #f] #:write [write-proc #f])
  (let ([read-proc (or read-proc values)]
        [write-proc (or write-proc values)])
    ;; Read from a cache, or recompute the data
    (or (and (*CACHE?*)
             (file-exists? cache-file)
             (let ([v (with-handlers ([exn:fail? (cache-read-error cache-file)])
                        (read-proc (file->value cache-file)))])
               (and v
                    (printf "[INFO] reading cachefile '~a'" cache-file)
                    v)))
        (let ([r (thunk)]) ; Probably an expensive thunk
          (printf "[INFO] writing cachefile '~a'" cache-file)
          (with-output-to-file cache-file #:exists 'replace
            (lambda () (writeln (write-proc r))))
          r))))

;; =============================================================================
;; A few uses of `with-cache`
;
;(define (render-table render-proc)
;  (with-cache cache-file
;   #:read uncache-table
;   #:write cache-table
;   render-proc)
;
;(define (render-data-lattice bm v #:tag [tag "*"])
;  (with-cache (lattice-cache-file bm v tag)
;    #:read deserialize
;    #:write serialize
;    (lambda () (file->performance-lattice (data-path bm v tag)))))
;
;(define (render-lnm-table)
;  (with-cache (lnm-table-cache)
;   #:read (lambda (tag+data)
;            (let ([d (uncache-table tag+data)])
;              (and d (deserialize d))))
;   #:write (compose1 cache-table serialize)
;   new-lnm-bars))
;
;;; -----------------------------------------------------------------------------
;;; Functions to use in #:read and #:write
;
;(define BENCHMARK-NAMES '(a b c))
;
;(define (cache-table T)
;  (cons BENCHMARK-NAMES T))
;
;(define (uncache-table tag+data)
;  (and (equal? (car tag+data) BENCHMARK-NAMES)
;       (cdr tag+data)))
