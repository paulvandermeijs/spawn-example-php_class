(require-builtin steel/hash)

; Add namespace to the context from composer.json autoload config
(define (context)
  (let* ((composer-path (find-composer (current-directory))))
    (if composer-path
      (let* ((composer-content (read-port-to-string (open-input-file composer-path)))
             (composer-data (string->jsexpr composer-content))
             (autoload (hash-try-get composer-data 'autoload))
             (psr4 (and autoload (hash-try-get autoload 'psr-4)))
             (psr0 (and autoload (hash-try-get autoload 'psr-0)))
             (composer-dir (parent-name composer-path))
             (current-dir (current-directory))
             (namespace (or (and psr4 (get-namespace psr4 composer-dir current-dir))
                         (and psr0 (get-namespace psr0 composer-dir current-dir))
                         "")))
        (hash 'namespace namespace))
      (hash 'namespace #f))))

; Search parents of given path for a Composer file
(define (find-composer path)
  (if (string=? path "")
    #f
    (let ((composer-path (string-append path "/composer.json")))
      (if (is-file? composer-path)
        composer-path
        (find-composer (parent-name path))))))

; Compute relative path from base to target
(define (relative-path base target)
  (if (starts-with? target base)
    (substring target (string-length base))
    ""))

; Convert file path to namespace format converting "/" to "\\"
(define (nsify path)
  (string-replace path "/" "\\"))

; Find first matching base directory from a list of bases
(define (find-match bases composer-dir current-dir)
  (if (null? bases)
    #f
    (let ((base (car bases)))
      (if (starts-with? current-dir (string-append composer-dir "/" base))
        base
        (find-match (cdr bases) composer-dir current-dir)))))

; Compute the namespace based on psr4 mappings, composer.json directory, and current directory.
(define (get-namespace mappings composer-dir current-dir)
  (let loop ((keys (hash-keys->list mappings)))
    (if (null? keys)
      #f
      (let* ((prefix (car keys))
             (base-dirs (hash-ref mappings prefix))
             (base-dirs (if (string? base-dirs) (list base-dirs) base-dirs))
             (matched (find-match base-dirs composer-dir current-dir)))
        (if matched
          (let* ((full-base (string-append composer-dir "/" matched))
                 (rel (relative-path full-base current-dir)))
            (if (string=? rel "")
              prefix
              (let* ((namespace (nsify (string-append "\\" rel)))
                     (namespace (string-append (symbol->string prefix) namespace))
                     (namespace (trim-start-matches namespace "\\")))
                namespace)))
          (loop (cdr keys)))))))
