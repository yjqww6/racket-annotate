#lang racket/base
(require (for-syntax racket/base syntax/parse racket/syntax
                     racket/sequence racket/stxparam-exptime
                     syntax/parse/lib/function-header)
         racket/stxparam racket/performance-hint)

(provide define-implicit-key define-implicit with-implicit)

(begin-for-syntax
  (struct paramed (param kw)
    #:property prop:procedure
    (λ (s stx)
      (syntax-case stx ()
        [id
         (identifier? #'id)
         (let ([v (syntax-parameter-value (paramed-param s))])
           (cond
             [(syntax-e v) v]
             [else (raise-syntax-error (syntax-e #'id)
                                       "unbound here" stx)]))]))))

(define-syntax (define-implicit-key stx)
  (syntax-parse stx
    [(_ name:id (~optional k:keyword #:defaults ([k (id->key #'name)])))
     #:with param (generate-temporary 'param)
     #`(begin
         (define-syntax-parameter param #'#f)
         (define-syntax name
           (paramed #'param 'k)))]))

(begin-for-syntax
  (define (id->key id)
    (datum->syntax #f (string->keyword (symbol->string (syntax-e id)))))
  
  (define-syntax-class imp
    (pattern x:id
             #:attr value (syntax-local-value #'x)
             #:with param (paramed-param (attribute value))
             #:with key (paramed-kw (attribute value))
             #:with def (syntax-parameter-value #'param)
             #:with temp (generate-temporary #'x)
             #:with bind (if (syntax-e #'def)
                             #'[temp def]
                             #'temp)))

  (define (fix-actuals args h stx)
    (let loop ([args args] [h h])
      (syntax-parse args
        [()
         #:with ((k:keyword . v:expr) ...)
         (for/list ([(k v) (in-hash h)])
           (cons k v))
         #'((~@ k v) ...)]
        [(x:expr . rest)
         #`(x . #,(loop #'rest h))]
        [(k:keyword x:expr . rest)
         #`(k x . #,(loop #'rest (hash-remove h (syntax-e #'k))))])))

  (define (->hash k v)
    (for/hash ([k (in-syntax k)]
               [v (in-syntax v)])
      (values (syntax-e k) v)))

  (define (build-call name fmls)
    (let loop ([formals fmls] [ctx values])
      (syntax-parse formals
        [()
         #`(#,name . #,(ctx #'()))]
        [rest:id
         #`(apply #,name . #,(ctx #'(rest)))]
        [((~or x:id [x:id _:expr]) . rest)
         (loop #'rest (λ (r) (ctx #`(x . #,r))))]
        [(k:keyword (~or x:id [x:id _:expr]) . rest)
         (loop #'rest (λ (r) (ctx #`(k x . #,r))))])))
  )

(define-syntax (define-implicit stx)
  (syntax-parse stx
    [(_ (name . args:formals) (imp:imp ...) body:expr ...+)
     #:with new-args:formals #'((~@ imp.key imp.bind) ...
                                . args)
     #:with (aux-name flat-name) (generate-temporaries (list #'name #'flat))
     #:with (new-params ...) #'new-args.params
     (syntax-property
      #'(begin
          (define (flat-name new-params ...)
            (syntax-parameterize ([imp.param #'imp.temp] ...)
              body ...))
          (define-inline (aux-name . new-args)
            (flat-name new-params ...))
          (define-proxy name aux-name args (imp ...)))
      'disappeared-use
      (map syntax-local-introduce (syntax->list #'(imp ...))))]))

(define-syntax (define-proxy stx)
  (syntax-parse stx
    [(_ name:id aux-name:id fmls (imp ...))
     #:with imps #'(list #'imp ...)
     #'(...
        (define-syntax (name stx)
          (syntax-parse stx
            [(_ args ...)
             #:with (imp-id:imp ...) imps
             #:with actuals
             (fix-actuals #'(args ...)
                          (->hash #'(imp-id.key ...)
                                  #'(imp-id ...))
                          stx)
             #'(aux-name . actuals)
             ]
            [_
             #:when (identifier? stx)
             #:with (imp-id:imp ...) imps
             #:with args #'((~@ imp-id.key imp-id.bind) ...
                            . fmls)
             #:with call (build-call #'aux-name #'args)
             #'(λ args
                 (syntax-parameterize ([imp-id.param #'imp-id.temp] ...)
                   call))])))]))

(define-syntax (with-implicit stx)
  (syntax-parse stx
    [(_ ([imp:imp e:expr] ...) body:expr ...+)
     (syntax-property
      #'(let ([imp.temp e] ...)
          (syntax-parameterize ([imp.param #'imp.temp] ...)
            body ...))
      'disappeared-use
      (map syntax-local-introduce (syntax->list #'(imp ...))))]))