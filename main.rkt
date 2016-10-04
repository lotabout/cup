#lang axe

(require racket/contract)

(provide get post put patch delete
         dispatch
         status->message
         handler-404
         (contract-out [add-filter (-> string?
                                       ((or/c #f string?) . -> . (cons/c string? (-> string? any/c)))
                                       void?)]))

(require web-server/servlet
         web-server/servlet-env
         net/uri-codec
         (for-syntax syntax/parse
                     racket/list
                     racket/match))

(module+ test
  (require rackunit)
  (require (for-syntax rackunit)))

;; regexp-flatten: (string? -> string?)
;; change the capturing groups in a regexp string into non-capturing groups
;; example: "(.*)" => "(?:.*)"
(define (regexp-flatten regex-string)
  ; why the \? is enclosed with parenthesises because racket's regex had a bug
  (regexp-replace* #px/(\\*)(\((?!(\?)))/
                   regex-string
                   (lambda matches
                     (if (not (zero? (~> matches (list-ref 1) string-length)))
                         (list-ref matches 0)
                         (string-append (list-ref matches 1)
                                        "(?:")))))

(module+ test
  (check-equal? (regexp-flatten r/((x)(x))/) r/(?:(?:x)(?:x))/)
  (check-equal? (regexp-flatten r/(?!(?=x)(x))/) r/(?!(?=x)(?:x))/))

(define RULE_RE #px/([^<]*)(?:<([a-zA-Z0-9_]*)(?:(?::([a-zA-Z_]*))(?::([^>]+))?)?>)?/)

(module+ test
  (check-equal? (regexp-match* RULE_RE "/hello/" :match-select cdr)
                '(("/hello/" #f #f #f) ("" #f #f #f)))
  (check-equal? (regexp-match* RULE_RE "/hello/<name>" :match-select cdr)
                '(("/hello/" "name" #f #f) ("" #f #f #f)))
  (check-equal? (regexp-match* RULE_RE "/hello/<name:path>" :match-select cdr)
                '(("/hello/" "name" "path" #f) ("" #f #f #f)))
  (check-equal? (regexp-match* RULE_RE r"/hello/<name:re:[a-z]+[\d]+>" :match-select cdr)
                '(("/hello/" "name" "re" "[a-z]+[\\d]+") ("" #f #f #f)))
  (check-equal? (regexp-match* RULE_RE r"/hello/<name:filter:conf>/<name:filter:conf>/edit" :match-select cdr)
                '(("/hello/" "name" "filter" "conf")
                  ("/" "name" "filter" "conf")
                  ("/edit" #f #f #f)
                  ("" #f #f #f))))

(define DEFAULT-FILTER-PATTERN r"[^/]+")
;; a filter is a functino that return (conf-to-regex, str-to-param)
(define FILTERS
  (box {"int" (lambda (conf) (cons r/-?\d+/, string->number)),
        "float" (lambda (conf) (cons r/-?[\d.]+/, string->number)),
        "path" (lambda (conf) (cons r/.+?/, identity))
        "re" (lambda (conf) (cons (regexp-flatten (or conf DEFAULT-FILTER-PATTERN))
                                  identity))}))

; (string? (string? . -> . (cons/c regexp? (string? . -> . any/c))) . -> .void?)
(define (add-filter name custom-filter)
  (set-box! FILTERS (dict-set (unbox FILTERS) name custom-filter)))

;; turn a URL pattern into a matcher that when feed a real URL, it will return a dictionary of params.
;; (string? . -> . (string? . -> dictionary?))
;;
;; example:
;; URL pattern: "/hello/<name>"
;; procedure (lambda (url) ...) => return {"name": "world"}
(define (pattern->match url-pattern)
  ; get the parameter components. We know the last one is dummy
  (define components (~> (regexp-match* RULE_RE url-pattern :match-select cdr)
                         (drop-right 1)))
  (match-define (list regex-strings names out-filters)
    (~>> components
        (map (match-lambda
               [(list prefix name mode conf)
                (cond
                  [(and (equal? prefix "") (not name) (not mode) (not conf))
                   (error "pattern should be in format: '<name:filter:conf>'")]
                  [(not name) (list prefix #f #f)]
                  [else
                    (match ((hash-ref (unbox FILTERS) (or mode "re")) conf)
                      [(cons filter-regex out-filter)
                       ; output (regex-string, name of field, out-filter)
                       (list (string-append prefix "(" (regexp-flatten filter-regex) ")")
                             name
                             out-filter)])])]))
        ; transpose ((A 1) (B 2) ...) => ((A B ...) (1 2 ...))
        (apply map list)))

  (define pattern-regexp (pregexp (string-append "^" (string-join regex-strings "") "$")))
  (define name-list (map string->symbol (filter identity names)))
  (define out-filter-list (filter identity out-filters))

  (lambda (url)
    (if-let [match-result (regexp-match pattern-regexp url)]
      (for/hash ([key (in-list name-list)]
                 [val (in-list (cdr (regexp-match pattern-regexp url)))]
                 [out-filter (in-list out-filter-list)]
                 #:when (not (equal? key '_)))
          (values key (out-filter val)))
      #f)))

(module+ test
  (check-equal? ((pattern->match "/int/<x:int>") "/int/10") {'x 10})
  (check-equal? ((pattern->match "/int/<x:int>/") "/int/10") #f)
  (check-equal? ((pattern->match "/int/<x:int>") "/int/abc") #f)

  (check-equal? ((pattern->match "/float/<x:float>") "/float/20.32") {'x 20.32})
  (check-equal? ((pattern->match "/float/<x:float>/") "/float/20.32") #f)
  (check-equal? ((pattern->match "/float/<x:float>") "/float/20") {'x 20})
  (check-equal? ((pattern->match "/float/<x:float>") "/float/20a") #f)

  (check-equal? ((pattern->match "/hello/<name>") "/hello/中国") {'name "中国"})
  (check-equal? ((pattern->match "/hello/<name>/") "/hello/中国") #f)
  (check-equal? ((pattern->match "/hello/<name>/") "/hello/中国/") {'name "中国"})

  (check-equal? ((pattern->match "/hello/<action>/<param>") "/hello/minus/10")
                {'action "minus", 'param "10"})

  (check-equal? ((pattern->match "/hello/<x:path>") "/hello/world/I/love/you/")
                {'x "world/I/love/you/"})
  (check-equal? ((pattern->match "/re/<x:re:a*b?c*>") "/re/aaabccc") {'x "aaabccc"})
  (check-exn exn:fail? (lambda () (pattern->match "/wrong/pattern/<x:un"))))

(define-struct route (method matcher callback) #:transparent)
(define-struct param (names has-keyword? has-rest?) #:transparent)

(define ((gen-callback param func) params)
  (define arg-list
    (append
      (for/list ([name (param-names param)])
        (params name))
      (if (param-has-rest? param)
          (list (for/fold ([params params])
                  ([name (param-names param)])
                  (dict-remove params name)))
          '())))
  (if (param-has-keyword? param)
      (apply func arg-list #:as params)
      (apply func arg-list)))

(define-for-syntax (parse-arg-list stx)
  (define arg-stx-list (syntax-e stx))
  (let loop ([arg-list arg-stx-list]
             [nms '()]
             [keyword #f]
             [rest-name #f])
    (cond
      [(null? arg-list)
       (list (reverse nms) keyword rest-name)]
      [(not (pair? arg-list))
       (list (reverse nms) keyword (syntax-e arg-list))]
      [(keyword? (syntax-e (car arg-list)))
        (loop (cddr arg-list) nms (syntax-e (cadr arg-list)) rest-name)]
      [else
        (loop (cdr arg-list) (cons (syntax-e (car arg-list)) nms) keyword rest-name)])))

(module+ test
  (begin-for-syntax
    (check-equal? (parse-arg-list #'()) (list '() #f #f))
    (check-equal? (parse-arg-list #'(a)) (list '(a) #f #f))
    (check-equal? (parse-arg-list #'(a b)) (list '(a b) #f #f))
    (check-equal? (parse-arg-list #'(a b . r)) (list '(a b) #f 'r))
    (check-equal? (parse-arg-list #'(a b #:as k)) (list '(a b) 'k #f))
    (check-equal? (parse-arg-list #'(a b #:as k c)) (list '(a b c) 'k #f))
    (check-equal? (parse-arg-list #'(a b #:as k c . r)) (list '(a b c) 'k 'r))))

(define-for-syntax (compose-args names keyword rest-name)
  (define front-part (append names (if keyword `(#:as ,keyword) '())))
  (if rest-name
      (append (drop-right front-part 1) (cons (last front-part) rest-name))
      front-part))

(module+ test
  (begin-for-syntax
    (check-equal? (compose-args '() #f #f) '())
    (check-equal? (compose-args '(a b) #f #f) '(a b))
    (check-equal? (compose-args '(a b) 'k #f) '(a b #:as k))
    (check-equal? (compose-args '(a b) #f 'r) '(a b . r))
    (check-equal? (compose-args '(a b) 'k 'r) '(a b #:as k . r))))

(define-syntax (gen-handler stx)
  (syntax-case stx ()
    [(_ method pattern arg-list . body)
     (match (parse-arg-list #'arg-list)
       [(list names kw-name rest-name)
        (with-syntax ([args (datum->syntax #'arg-list (compose-args names kw-name rest-name) #'arg-list)]
                      [(nm ...) (datum->syntax stx names stx)]
                      [has-keyword (if kw-name #'#t #'#f)]
                      [has-rest (if rest-name #'#t #'#f)])
          #'(add-route method pattern
                       (gen-callback (param (list 'nm ...) has-keyword has-rest)
                                     (lambda args . body))))])]))

(define-syntax-rule (get pattern args . body)
  (gen-handler "GET" pattern args . body))
(define-syntax-rule (post pattern args . body)
  (gen-handler "POST" pattern args . body))
(define-syntax-rule (put pattern args . body)
  (gen-handler "PUT" pattern args . body))
(define-syntax-rule (patch pattern args . body)
  (gen-handler "PATCH" pattern args . body))
(define-syntax-rule (delete pattern args . body)
  (gen-handler "DELETE" pattern args . body))

(define ROUTES (box '()))

(define (add-route method pattern callback)
  (set-box! ROUTES (dict-update (unbox ROUTES)
                                method
                                (lambda (old) (cons (route method (pattern->match pattern) callback) old))
                                '())))

;; default 404 handler
(define handler-404
  (make-parameter (lambda (req)
                    (response/xexpr
                      #:code 404
                      #:message (status->message 404)
                      `(html (body (p "Not Found.")))))))

(define (dispatch req)
  (define method (bytes->string/utf-8 (request-method req)))
  (let loop ([routes (dict-ref (unbox ROUTES) method '())])
    (if (null? routes)
        ((handler-404) req)
        (match (car routes)
          [(route method matcher callback)
           (if-let [params (matcher (~> req request-uri url->string uri-decode))]
             (callback (merge-params params req))
             (loop (cdr routes)))]))))

;; Merge the parameters extracted from requesting URL and the query/post parameters.
(define (merge-params params request)
  (dict-merge
    `((method . ,(bytes->string/utf-8 (request-method request)))
      (headers . ,(request-headers request))
      (host-ip . ,(request-host-ip request))
      (host-port . ,(request-host-port request))
      (client-ip . ,(request-client-ip request))
      (bindings . ,(request-bindings/raw request)))
    (~> request request-uri url-query reverse)
    params))

(define (status->message status)
  (case status
    [(100) #"Continue"]
    [(101) #"Switching Protocols"]
    [(200) #"OK"]
    [(201) #"Created"]
    [(202) #"Accepted"]
    [(203) #"Non-Authoritative Information"]
    [(204) #"No Content"]
    [(205) #"Reset Content"]
    [(206) #"Partial Content"]
    [(300) #"Multiple Choices"]
    [(301) #"Moved Permanently"]
    [(302) #"Found"]
    [(303) #"See Other"]
    [(304) #"Not Modified"]
    [(305) #"Use Proxy"]
    [(307) #"Temporary Redirect"]
    [(400) #"Bad Request"]
    [(401) #"Unauthorized"]
    [(402) #"Payment Required"]
    [(403) #"Forbidden"]
    [(404) #"Not Found"]
    [(405) #"Method Not Allowed"]
    [(406) #"Not Acceptable"]
    [(407) #"Proxy Authentication Required"]
    [(408) #"Request Timeout"]
    [(409) #"Conflict"]
    [(410) #"Gone"]
    [(411) #"Length Required"]
    [(412) #"Precondition Failed"]
    [(413) #"Request Entity Too Large"]
    [(414) #"Request-URI Too Long"]
    [(415) #"Unsupported Media Type"]
    [(416) #"Requested Range Not Satisfiable"]
    [(417) #"Expectation Failed"]
    [(500) #"Internal Server Error"]
    [(501) #"Not Implemented"]
    [(502) #"Bad Gateway"]
    [(503) #"Service Unavailable"]
    [(504) #"Gateway Timeout"]
    [(505) #"HTTP Version Not Supported"]
    [else #""]))
