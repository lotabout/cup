#lang axe

(module+ test
  (require rackunit))

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

(define RULE_RE #px/([^<]*)(?:<([a-zA-Z0-9]*)(?:(?::([a-zA-Z_]*))(?::([^>]+))?)?>)?/)

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
  {"int" (lambda (conf) (list r/-?\d+/, string->number)),
   "float" (lambda (conf) (list r/-?[\d.]+/, string->number)),
   "path" (lambda (conf) (list r/.+?/, identity))
   "re" (lambda (conf) (list (regexp-flatten (or conf DEFAULT-FILTER-PATTERN))
                             identity))})

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
                    (match ((hash-ref FILTERS (or mode "re")) conf)
                      [(list filter-regex out-filter)
                       ; output (regex-string, name of field, out-filter)
                       (list (string-append prefix "(" (regexp-flatten filter-regex) ")")
                             name
                             out-filter)])])]))
        ; transpose ((A 1) (B 2) ...) => ((A B ...) (1 2 ...))
        (apply map list)))

  (define pattern-regexp (pregexp (string-append "^" (string-join regex-strings "") "$")))
  (define name-list (filter identity names))
  (define out-filter-list (filter identity out-filters))

  (lambda (url)
    (if-let [match-result (regexp-match pattern-regexp url)]
      (for/hash ([key (in-list name-list)]
                   [val (in-list (cdr (regexp-match pattern-regexp url)))]
                   [out-filter (in-list out-filter-list)])
          (values key (out-filter val)))
      #f)))

(module+ test
  (check-equal? ((pattern->match "/int/<x:int>") "/int/10") {"x" 10})
  (check-equal? ((pattern->match "/int/<x:int>/") "/int/10") #f)
  (check-equal? ((pattern->match "/int/<x:int>") "/int/abc") #f)

  (check-equal? ((pattern->match "/float/<x:float>") "/float/20.32") {"x" 20.32})
  (check-equal? ((pattern->match "/float/<x:float>/") "/float/20.32") #f)
  (check-equal? ((pattern->match "/float/<x:float>") "/float/20") {"x" 20})
  (check-equal? ((pattern->match "/float/<x:float>") "/float/20a") #f)

  (check-equal? ((pattern->match "/hello/<name>") "/hello/中国") {"name" "中国"})
  (check-equal? ((pattern->match "/hello/<name>/") "/hello/中国") #f)
  (check-equal? ((pattern->match "/hello/<name>/") "/hello/中国/") {"name" "中国"})

  (check-equal? ((pattern->match "/hello/<action>/<param>") "/hello/minus/10")
                {"action" "minus", "param" "10"})

  (check-equal? ((pattern->match "/hello/<x:path>") "/hello/world/I/love/you/")
                {"x" "world/I/love/you/"})
  (check-equal? ((pattern->match "/re/<x:re:a*b?c*>") "/re/aaabccc") {"x" "aaabccc"})
  (check-exn exn:fail? (lambda () (pattern->match "/wrong/pattern/<x:un"))))
