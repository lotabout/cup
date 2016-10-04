#lang axe

(require web-server/servlet)
(require web-server/servlet-env)
(require "main.rkt")

(define example-links (box '()))
(define (add-example description link)
  (set-box! example-links (cons (cons description link)
                                (unbox example-links))))

; test URL: localhost:8000/int/10
(get "/int/<x:int>" [x]
     (response/xexpr
       `(html (body (p "x is : " ,(number->string x))))))
(add-example "Filter integer" "/int/10")
;
; test URL: localhost:8000/float/20.32
(get "/float/<x:float>" [x]
     (response/xexpr
       `(html (body (p "x is : " ,(number->string x))))))
(add-example "Filter float" "/float/20.32")

; test URL: localhost:8000/hello/world
; test URL: localhost:8000/hello/中国
(get "/hello/<name>" [name]
     (response/xexpr
       `(html (body (p "name is : " ,name)))))
(add-example "Normal usage: ASCII" "/hello/world")
(add-example "Normal usage: unicode" "/hello/中国")

; test URL: localhost:8000/hello/world/I/love/you
(get "/hello/<x:path>" [x]
     (response/xexpr
       `(html (body (p "x: " ,x)))))
(add-example "Filter path" "/hello/world/I/love/you")

; test URL: localhost:8000/hello/minus/10
(get "/hello/<action>/<param>" [action param]
     (response/xexpr
       `(html (body (p "action : " ,action)
                    (p "param: " ,param)))))
(add-example "Multilpe Patterns" "/hello/minus/10")

; test URL: localhost:8000/hello/world/I/love/you
(get "/re/<x:re:a*b?c*>" [x]
     (response/xexpr
       `(html (body (p "x: " ,x)))))
(add-example "Filter regular expression" "/re/aaabccc")


; test URL: localhost:8000/hello/minus/10
(get "/hello/<action>:<param>" [action param]
     (response/xexpr
       `(html (body (p "action : " ,action)
                    (p "param: " ,param)))))
(add-example "Delimiter other than /" "/hello/test:route")

; Add custom filter
(add-filter
  "list"
  (lambda (config)
    (define delimiter (or config ","))
    (define regex (string-append r"\d+(" delimiter r"\d)*"))
    (define (to-list match)
      (map string->number (string-split match ",")))
    (cons regex to-list)))

; test URL: localhost:8000/list/1,2,3,4,5
(get "/list/<lst:list>" [lst]
     (response/xexpr
       `(html (body (p "list: " ,(format "~a" lst))))))
(add-example "Custome filter: list" "/list/1,2,3,4,5")

(get "/examples" []
     (response/xexpr
       `(html (body
                (ul
                  ,@(for/list ([example (in-list (reverse (unbox example-links)))])
                      `(li (a ((href ,(cdr example))) ,(car example)))))))))

(serve/servlet dispatch
  :servlet-regexp #rx""
  :servlet-path "/examples")
