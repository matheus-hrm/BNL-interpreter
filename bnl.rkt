#lang racket

(define process
  (lambda (code)
    (eval (parse-tree (reader code)))))

(define (reader expr)
  (let append ((expr expr) (acc '()))
    (cond
      ((null? expr) (reverse acc))
      ((equal? (car expr) '!))
      (else (append (cdr expr) (cons (car expr) acc)))
    )
  )
)

(define (parse-tree expr)
  (cond 
    ((equal? (car expr) 'function) (parse-function (cdr expr)))
    (else '())
  )
)

(define (parse-function expr)
  (cons 'function (parse-body expr))
)

(define (parse-body expr)
  (cond
    ((equal? (car expr) 'begin) (parse-begin (cdr expr)))
    (else '())
  )
)
  
(define (parse-begin expr)
  (cond
    ((equal? (car expr) 'return) (parse-return (cdr expr)))
    (else '())
  )
)

(define (parse-return expr)
  (cons 'return (parse-expr (cdr expr)))
)

(define (parse-expr expr)
  (cond
    ((equal? (car expr) '!))
    ((equal? (car expr) '=) (parse-assign (cdr expr)))
    ((equal? (car expr) '+) (parse-add (cdr expr)))
    (else '())
  )
)

(define (parse-assign expr)
  (cons '= (parse-expr (cdr expr)))
)

(define (parse-add expr)
  (cons '+ (parse-expr (cdr expr)))
)


(define (eval expr)
  (cond 
    ((equal? (car expr) 'function) (eval-function (cdr expr)))
    (else '())
  )
)

(define (eval-function expr)
  (eval-body expr)
)

(define (eval-body expr)
  (cond
    ((equal? (car expr) 'begin) (eval-begin (cdr expr)))
    (else '())
  )
)

(define (eval-begin expr)
  (cond
    ((equal? (car expr) 'return) (eval-return (cdr expr)))
    (else '())
  )
)

(define (eval-return expr)
  (eval-expr expr)
)

(define (eval-expr expr)
  (cond
    ((equal? (car expr) '!))
    ((equal? (car expr) '=) (eval-assign (cdr expr)))
    ((equal? (car expr) '+) (eval-add (cdr expr)))
    (else '())
  )
)

(define code '(
  function main
  begin
    a = 2 !
    a = 5.0 + 4 !
    return a !
))

(process code)