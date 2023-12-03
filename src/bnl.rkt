#lang racket

(define (parser tokens)
  (cond
   ((null? tokens) (error "Empty program"))
   ((equal? (car tokens) 'function ) (parse-function (cdr tokens)))
   (else (parser (cdr tokens)))
  )
)

(define (parse-function tokens)
  (let ((function-name (cadr tokens))
        (function-args (caddr tokens))
        (function-body (cdddr tokens)))
    (list 'function function-name function-args function-body)))

(define (eval ast env)
  (cond 
    ((equal? (car ast) 'function) (eval-function (cdr ast) env))
    ((equal? (car ast) 'vars ) (eval-vars (cdr ast) env))
    ((equal? (car ast) 'begin) (eval-begin (cdr ast) env))
    ((equal? (car ast) 'if) (eval-if (cdr ast) env))
    ((equal? (car ast) 'return) (eval-return (cadr ast) env))
  ))

(define (eval-function ast env)
  (let ((function-name (car ast))
        (function-args (cadr ast))
        (function-body (caddr ast)))
    (define (function-eval args)
      (let ((new-env (append (map (lambda (arg) (list arg (car args))) function-args) env)))
        (eval function-body new-env)))
    (list function-name function-eval)))

(define (eval-vars ast env)
 (let ((vars (car ast)))
   (define (eval-var var)
     (let ((var-name (car var))
           (var-value (cadr var)))
       (list var-name (eval var-value env))))
   (map eval-var vars))
)

(define (eval-begin ast env)
  (let ((expressions (car ast)))
    (map (lambda (expr) (eval expr env)) expressions)))

(define (eval-if ast env)
  (let ((condition (car ast))
        (then-expr (cadr ast))
        (else-expr (caddr ast)))
    (if (eval condition env)
        (eval then-expr env)
        (eval else-expr env))))

(define (eval-return ast env)
  (eval (car ast) env))

(define (lexer code)
  (let ((tokens '())
        (current-token '())
        (inside-string? #f))
    
    (define (add-token)
      (when (not (null? current-token))
        (set! tokens (append tokens (list (string->symbol (list->string current-token)))))
        (set! current-token '())))
    
    (define (process-char c)
      (cond
       ((char-whitespace? c) ; Verifica se o caractere é um espaço em branco
        (add-token))
       ((char=? c #\") ; Verifica se o caractere é uma aspa dupla
        (if inside-string?
            (begin (set! inside-string? #f) (add-token))
            (set! inside-string? #t)))
       ((char=? c #\;) ; Ignora o restante da linha se encontrar um ponto e vírgula (comentário)
        (add-token) ; Adiciona o token antes de ignorar o restante da linha
        (set! current-token '()) ; Reseta o token atual
        (set! inside-string? #f) ;  não esta dentro de uma string
        (read-line)) ; Ignora o restante da linha
       ((eof-object? c) ; Verifica se  final do arquivo
        (add-token)) ; Adiciona o último token se houver algum
       (else
        (set! current-token (append current-token (list c))))))

    (for-each process-char code)
    tokens))
;; Exemplo de uso:
(define code-example
  "( function main < >
     vars a b
     b = 2 !
     begin
       a = 2 !
       if a gt b then b = 0 fi
       return 0 !
     end
   )")

(display (eval (parser (lexer (string->list code-example))) '()))