#lang racket

(define (parser tokens)
  (cond
   ((null? tokens) (error "Empty program"))
   ((equal? (car tokens) 'function ) (parse-function (cdr tokens)))
   (else (parser (cdr tokens)))))

(define (parse-function tokens)
  (let ((function-name (car tokens))
        (rest-tokens (cddr tokens)))
    (let loop ((args '()))
      (cond
       ((null? rest-tokens) (list 'function function-name args '()))
       ((equal? (car rest-tokens) '<)
        (let* ((arg (cadr rest-tokens))
               (remaining-tokens (cddr rest-tokens)))
          (loop (append args (list arg)) remaining-tokens)))
       (else (error "Invalid function declaration"))))))

(define (parse-body tokens)
  (cond
    ((null? tokens) '())
    ((equal? (car tokens) 'vars) (cons (parse-vars (cdr tokens)) (parse-body (cddr tokens))))
    ((equal? (car tokens) 'begin) (cons (parse-begin (cdr tokens)) (parse-body (cddr tokens))))
    ((equal? (car tokens) 'if) (cons (parse-if (cdr tokens)) (parse-body (cddr tokens))))
    ((equal? (car tokens) 'return) (cons (parse-return (cdr tokens)) (parse-body (cddr tokens))))
    (else (error "Unknown expression"))))


(define (parse-vars tokens)
  (let loop ((tokens tokens)
             (vars '()))
    (cond
      ((null? tokens) vars)
      ((equal? (car tokens) '!) vars)
      ((equal? (car tokens) 'end) vars)
      (else (loop (cdr tokens) (append vars (list (parse-var (car tokens)))))))))

(define (parse-var tokens)
  (let ((var-name (car tokens))
        (var-value (cadr tokens)))
    (list var-name (parse-expression var-value))))

(define (parse-expression tokens)
  (cond
    ((number? tokens) tokens)
    ((string? tokens) tokens)
    ((equal? tokens 'gt) '>)
    ((equal? tokens 'lt) '<)
    ((equal? tokens 'eq) '=)
    ((equal? tokens 'print) 'display)
    ((equal? tokens '+) '())
    ((equal? tokens '-) '())
    ((equal? tokens '*) '())
    ((equal? tokens '/) '())))

(define (parse-begin tokens)
  (let loop ((tokens tokens)
             (expressions '()))
    (cond
      ((null? tokens) expressions)
      ((equal? (car tokens) 'end) expressions)
      (else (loop (cdr tokens) (append expressions (list (parse-expression (car tokens)))))))))

(define (parse-if tokens)
  (let ((condition (car tokens))
        (then-expr (cadr tokens))
        (else-expr (caddr tokens)))
    (list 'if (parse-expression condition) (parse-expression then-expr) (parse-expression else-expr))))

(define (parse-return tokens)
  (list 'return (parse-expression (car tokens))))

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
       ((char-whitespace? c)        ; Verifica se o caractere é um espaço em branco
        (add-token))
       ((char=? c #\")              ; Verifica se o caractere é uma aspa dupla
        (if inside-string?
            (begin (set! inside-string? #f) (add-token))
            (set! inside-string? #t)))
       ((char=? c #\;)              ; Ignora o restante da linha se encontrar um ponto e vírgula (comentário)
        (add-token)                 ; Adiciona o token antes de ignorar o restante da linha
        (set! current-token '())    ; Reseta o token atual
        (set! inside-string? #f)    ; não esta dentro de uma string
        (read-line))                ; Ignora o restante da linha
       ((eof-object? c)             ; Verifica se  final do arquivo
        (add-token))                ; Adiciona o último token se houver algum
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

(display (lexer (string->list code-example)))
(newline)
(display (parser (lexer (string->list code-example)))) ; fix
