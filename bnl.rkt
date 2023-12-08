#lang racket

(define (parse-program tokens)
  (if (equal? (peek-token tokens) 'function)
      (parse-func-def tokens)
      (parse-program (cadr tokens))))

(define (parse-func-def tokens)
  (let ((protocol (parse-protocol tokens)))
    (cond
      ((equal? (peek-token tokens) 'vars)
       (consume-token tokens)
       (let ((var-def (parse-var-def tokens)))
         `(func-def-vars ,protocol ,var-def)))
      (else
       `(func-def ,protocol)))))

(define (parse-protocol tokens)
  (expect-token tokens 'function)
  (let ((name (expect-token tokens 'identifier)))
    (cond
      ((equal? (peek-token tokens) '<)
       (consume-token tokens)
       (expect-token tokens '>)
       `(protocol-no-args ,name))
      ((equal? (peek-token tokens) '<)
       (consume-token tokens)
       (let ((names (parse-names tokens)))
         (expect-token tokens '>)
         `(protocol-with-args ,name ,names))))))

(define (parse-var-def tokens)
  (expect-token tokens 'vars)
  (let ((names (parse-names tokens)))
    `(var-def ,names)))

(define (parse-names tokens)
  (let loop ((names '()))
    (cond
      ((equal? (peek-token tokens) 'identifier)
       (loop (cons (consume-token tokens) names)))
      (else (reverse names)))))

(define (expect-token tokens expected)
  (let ((actual (peek-token tokens)))
    (if (equal? expected actual)
        (consume-token tokens)
        (error (format "Expected ~a, but got ~a" expected actual)))))

(define (peek-token tokens)
  (if (null? tokens)
      'eof
      (car tokens)))

(define (consume-token tokens)
  (let ((current-token (peek-token tokens)))
    (set! tokens (cdr tokens))
    current-token))

;; Example usage:


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

(define example-code
  " function main < > vars a b b = 2 ! begin a = 2 ! if a gt b then b = 0 fi return 0 ! end")

(define example-tokens 
  (lexer (string->list code-example)))

(display (parse-program example-tokens))