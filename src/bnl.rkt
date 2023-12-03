#lang racket

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
  "(function f < a b >
     vars c
     begin
       a = 2 !
       if a gt b then b = 0 fi
       return 0 !
     end
     function main < >
     begin
       f < 1 2 > !
       delete f @ 2 !
       f < 5 3 > !)")

(display (lexer (string->list code-example)))