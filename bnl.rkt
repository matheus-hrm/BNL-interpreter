#lang racket

(define escopos (list (make-hash)))          

(define (var-cria nome valor)
  (hash-set! (car escopos) nome valor))

(define (var-acessa nome)
  (hash-ref (car escopos) nome))          
(define (escopo-entra)          
  (set! escopos (cons (make-hash) escopos)))

(define (escopo-sai)          
  (set! escopos (cdr escopos)))          

(define (processar-linha linha)          
  (define (remover-espacos str)
    (string-join (string-split str) "")) ; Remove espaços da string **NAO FUNCIONA**
  (cond
    ((string-contains? linha "var")
     (let* ((parts (string-split linha)); Divide a linha em partes
            (nome (cadr parts))
            (valor-str (cadddr parts))
            (val (if (string->number valor-str)
                     (string->number valor-str)
                     (if (string-contains? valor-str "+") ; Se o valor contém "+", é uma soma
                         (let* ((sum-parts (string-split valor-str "+"))
                                (a (string->number  (car sum-parts))) ; Obtém o primeiro valor da soma
                                (b (string->number  (cadr sum-parts))))
                           (+ a b))
                         (error "Valor inválido")))))
       (var-cria nome val)))                    
    ((string-contains? linha "escopo"); Se a linha contém "escopo", entra em um novo escopo
     (escopo-entra))                    
    ((string-contains? linha "end"); Se a linha contém "end", sai do escopo atual
     (escopo-sai))                    
    (else #f)))     ; Se a linha não se encaixa em nenhum caso, retorna #f              
(define (processar-codigo codigo)
  (for-each processar-linha (string-split codigo "\n")))

(define process (processar-codigo "
                                   var a = 1
                                   var b = 2
                                   escopo
                                   var a = 3
                                   var c = 4
                                   escopo
                                   var a = 5
                                   var d = 2+3  
                                   "))
(define (exibir-todas-variaveis)
  (display "Todas as variáveis:")
  (newline)
  (for-each (lambda (escopo); Para cada escopo na lista de escopos
              (let ((variaveis (hash->list escopo))); Obtém a lista de variáveis do escopo
                (for-each (lambda (variavel); Para cada variável no escopo
                            (display (format "~a = ~a" (car variavel) (cdr variavel))); Exibe o nome e o valor da variável
                            (newline))
                          variaveis)))
            escopos))
(exibir-todas-variaveis)


(define (exibir-variaveis-escopo-atual)
  (let ((variaveis-no-escopo (hash->list (car escopos))))
    (display "Variáveis no escopo atual:")
    (newline)
    (for-each (lambda (variavel); Para cada variável no escopo atual
                (display (format "~a = ~a" (car variavel) (cdr variavel)))
                (newline))
              variaveis-no-escopo))); Exibe variáveis no escopo atual
(exibir-variaveis-escopo-atual)
