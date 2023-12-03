(define-struct token (tipo valor))


(define tokenType 
  (enum Function Number Identifier Equals 
        OpenPar ClosePar BinOp Var RelOp 
        Begin End ExclMark Return ))

(define-struct lexer (tokens))