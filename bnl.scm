# lang racket 


(define gramm '(
    (FUNC_DEF (PROTOCOL FUNC_BODY) func_def)
    (FUNC_DEF (PROTOCOL VAR_DEF FUNC_BODY) func_def)
    (PROTOCOL ("function" NAME "<" ">") protocol)
    (PROTOCOL ("function" NAME "<" NAMES ">") protocol)
    (VAR_DEF ("vars" NAMES) var_def)
    
    (NAMES (NAME) names)
    (NAMES (NAME NAMES) names)
    (FUNC_BODY ("begin" COMMANDS "end") func_body)
    
    (COMMANDS (COMMAND) commands)
    (COMMANDS (COMMAND COMMANDS) commands)
    (COMMAND (FUNC_CALL "!") command)
    (COMMAND (RETURN "!") command)
    (COMMAND (ATTRIBUTION "!") command)
    (COMMAND (IF "!") command)
    (COMMAND (CODE_MOD "!") command)
    
    (FUNC_CALL (NAME "<" ">") func_call)
    (FUNC_CALL (NAME "<" VALS ">") func_call)
    (RETURN (RETURN VALS) return)
    (ATTRIBUTION (SIMPLE_ATTRIBUTION) attribution)
    (ATTRIBUTION (EXPR_ATTRIBUTION) attribution)
    (ATTRIBUTION (FUNC_ATTRIBUTION) attribution)
    (SIMPLE_ATTRIBUTION (NAME "=" VAL) simple_attribution)
    (EXPR_ATTRIBUTION (NAME "=" VAL OP VAL) expr_attribution)
    (FUNC_ATTRIBUTION (NAME "=" FUNC_CALL) func_attribution)

    (CONDITIONAL ("IF" "<"IF_TEST">" "THEN" IF_COMMAND "FI") conditional)
    (IF_TEST (VAL REL_OP VAL) if_test)
    (REL_OP ("lt" "le" "gt" "ge" "eq" "ne") rel_op)
    (IF_COMMAND (FUNC_CALL "!") if_command)
    (IF_COMMAND (ATTRIBUTION "!") if_command)
    (IF_COMMAND (RETURN "!") if_command)

    (CODE_MOD (CODE_UPDT CODE_DEL) code_mod)
    (CODE_UPDT ("update" NAME "@" INTEGER : COMMAND) code_updt)
    (CODE_DEL ("delete" NAME "@" INTEGER) code_del)
    
    (VALS (VAL) vals)
    (VALS (VAL VALS) vals)
    (VAL (NAME) val)
    (VAL (NUMBER) val)
    (NUMBER (INTEGER) number)
    (NUMBER (FLOAT) number)

    (OP ("+" "-" "*" "/") op)
    (NAME ( [a-z] ) name)
    (INTEGER ( [0-9] ) integer)
    (FLOAT ( [0-9] "." [0-9] ) float)

))

(define (func_def code)
    (eq? (cadr code) (protocol (cadr code)))
    
)

(define (protocol code)
    (cond
        ((eq? (cadr code) 'function (caddr code) '<>))
        ((eq? (cadr code) 'function (caddr code) '< (caddr code) '>))
    ) 
)


(define code '(
    function main < >
    begin
        a = 2 !
        a = 5.0 + 4 !
        return a !
    end
))



(define process (lambda (code)
    (cond
        ((eq? (car code) 'function) (func_def code)')
    )
))


(process code)