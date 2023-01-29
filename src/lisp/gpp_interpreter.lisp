(load "gpp_lexer.lisp")

; grammar rules
(setq *EXP1* (list "OP_OP" "OP_PLUS" "EXP" "EXP" "OP_CP"))
(setq *EXP2* (list "OP_OP" "OP_MINUS" "EXP" "EXP" "OP_CP"))
(setq *EXP3* (list "OP_OP" "OP_MULT" "EXP" "EXP" "OP_CP"))
(setq *EXP4* (list "OP_OP" "KW_IF" "EXPB" "EXP" "EXP" "OP_CP"))
(setq *EXP5* (list "OP_OP" "KW_SET" "IDENTIFIER" "EXP" "OP_CP"))
(setq *EXP6* (list "OP_OP" "KW_DEFVAR" "IDENTIFIER" "EXP" "OP_CP"))
(setq *EXPB1* (list "OP_OP" "KW_AND" "EXPB" "EXPB" "OP_CP"))
(setq *EXPB2* (list "OP_OP" "KW_OR" "EXPB" "EXPB" "OP_CP"))
(setq *EXPB3* (list "OP_OP" "KW_NOT" "EXPB" "OP_CP"))
(setq *EXPB4* (list "OP_OP" "KW_EQ" "EXP" "EXP" "OP_CP"))
(setq *EXPB5* (list "OP_OP" "KW_GT" "EXP" "EXP" "OP_CP"))
(setq *FCALL1* (list "OP_OP" "IDENTIFIER" "OP_CP"))
(setq *FCALL2* (list "OP_OP" "IDENTIFIER" "EXP" "OP_CP"))
(setq *FCALL3* (list "OP_OP" "IDENTIFIER" "EXP" "EXP" "OP_CP"))
(setq *FCALL4* (list "OP_OP" "IDENTIFIER" "EXP" "EXP" "EXP" "OP_CP"))

; lexers word pair list
(setq *word_pair* (list ))

; symbol table for identifiers (name, value pairs)
(setq *symbol_table* (list ))

; function table (name, variable pairs(local and argument), explist)
(setq *function_table* (list ))

; indicates function is being defined
(setq *func_def_state* nil)

; used when function is called
;(setq *fun_call_stack* (list ))
(setq *fun_call_num* 0)
;(setq *local_symbol_table* (list ))

; main function
(defun gpp-driver ()
    (if (null *args*) (gppinterpreter nil) (gppinterpreter nil (car *args*)))    
)

; main interpreter
(defun gppinterpreter (parse_stack &optional file_name)

    (setq *word_pair* (gpp-lexer file_name))
    ;(write *word_pair*)

    ; parsing
    (if parse_stack (setq parse_stack (gpp-parse parse_stack)) (setq parse_stack (gpp-parse (list ))))

    (setq tokens (list ))
    (setq tokens (mapcar #'(lambda(x) (append tokens (car x))) parse_stack))
    ; (format t "~%Tokens: ")
    ; (write tokens)
    
    ; check syntax
    (if (null *func_def_state*) (check-syntax-state-general tokens))
    (if (null *func_def_state*) (setq parse_stack (eval-parse-stack parse_stack)))

    ; (format t "~%Parse Stack: ")
    ; (write parse_stack)
    ; (format t "~%Word List: ")
    ; (write *word_pair*)
    ; (format t "~%Symbol Table: ")
    ; (write *symbol_table*)
    ; (format t "~%Function Table: ")
    ; (write *function_table*)
    ; (format t "~%Function Definition State:")
    ; (write *func_def_state*)

    (gppinterpreter parse_stack)  
)


(defun gpp-parse (parse_stack)
    
    ; (format t "~%Parse Stack: ")
    ; (write parse_stack)
    ; (format t "~%Word List: ")
    ; (write *word_pair*)
    ; (format t "~%")

    ; if it is not consumed
    (if *word_pair*
        (progn

            (if (null *func_def_state*) (setq parse_stack (reduce-stack parse_stack *symbol_table*)))

            ; get current (val, token)
            (setq cur_dual (car *word_pair*))

            ; consume token list, and get next token
            (setq *word_pair* (cdr *word_pair*))
            
            (setq look_ahead (if (listp *word_pair*) (car *word_pair*)))
            
            ; get current token
            (setq cur_token (car (last cur_dual)))
            (setq next_token (car (last look_ahead)))
            
            ; get previous element in the parse stack
            (setq prev_elem (car (car (last parse_stack))))
            
            (if (string= cur_token "KW_EXIT") 
                (progn
                    (format t "~%Terminating Bye.~%")
                    (exit)
                )
            )

            ; function definition state
            (if (and (not (null prev_elem)) (string= prev_elem "OP_OP") (string= cur_token "KW_DEFFUN")) 
                (setq *func_def_state* t)
            )

            ; reduce individual element in a pair (token, val)
            (setq reduced_pair (reduce-elem cur_dual prev_elem *symbol_table*))
            ; (format t "~%Reduced Pair: ")
            ; (write reduced_pair)

            (cond              
                (
                    ; conditions for reducing nested expressions
                    (and
                        (null *func_def_state*) 
                        (not (null prev_elem)) (if (not (null cur_token)) (string= cur_token "OP_OP") cur_token) 
                        (or (is-optok prev_elem) (string= prev_elem "EXP") (string= prev_elem "IDENTIFIER") (string= prev_elem "KW_PROGN")
                            (string= prev_elem "EXPB") (string= prev_elem "OP_CP") 
                            (string= prev_elem "KW_IF") (string= prev_elem "KW_EQ") (string= prev_elem "KW_GT")
                        )
                    ) 
                    (progn
                        ;(setq parse_stack (reduce-stack parse_stack))
                        (setq *word_pair* (cons (list "(" "OP_OP") *word_pair*))
                        ;(write "aaa")

                        ; call with sub_stack, and append it to the parse stack
                        (setq parse_stack (append parse_stack (gpp-parse '())))
                        (setq parse_stack (reduce-stack parse_stack *symbol_table*))

                        ;(format t "~%parse_stack: ")
                        ;(write parse_stack)    
                    )
                )
                ; else
                (t (setq parse_stack (append parse_stack (list reduced_pair))))
            )
            (gpp-parse parse_stack)       
        )

        (progn
            ; reduce stack
            (setq parse_stack (reduce-stack parse_stack *symbol_table*))
            parse_stack
        )
    )
)

; parsing function bodies
(defun fun-parse (exp_stack fun_call_stack local_symbol_table)

    ; (format t "~%Exp Stack: ")
    ; (write exp_stack)
    ; (format t "~%Fun Call Stack: ")
    ; (write fun_call_stack)
    ; (format t "~%Local Symbol Table: ")
    ; (write local_symbol_table)
    ; (format t "~%Function State:")
    ; (write *fun_call_num*)
    ; (format t "~%Symbol Table: ")
    ; (write *symbol_table*)
    ; (format t "~%")

    (if fun_call_stack
        
        (progn
            ; get current (val, token)
            (setq cur_dual (car fun_call_stack))

            ; consume token list, and get next token
            (setq fun_call_stack (cdr fun_call_stack))
            
            ; get current token
            (setq cur_token (car cur_dual))

            ; get previous element in the parse stack
            (setq prev_elem (car (car (last exp_stack))))        

            ; flip the cur_dual from (token, val) to (val, token)
            (setq temp_dual (list (car (last cur_dual)) (car cur_dual)))

            (setq exp_stack (reduce-stack exp_stack local_symbol_table))

            ; reduce individual element in a pair (token, val)
            (setq reduced_pair (reduce-elem temp_dual prev_elem local_symbol_table))

            (cond
                (
                    ; conditions
                    (and
                        (not (null prev_elem)) (if (not (null cur_token)) (string= cur_token "OP_OP") cur_token) 
                        (or (is-optok prev_elem) (string= prev_elem "EXP") (string= prev_elem "IDENTIFIER") (string= prev_elem "KW_PROGN")
                            (string= prev_elem "EXPB") (string= prev_elem "OP_CP")
                        )
                    ) 
                    (progn
                        (setq exp_stack (reduce-stack exp_stack local_symbol_table))
                        (setq fun_call_stack (cons (list "OP_OP" "(") fun_call_stack))
                        
                        ; call with sub_stack, and append it to the parse stack
                        (setq func_res (fun-parse '() fun_call_stack local_symbol_table))
                        ; (format T "~%Func Res: ")
                        ; (write func_res)
                        (setq fun_call_stack (nth 1 func_res))
                       
                        (setq exp_stack (append exp_stack (car func_res)))

                        (setq exp_stack (reduce-stack exp_stack local_symbol_table))

                        ;(format t "~%parse_stack: ")
                        ;(write parse_stack)    
                    )
                )
                ; else
                (t (setq exp_stack (append exp_stack (list reduced_pair))))
            )
            (fun-parse exp_stack fun_call_stack local_symbol_table)          
        )

        (progn
            ; reduce stack
                                 
            (setq exp_stack (reduce-stack exp_stack local_symbol_table))  
            (list exp_stack fun_call_stack)
        )        
    )
)

(defun is-optok (elem)

    (if (not (null elem))
        (or (string= elem "OP_PLUS") (string= elem "OP_MINUS") (string= elem "OP_MULT"))
    ) 
)


(defun reduce-stack (parse_stack s_table)

    (setq tokens '())
    ; get the current rule
    (setq tokens (mapcar #'(lambda(x) (append tokens (car x))) parse_stack))

    (cond 
        ; case for function
        (
            ; check for op_op, and kw_deffun
            (and 
                (not (null parse_stack)) (not (null (cdr parse_stack)))
                (string= (car (car parse_stack)) "OP_OP")
                (string= (car (car (cdr parse_stack))) "KW_DEFFUN")
            )
            ; reduce the stack
            (progn

                (if (> *fun_call_num* 0) (synt-error "Syntax Error: Function definition inside function is prohibited."))

                (setq *func_def_state* t)
                ; check if function definition is complete
                (setq is_complete (is-complete-definiton tokens))
                
                ; if it is complete, then reduce
                (if is_complete
                    (progn
                        (setq *func_def_state* nil)

                        ; get the function id
                        (setq fun_id (car (cdr (nth 2 parse_stack))))
                                  
                        ; get arglist
                        (setq arglist_pos (car (cdr is_complete)))
                        (setq arglist (subseq parse_stack 3 (1+ arglist_pos)))
                        
                        ; get explist
                        (setq explist_end_pos (car is_complete))
                        (setq explist (subseq parse_stack (1+ arglist_pos) explist_end_pos))

                        ; update function table
                        (setq is_exist (is-exist-fun *function_table* fun_id))
                        (if (null is_exist) 
                            (progn 
                                (setq *function_table* (append *function_table* (list (list fun_id arglist explist))))
                            )
                            (synt-error "Syntax Error: Function already defined.")
                        )
                        
                        ; reduce the stack
                        (setq parse_stack (subseq parse_stack (1+ explist_end_pos)))
                        parse_stack
                    )

                    parse_stack
                )
            )
        )
        (t 
            (progn

                ; reduce, and eval
                (if (null *func_def_state*) (setq parse_stack (match-rule tokens parse_stack s_table)))

                parse_stack    
            )
        )
    )
)

(defun match-rule (rule stack s_table)

    ; (found, syntactical state, start-pos, end-pos, rule)
    (setq result (list NIL NIL 0 0 "none"))

    (cond
        (
            (progn (setq result (check-rule rule)) (car result)) 
                (setq stack (eval-and-reduce stack (nth 4 result) (nth 2 result) (nth 3 result) s_table))
        )
    )
    ;(format t "~%syntax state:")
    ;(write (nth 1 result))
    
    ;(setq syntax_state (nth 1 result))
    ;(if (and (null syntax_state) (null (nth 0 result))) (synt-error "Syntax Error: Invalid syntax."))

    stack
)

; check the current situation of the syntax
; return value is a list of (found, syntactical state, start-pos, end-pos, rule)
(defun check-rule (rule)
    (setq result (list NIL 0 0))
    (setq syntax_state NIL)
    (setq match "none")
    (cond
        ((progn (setq result (is-sublist *EXP1* rule 0 0 *EXP1*)) (car result)) 
            (setq match "EXP1"))
        ((progn (setq result (is-sublist *EXP2* rule 0 0 *EXP2*)) (car result)) 
            (setq match "EXP2"))
        ((progn (setq result (is-sublist *EXP3* rule 0 0 *EXP3*)) (car result)) 
            (setq match "EXP3"))
        ((progn (setq result (is-sublist *EXP4* rule 0 0 *EXP4*)) (car result))
            (setq match "EXP4"))
        ((progn (setq result (is-sublist *EXP5* rule 0 0 *EXP5*)) (car result))
            (setq match "EXP5"))
        ((progn (setq result (is-sublist *EXP6* rule 0 0 *EXP6*)) (car result))
            (setq match "EXP6"))
        ((progn (setq result (is-explist rule 0 0 NIL)) (car result)) 
            (setq match "EXPLIST"))

        ; check boolean expression rules    
        ((progn (setq result (is-sublist *EXPB1* rule 0 0 *EXPB1*)) (car result)) 
            (setq match "EXPB1"))
        ((progn (setq result (is-sublist *EXPB2* rule 0 0 *EXPB2*)) (car result))
            (setq match "EXPB2"))
        ((progn (setq result (is-sublist *EXPB3* rule 0 0 *EXPB3*)) (car result))
            (setq match "EXPB3"))
        ((progn (setq result (is-sublist *EXPB4* rule 0 0 *EXPB4*)) (car result))
            (setq match "EXPB4"))
        ((progn (setq result (is-sublist *EXPB5* rule 0 0 *EXPB5*)) (car result))
            (setq match "EXPB5"))

        ; check function call rules *FCALL1*
        ((progn (setq result (is-sublist *FCALL1* rule 0 0 *FCALL1*)) (car result))
            (setq match "FCALL1"))
        ((progn (setq result (is-sublist *FCALL2* rule 0 0 *FCALL2*)) (car result))
            (setq match "FCALL2"))
        ((progn (setq result (is-sublist *FCALL3* rule 0 0 *FCALL3*)) (car result))
            (setq match "FCALL3"))
        ((progn (setq result (is-sublist *FCALL4* rule 0 0 *FCALL4*)) (car result))
            (setq match "FCALL4"))
    )
    ; (format t "~%result: ")
    ; (write (list (car result) syntax_state (nth 2 result) (nth 3 result) match))  
    (list (car result) syntax_state (nth 2 result) (nth 3 result) match)
)

; check if given list is a sublist of another list
(defun is-sublist (inner outer strt_pos end_pos unreduced)

    (cond
        ((null inner) (list t t strt_pos end_pos))
        ((null outer) (list NIL NIL 0 0))
        ((string= (car inner) (car outer)) 
            (progn (setq res (is-sublist (cdr inner) (cdr outer) strt_pos (1+ end_pos) unreduced)) 
                (if (car res) res 
                    (if (string= (car unreduced) (car outer))
                            (is-sublist (cdr unreduced) (cdr outer) end_pos (1+ end_pos) unreduced)
                            (is-sublist unreduced (cdr outer) (1+ end_pos) (1+ end_pos) unreduced)
                    )
                )
            )
        )
        (t (is-sublist unreduced (cdr outer) (1+ end_pos) (1+ end_pos) unreduced))
    )
)

; EXPLIST -> OP_OP KW_PROGN EXP ... OP_CP
; there could be one or more EXP
; cur_state is the current state of the algorithm, preassumption is that it is NIL at the beginning
; cur_state is set to t when current token is "OP_OP", next token is "KW_PROGN", next token is "EXP"
; cur_state is set to NIL when cur_state is t, and current token is not "EXP" or "OP_CP"
; when cur_state is t and "OP_CP" is found, return (list T T strt_pos end_pos)
; when the rule list is consumed, return (list NIL NIL 0 0)
(defun is-explist (rule strt_pos end_pos cur_state)

    (if cur_state 
    ; if cur_state is t
        (cond
            ((string= (car rule) "EXP") (is-explist (cdr rule) strt_pos (1+ end_pos) t))
            ((string= (car rule) "OP_CP") (list t t strt_pos (1+ end_pos)))
            (t (is-explist (cdr rule) (1+ end_pos) (1+ end_pos) NIL))
        )
    
    ; if cur_state is NIL, look for "OP_OP" "KW_PROGN"
        (cond
            ((null rule) (list NIL t 0 0))
            ((null (car rule)) (list NIL NIL 0 0))
            
            (
                (and (string= (car rule) "OP_OP") (string= (nth 1 rule) "KW_PROGN"))
                ; if "OP_OP" "KW_PROGN" is found, set cur_state to t
                (is-explist (cdr (cdr rule)) strt_pos (+ end_pos 2) t)
            )
            ; if not "OP_OP" "KW_PROGN", continue
            (t (is-explist (cdr rule) (1+ end_pos) (1+ end_pos) NIL))
        )
    )
)


(defun check-syntax-state-general (rule)

    (setq syntax_state NIL)
    (setq match "none")

    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXP1* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXP2* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXP3* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXP4* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXP5* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXP6* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXPB1* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXPB2* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXPB3* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXPB4* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *EXPB5* rule)))    
    (if (null syntax_state) (setq syntax_state (check-syntax-state *FCALL1* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *FCALL2* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *FCALL3* rule)))
    (if (null syntax_state) (setq syntax_state (check-syntax-state *FCALL4* rule)))
    
    ; (format t "~%syntax_state: ")
    ; (write syntax_state)

    (if (null syntax_state) (synt-error "Syntax error: invalid syntax"))
)


; if return is nil, this means syntax error
(defun check-syntax-state (rule cur_rule)

    ; (format t "~%rule: ")
    ; (write rule)
    ; (format t "~%cur_rule: ")
    ; (write cur_rule)
    
    (cond
        ((null cur_rule) t)
        ((null rule) NIL)

        ; if the current rule is "OP_OP" and the next rule is "IDENTIFIER" or "KW_PROGN", return t
        ((and (= (length cur_rule) 1) (string= (car cur_rule) "OP_OP")) t)

        ; if the current rule is "OP_OP" and the next rule is "IDENTIFIER" or "KW_PROGN", check the next rule
        (
        (and (> (length cur_rule) 1) (string= (car cur_rule) "OP_OP") 
                (or (string= (nth 1 cur_rule) "IDENTIFIER") (string= (nth 1 cur_rule) "KW_PROGN") 
                    (string= (nth 1 cur_rule) "KW_NOT") (string= (nth 1 cur_rule) "KW_AND") 
                    (string= (nth 1 cur_rule) "KW_OR") (string= (nth 1 cur_rule) "KW_EQ")
                    (string= (nth 1 cur_rule) "KW_GT") (string= (nth 1 cur_rule) "KW_WHILE")
                    (string= (nth 1 cur_rule) "KW_DEFFUN")
                )
        )
            (check-syntax-state rule (cdr (cdr cur_rule))))

        ; if the current rule is "EXP", check the next rule    
        ((string= (car cur_rule) "EXP") (check-syntax-state (cdr rule) (cdr cur_rule)))
        
        ((string= (car rule) (car cur_rule)) (check-syntax-state (cdr rule) (cdr cur_rule)))
        (t NIL)
    )
)

; *function_table*
(defun is-exist-fun (table fun_id)
    (cond
        ((null table) NIL)
        ((string= (car (car table)) fun_id) t)
        (t (is-exist-fun (cdr table) fun_id))
    )
)

(defun eval-and-reduce (stack rule_title start end s_table)

    (setq eval_seq (subseq stack start end))
    (cond
        ((string= rule_title "EXP1") (setq eval_seq (eval-exp1 eval_seq)))
        ((string= rule_title "EXP2") (setq eval_seq (eval-exp2 eval_seq)))
        ((string= rule_title "EXP3") (setq eval_seq (eval-exp3 eval_seq)))
        ((or (string= rule_title "EXP5") (string= rule_title "EXP6")) (setq eval_seq (eval-exp-asg1 eval_seq s_table)))
        ((string= rule_title "EXPLIST") (setq eval_seq (eval-explist eval_seq)))
        ((string= rule_title "EXPB1") (setq eval_seq (eval-expb-and eval_seq)))
        ((string= rule_title "EXPB2") (setq eval_seq (eval-expb-or eval_seq)))
        ((string= rule_title "EXPB3") (setq eval_seq (eval-expb-not eval_seq)))
        ((string= rule_title "EXPB4") (setq eval_seq (eval-expb-eq eval_seq)))
        ((string= rule_title "EXPB5") (setq eval_seq (eval-expb-gt eval_seq)))
        ((string= rule_title "EXP4") (setq eval_seq (eval-if-exp eval_seq)))
        
        ; eval function calls
        ((string= rule_title "FCALL1") (setq eval_seq (eval-fun1 eval_seq)))
        ((string= rule_title "FCALL2") (setq eval_seq (eval-fun2 eval_seq)))
        ((string= rule_title "FCALL3") (setq eval_seq (eval-fun3 eval_seq)))
        ((string= rule_title "FCALL4") (setq eval_seq (eval-fun4 eval_seq)))
    )
    
    ; merge evaluated result
    (setq result stack)
    
    ; (format t "~%stack: ")
    ; (write stack)
    ; (format t "~%eval_seq: ")
    ; (write eval_seq)
    
    ; (format t "~%append: ")
    ; (write (append (subseq stack 0 start) eval_seq))    

    (if (= start 0) (setq result eval_seq) (progn (setq result (subseq stack 0 start)) (setq result (append result eval_seq))))
    ;; (format t "~%result1: ")
    ;; (write result)    
    (if (= end (length stack)) () (setq result (append result (subseq stack end))))
    ;; (write (list "~%RESULT: " start end (= end (1- (length stack)))))
    ;; (format t "~%result2: ")
    ;; (write result)    
    
    result
)

(defun eval-parse-stack (parse_stack)

    ; eval from left to right, if there is EXP, then print its value without ""
    (cond
        ((null parse_stack) ())
        ((string= (car (car parse_stack)) "EXP") (progn (format t "~%~a" (nth 1 (car parse_stack))) (eval-parse-stack (cdr parse_stack))))
        (t parse_stack)
    )
)

(defun eval-exp1 (seq)
    ; calculate the result
    (setq lhs (nth 1 (nth 2 seq)))
    (setq rhs (nth 1 (nth 3 seq)))

    (setq res (fraction+ lhs rhs))

    (list (list "EXP" res))
)

(defun eval-exp2 (seq)
    ; calculate the result
    (setq lhs (nth 1 (nth 2 seq)))
    (setq rhs (nth 1 (nth 3 seq)))

    (setq res (fraction- lhs rhs))

    (list (list "EXP" res))
)

(defun eval-exp3 (seq)
    ; calculate the result
    (setq lhs (nth 1 (nth 2 seq)))
    (setq rhs (nth 1 (nth 3 seq)))

    (setq res (fraction* lhs rhs))

    (list (list "EXP" res))
)

; checks if given identifier exists in the symbol table
; if not, add it to the symbol table
; if yes, update its value
(defun eval-exp-asg1 (seq s_table)

    (setq id (nth 1 (nth 2 seq)))
    (setq val (nth 1 (nth 3 seq)))
    (save-symbol-table s_table id val)

    (list (list "EXP" val))
)

; use *symbol_table* to store the symbol table
; *symbol_table* is a list of (id val) pairs
; returns the value of the given identifier
(defun save-symbol-table (s_table id val)
    
    ; check if the id exists in the symbol table
    ; get the position of the id in the symbol table

    (cond 
        ((> *fun_call_num* 0)
            (setq is_exist (update-id s_table id val))
            (if (null is_exist) (progn (setf s_table (append local_symbol_table (list (list id val)))) val))
        )
        (t
            (setq is_exist (update-id *symbol_table* id val))
            (if (null is_exist) (progn (setq *symbol_table* (append *symbol_table* (list (list id val)))) val))
        )
    )

    ; (setq is_exist (update-id *symbol_table* id val))
    ; (if (null is_exist) (progn (setq *symbol_table* (append *symbol_table* (list (list id val)))) val))
)

; check if the given id exists in the symbol table
; if it exists, update its value
(defun update-id (table id val)
    (cond
        ((null table) nil)
        ((string= (car (car table)) id) (progn (setf (car (cdr (car table))) val) (car (cdr (car table)))))
        (t (update-id (cdr table) id val))
    )
)

; returns the value of the given identifier
(defun get-value-from-table (table id)
    (cond
        ((null table) nil)
        ((string= (car (car table)) id) (car (cdr (car table))))
        (t (get-value-from-table (cdr table) id))
    )
)

; evaluate the if expression
(defun eval-if-exp (seq)

    (setq op_str (nth 1 (nth 2 seq)))
    (setq lhs (nth 1 (nth 3 seq)))
    (setq rhs (nth 1 (nth 4 seq)))

    (setq res (if (string= op_str "true") lhs rhs))
    (list (list "EXP" res))
)

; returns the last expression in the list
(defun eval-explist (seq)

    (list (nth (- (length seq) 2) seq))
)

; fraction addition
(defun fraction+ (lhs rhs)
    ; get the nominator of lhs using get-nom-denom-list function
    (setq lhs_list (get-nom-denom-list lhs))
    (setq lhs_nom (nth 0 lhs_list))
    (setq lhs_denom (nth 1 lhs_list))

    ; get the nominator of rhs using get-nom-denom-list function
    (setq rhs_list (get-nom-denom-list rhs))
    (setq rhs_nom (nth 0 rhs_list))
    (setq rhs_denom (nth 1 rhs_list))

    ; calculate the result
    (setq nom_res (+ (* lhs_nom rhs_denom) (* rhs_nom lhs_denom)))
    (setq denom_res (* lhs_denom rhs_denom))

    ; simplify
    (setq res_list (simplify-fract nom_res denom_res))
    (setq nom_res (nth 0 res_list))
    (setq denom_res (nth 1 res_list))
    
    ; convert to string in format: "nomfdenom"
    (setq res (get-frac-str nom_res denom_res))
    res
)

; fraction subtraction
(defun fraction- (lhs rhs)
    ; get the nominator of lhs, to nom_str_lhs example: "3f2" -> 3
    (setq lhs_list (get-nom-denom-list lhs))
    (setq lhs_nom (nth 0 lhs_list))
    (setq lhs_denom (nth 1 lhs_list))

    ; get the nominator of rhs, to nom_str_rhs example: "3f2" -> 3
    (setq rhs_list (get-nom-denom-list rhs))
    (setq rhs_nom (nth 0 rhs_list))
    (setq rhs_denom (nth 1 rhs_list))

    ; calculate the result
    (setq nom_res (- (* lhs_nom rhs_denom) (* rhs_nom lhs_denom)))
    (setq denom_res (* lhs_denom rhs_denom))

    ; simplify
    (setq res_list (simplify-fract nom_res denom_res))
    (setq nom_res (nth 0 res_list))
    (setq denom_res (nth 1 res_list))

    ; convert to string in format: "nomfdenom"
    (setq res (get-frac-str nom_res denom_res))
    res
)

; fraction multiplication
(defun fraction* (lhs rhs)
    ; get the nominator of lhs, to nom_str_lhs example: "3f2" -> 3
    (setq lhs_list (get-nom-denom-list lhs))
    (setq lhs_nom (nth 0 lhs_list))
    (setq lhs_denom (nth 1 lhs_list))

    ; get the nominator of rhs, to nom_str_rhs example: "3f2" -> 3
    (setq rhs_list (get-nom-denom-list rhs))
    (setq rhs_nom (nth 0 rhs_list))
    (setq rhs_denom (nth 1 rhs_list))

    ; calculate the result
    (setq nom_res (* lhs_nom rhs_nom))
    (setq denom_res (* lhs_denom rhs_denom))

    ; simplify
    (setq res_list (simplify-fract nom_res denom_res))
    (setq nom_res (nth 0 res_list))
    (setq denom_res (nth 1 res_list))

    ; convert to string in format: "nomfdenom"
    (setq res (get-frac-str nom_res denom_res))
    res
)

; get the nominator and denominator of a fraction
(defun get-nom-denom-list (str)
    (setq nom_str (subseq str 0 (position #\f str)))
    (setq denom_str (subseq str (1+ (position #\f str))))

    ;convert to integer
    (setq nom (parse-integer nom_str))  
    (setq denom (parse-integer denom_str))
    
    ;return integer list
    (list nom denom)
)

; simplify the fraction
(defun simplify-fract (nom denom)
    (setq gcd (gcd nom denom))
    (setq nom (/ nom gcd))
    (setq denom (/ denom gcd))
    (list nom denom)
)

; convert to string in format: "nomfdenom"
(defun get-frac-str (nom denom)
    (setq nom_str (write-to-string nom))
    (setq denom_str (write-to-string denom))
    (setq frac_str (concatenate 'string nom_str "f" denom_str))
    frac_str
)

; and boolean expression
(defun eval-expb-and (seq)
    (setq val1 (nth 1 (nth 2 seq)))
    (setq val2 (nth 1 (nth 3 seq)))
    (setq op1 NIL)
    (setq op2 NIL)

    (cond ((string= val1 "true") (setq op1 t)))
    (cond ((string= val2 "true") (setq op2 t)))

    (setq res (and op1 op2))
    (setq res_str (if res "true" "false"))
    (list (list "EXPB" res_str))    
)

; or boolean expression
(defun eval-expb-or (seq)

    (setq val1 (nth 1 (nth 2 seq)))
    (setq val2 (nth 1 (nth 3 seq)))
    (setq op1 NIL)
    (setq op2 NIL)

    (cond ((string= val1 "true") (setq op1 t)))
    (cond ((string= val2 "true") (setq op2 t)))

    (setq res (or op1 op2))
    (setq res_str (if res "true" "false"))
    (list (list "EXPB" res_str))
)

; not boolean expression
(defun eval-expb-not (seq)

    (setq val1 (nth 1 (nth 2 seq)))
    (setq op1 t)

    (cond 
        ((string= val1 "true") (setq op1 t))
        ((string= val1 "false") (setq op1 nil))
    )

    (setq res (not op1))
    (setq res_str (if res "true" "false"))
    (list (list "EXPB" res_str))  
)

; equal boolean expression
(defun eval-expb-eq (seq)
    
        (setq val1 (nth 1 (nth 2 seq)))
        (setq val2 (nth 1 (nth 3 seq)))
        (setq op t)
    
        (setq val1_list (get-nom-denom-list val1))
        (setq val1_nom (nth 0 val1_list))
        (setq val1_denom (nth 1 val1_list))
    
        (setq val2_list (get-nom-denom-list val2))
        (setq val2_nom (nth 0 val2_list))
        (setq val2_denom (nth 1 val2_list))
    
        (setq val1_nom (* val1_nom val2_denom))
        (setq val2_nom (* val2_nom val1_denom))
    
        (cond 
            ((/= val1_nom val2_nom) (setq op nil))
        )
    
        (setq res_str (if op "true" "false"))
        (list (list "EXPB" res_str))  
)

; greater than boolean expression
(defun eval-expb-gt (seq)

    (setq val1 (nth 1 (nth 2 seq)))
    (setq val2 (nth 1 (nth 3 seq)))
    (setq op t)

    (setq val1_list (get-nom-denom-list val1))
    (setq val1_nom (nth 0 val1_list))
    (setq val1_denom (nth 1 val1_list))

    (setq val2_list (get-nom-denom-list val2))
    (setq val2_nom (nth 0 val2_list))
    (setq val2_denom (nth 1 val2_list))

    (setq val1_nom (* val1_nom val2_denom))
    (setq val2_nom (* val2_nom val1_denom))

    (cond 
        ((> val1_nom val2_nom) (setq op t))
        ((<= val1_nom val2_nom) (setq op nil))
    )

    (setq res_str (if op "true" "false"))
    (list (list "EXPB" res_str))
)

; no arg function
; seq = ("OP_OP" "IDENTIFIER" "OP_CP")
(defun eval-fun1 (seq)

    (setq fun_id (nth 1 (nth 1 seq)))

    ; get function properties from function table
    (setq fun_props (get-fun-props fun_id *function_table*))
    (if (null fun_props) (id-error fun_id))

    (setq arglist (nth 1 fun_props))
    
    (setq explist (nth 2 fun_props))

    ; check if arglist matches the number of args in the function call
    (if (/= (length arglist) 2) (synt-error "Syntax Error: incompatible arguments"))


    (setq fun_call_stack explist)
    (setq local_symbol_table (list ))
    (setq *fun_call_num* (1+ *fun_call_num*))

    (setq res (fun-parse (list ) fun_call_stack local_symbol_table))

    (setq *fun_call_num* (1- *fun_call_num*))

    (car res)   
)

; one arg function
(defun eval-fun2 (seq)

    (setq fun_id (nth 1 (nth 1 seq)))

    ; get function properties from function table
    (setq fun_props (get-fun-props fun_id *function_table*))
    (if (null fun_props) (id-error fun_id))

    (setq arglist (nth 1 fun_props))
    (setq explist (nth 2 fun_props))
    ; check if arglist matches the number of args in the function call
    (if (/= (length arglist) 3) (synt-error "Syntax Error: incompatible arguments"))
    ; set the value of the argument in the arglist
    (setq arg_val (nth 1 (nth 2 seq)))


    (setq fun_call_stack explist)
    (setq local_symbol_table (list (list (nth 1 (nth 1 arglist)) arg_val)))
    (setq *fun_call_num* (1+ *fun_call_num*))

    (setq res (fun-parse (list ) fun_call_stack local_symbol_table))

    (setq *fun_call_num* (1- *fun_call_num*))

    (car res)
)

; two arg function
(defun eval-fun3 (seq)

    (setq fun_id (nth 1 (nth 1 seq)))

    ; get function properties from function table
    (setq fun_props (get-fun-props fun_id *function_table*))

    (if (null fun_props) (id-error fun_id))

    (setq arglist (nth 1 fun_props))

    ; check if arglist matches the number of args in the function call

    (if (/= (length arglist) 4) (synt-error "Syntax Error: incompatible arguments"))

    ; create local symbol table
    (setq arg_val1 (nth 1 (nth 2 seq)))
    (setq arg_val2 (nth 1 (nth 3 seq)))
    (setq explist (nth 2 fun_props))

    (setq fun_call_stack explist)
    (setq local_symbol_table (list (list (nth 1 (nth 1 arglist)) arg_val1) (list (nth 1 (nth 2 arglist)) arg_val2)))
    (setq *fun_call_num* (1+ *fun_call_num*))

    (setq res (fun-parse (list ) fun_call_stack local_symbol_table))

    (setq *fun_call_num* (1- *fun_call_num*))

    ; (format t "~%resx:")
    ; (write res)

    (car res)
)

; three arg function
(defun eval-fun4 (seq)

    (setq fun_id (nth 1 (nth 1 seq)))

    ; get function properties from function table

    (setq fun_props (get-fun-props fun_id *function_table*))

    (if (null fun_props) (id-error fun_id))

    (setq arglist (nth 1 fun_props))

    ; check if arglist matches the number of args in the function call

    (if (/= (length arglist) 5) (synt-error "Syntax Error: incompatible arguments"))
    (setq explist (nth 2 fun_props))

    ; create local symbol table
    (setq arg_val1 (nth 1 (nth 2 seq)))
    (setq arg_val2 (nth 1 (nth 3 seq)))
    (setq arg_val3 (nth 1 (nth 4 seq)))

    (setq fun_call_stack explist)
    (setq local_symbol_table (list (list (nth 1 (nth 1 arglist)) arg_val1) (list (nth 1 (nth 2 arglist)) arg_val2) (list (nth 1 (nth 3 arglist)) arg_val3)))
    (setq *fun_call_num* (1+ *fun_call_num*))

    (setq res (fun-parse (list ) fun_call_stack local_symbol_table))

    (setq *fun_call_num* (1- *fun_call_num*))

    (car res)
)

(defun get-fun-props (fun_id fun_table)
    (cond
        ((null fun_table) nil)
        ((string= (nth 0 (car fun_table)) fun_id) (car fun_table))
        (t (get-fun-props fun_id (cdr fun_table)))
    )
)
; pair = <val, token>
; returned pair = <token, val>
(defun reduce-elem (pair prev_elem s_table)

    (setq token (car (last pair)))
    (setq val "empty")
    (cond 
        ((string= token "VALUEF") (progn (setq token "EXP") (setq val (car pair))))
        ((string= token "EXP") (setq val (car pair)))
        
        ((string= token "IDENTIFIER") 
            (progn 
                (if (or (string= prev_elem "KW_SET") (string= prev_elem "KW_DEFVAR") (string= prev_elem "KW_DEFFUN") *func_def_state* (string= prev_elem "OP_OP")) 
                    (setq val (car pair))
                    (progn
                        ;(write "here")
                        ; (setq val (get-value-from-table *symbol_table* (car pair)))
                        (setq val 
                            (if *fun_call_num* 
                                (get-value-from-table s_table (car pair)) 
                                (get-value-from-table *symbol_table* (car pair))
                            )
                        )
                        (if (null val) (id-error (car pair)) (setq token "EXP"))
                    )
                )       
            )
        )
        (
            (or (string= token "KW_TRUE") (string= token "KW_FALSE"))
            (progn (setq token "EXPB") (setq val (car pair)))
        )
    )
    ; (format t "~%resultaaa: ")
    ; (write (list token val))
    ; (format t "~%pair: ")
    ; (write pair)
    ; (format t "~%prev_elem: ")
    ; (write prev_elem)

    (list token val)
)

(defun id-error (id)
    (format t "Syntax Error: ~a is not defined~%" id)
    (exit)
)

(defun synt-error (msg)
    (format t "~a~%" msg)
    (exit)
)

; seq = list of pairs
; check if given sequence has a complete function definition
; return true if it is complete, false otherwise
; function definition is complete if it consists of <"OP_OP" "KW_DEFFUN" "IDENTIFIER" "OP_OP" (|"IDENTIFIER"| "IDENTIFIER IDENTIFIER" | "IDENTIFIER IDENTIFIER IDENTIFIER") "OP_CP" "OP_OP" "OP_CP" "OP_OP" "OP_CP" "OP_CP">
;"OP_CP" "OP_OP" "KW_PROGN" ("EXP" ...) "OP_CP">
(defun is-complete-definiton (rule)
    (cond
        (
            (and (> (length rule) 2) (not (null (car (cdr (cdr rule))))) (string/= (car (cdr (cdr rule))) "IDENTIFIER")) 
            (synt-error "Syntax Error: function has invalid name")
        )
        (
            (and 
                (string= (car rule) "OP_OP")
                (string= (car (cdr rule)) "KW_DEFFUN")
                (not (null (car (cdr (cdr rule))))) (string= (car (cdr (cdr rule))) "IDENTIFIER")
            )

            (progn
                ;reduce the first three elements
                (setq len (length rule))
                (setq rule (cdr (cdr (cdr rule))))
                
                (if (< len 5) (return-from is-complete-definiton nil))
                
                (setq arglist1 (list "OP_OP" "OP_CP")) ; 0 arguments
                (setq arglist2 (list "OP_OP" "IDENTIFIER" "OP_CP")) ; 1 argument
                (setq arglist3 (list "OP_OP" "IDENTIFIER" "IDENTIFIER" "OP_CP")) ; 2 arguments
                (setq arglist4 (list "OP_OP" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" "OP_CP")) ; 3 arguments
                (setq arg_count 0)
                ;(write rule)
                ;(write len)
                (cond
                    ((and (> len 4) (equal (subseq rule 0 2) arglist1)) (setq arg_count 0))
                    ((and (> len 5) (equal (subseq rule 0 3) arglist2)) (setq arg_count 1))
                    ((and (> len 6) (equal (subseq rule 0 4) arglist3)) (setq arg_count 2))
                    ((and (> len 7) (equal (subseq rule 0 5) arglist4)) (setq arg_count 3))
                    
                    (t 
                        (progn
                            ; check for the error
                            (setq temp_rule nil)
                            (cond
                                ((> len 7) (setq temp_rule (subseq rule 0 5)))
                                ((> len 6) (setq temp_rule (subseq rule 0 4)))
                                ((> len 5) (setq temp_rule (subseq rule 0 3)))
                                ((> len 4) (setq temp_rule (subseq rule 0 2)))
                                
                                (t (return-from is-complete-definiton nil))
                            )
                            ;(write (invalid-arglist temp_rule))
                            (if (invalid-arglist temp_rule) (synt-error "Syntax Error: invalid argument list"))
                            (return-from is-complete-definiton nil)
                        )
                    )
                )
                (setq arglist_end_pos (+ arg_count 4))
                (setq rule (subseq rule (+ arg_count 2)))
                
                (if (< (length rule) 1) (return-from is-complete-definiton nil))
                
                ; check for the expression list
                (setq explist_pos (check-explist rule))

                (if (null explist_pos) (return-from is-complete-definiton nil))
                (setq explist_pos (+ arglist_end_pos explist_pos))
                (list explist_pos arglist_end_pos)
            )
        )
    )
)

(defun check-explist (rule)
    ; check for op_op and kw_progn first
    (cond
        (
            (and 
                (> (length rule) 1) (string= (car rule) "OP_OP") (string= (car (cdr rule)) "KW_PROGN")
            )
            (progn
                (setq rule (cdr (cdr rule)))
                (if (< (length rule) 1) (return-from check-explist nil))
                (setq pos (check-explist-helper rule 2 0))
                (if (= pos -1) (return-from check-explist nil))
                (+ pos 2)
            )
        )
        (t (synt-error "Syntax Error: function has invalid expression list"))
    )
)

; recursive function to check the expression list
; check if parantheses are balanced
; if it is 0, return pos
(defun check-explist-helper (rule p_count pos)
    (cond
        ((= p_count 0) pos)
        ((null rule) -1)
        ((string= (car rule) "OP_OP") (check-explist-helper (cdr rule) (1+ p_count) (1+ pos)))
        ((string= (car rule) "OP_CP") (check-explist-helper (cdr rule) (1- p_count) (1+ pos)))
        (t (check-explist-helper (cdr rule) p_count (1+ pos)))
    )
)

; checks syntax error
(defun invalid-arglist (rule)
    (setq len (length rule))
    ;(write rule)
    ; first element
    (if (string/= (car rule) "OP_OP") (return-from invalid-arglist t))
    (if (and (= len 5) (string/= (car (last rule)) "OP_CP")) (return-from invalid-arglist t))
    (setq rule (cdr rule))

    (setq res (mapcar #'(lambda(x) (string= x "IDENTIFIER")) rule))
    ;(write (car res))
    (if (car res) nil t)
)

(gpp-driver)
