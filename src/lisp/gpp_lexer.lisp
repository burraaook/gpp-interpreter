(defvar key_words (list "and" "or" "not" "eq" "gt"
                        "set" "deffun" "defvar" "if" "exit"
                        "true" "false" "exit" "progn" "while"))
(defvar operators (list "+" "-" "*" "(" ")"))

; states for tokens dfa
(defvar state "start")
(defvar *error_state* "initial")

; states for fractional numbers
(defvar numpre_state 0)
(defvar numpos_state 0)
(defvar f_state 0)



; states for identifier
(defvar prealpha_state 0)
(defvar preunderscore_state 0)

; state for comment
(defvar comment_state 0)

; leading zero state
(defvar leading_zero 0)

(defun gpp-lexer (&optional file_name)

    ; get the file in string format
    (if file_name (setq line (read-file file_name)) (progn (format t "~%~%>> ") (setq line (read-line))))
    
    ; trim
    (setq line (string-trim '(#\Space #\Tab #\Return #\Newline) line))  
    (setq word_pair (list ))
    
    ; get the pair list
    (setq word_pair (split-words word_pair line 0))

    ; check error state
    (cond 
        ((string/= *error_state* "initial") (progn (format t "LEXICAL ERROR: ~S cannot be tokenized" *error_state*) (exit)))
    )    

    ; print the pair list 
    ;(if word_pair (write word_pair))
    (reset-states)
    word_pair 
)

(defun split-words (word_pair line cur)

    ; terminate if there is no character
    (cond
        ((>= cur (length line)) (return-from split-words word_pair))
    )
    
    (setq token "")
 
    ; get the current token, increases the cur
    (setq token (extract-token line token cur 0))

    ; increment current
    (setq cur (+ cur (length token)))
    (setq token (string-trim '(#\Space #\Tab #\Return #\NEWLINE) token))

    (cond
        ((string/= token "") (set-final-state token))
    )

    (cond
        ((and (string= state "start") (string/= token "")) 
        (progn (setq *error_state* token) (return-from split-words word_pair)))
        
        ((and (string/= token "") (string/= state "COMMENT")) (setq word_pair
                                    (append word_pair 
                                    (list (list token state)))))                     
    )

    (reset-states)
    (split-words word_pair line cur)
)

(defun extract-token (line token cur prev)
    (cond 
        ((>= cur (length line)) (return-from extract-token token))
    )

    (if (>= cur (1- (length line)))
        (setq temp (subseq line cur)) 
    (setq temp (subseq line cur (1+ cur))))
    
    (setq stop (is-stop temp))
     
    (cond
        ((and (char= (char line cur) #\NEWLINE) (= comment_state 1)) (return-from extract-token token))
        ((= comment_state 1) (setq token (concatenate 'string token temp)))
        ((and (= prev 0) (string= temp ";") (= (next-char line cur #\;) 1) ) (progn (setq comment_state 1)
                                                                         (setq token (concatenate 'string token temp))))
        ((and (= prev 0) (string= temp "*") (= (next-char line cur #\*) 1)) (return-from extract-token (subseq line cur (+ cur 2))))

        ((= stop 0) (setq token (concatenate 'string token temp)))
        ((and (= stop 1) (= prev 0)) (return-from extract-token (concatenate 'string token temp)))
        ((= prev 1) (return-from extract-token token))
    )
    
    ;(write-line token)
    (extract-token line token (1+ cur) 1)
)

; read file into a whole string
(defun read-file (file_name)
    (with-output-to-string (content)
        (with-open-file (in file_name)
            (loop with buffer = (make-array 8192 :element-type 'character)
                for num_char = (read-sequence buffer in)
                while (< 0 num_char)
                do (write-sequence buffer content :start 0 :end num_char))
        )
    )
)

; checks if next char is given char
(defun next-char (line cur chr)
    (cond
        ((<= (length line) (1+ cur)) (return-from next-char 0))
        ((char= (char line (1+ cur)) chr) (return-from next-char 1))
    )
    (return-from next-char 0)
)

; sets all states
(defun set-final-state (token)

    ; set state of tokens
    (cond
        ((= comment_state 1) (setq state "COMMENT"))
        ((is-keyword token) (set-keyword-state token))
        ((is-operator token) (set-operator-state token))
        ((= (is-valuef token 0) 1) (setq state "VALUEF"))         
        ((= (is-identifier token 0) 1) (setq state "IDENTIFIER"))
    )
)

; checks if given token is stop token
(defun is-stop (token)
    (cond
        ((or (string= token " ") (char= (char token 0) #\NEWLINE) (string= token ";") (is-operator token)) (return-from is-stop 1))
    )
    (return-from is-stop 0)
)

; checks if given token is the element of the container list
(defun is-element (container token)
    (if (member token container :test #'string=) t nil)
)

; checks if given token is keyword
(defun is-keyword (token)
    (is-element key_words token)
)

; checks if given token is operator
(defun is-operator (token)
    (is-element operators token)
)

; sets the state of the keyword
(defun set-keyword-state (token)
    (setq state (concatenate 'string "KW_" (string-upcase token)))
)

; sets the state if token is operator
(defun set-operator-state (token)
    (cond
        ((string= token "+") (setq state "OP_PLUS"))
        ((string= token "-") (setq state "OP_MINUS"))
        ((string= token "*") (setq state "OP_MULT"))
        ((string= token "(") (setq state "OP_OP"))
        ((string= token ")") (setq state "OP_CP"))
    )
)

; returns 1 if token has leading zero
(defun is-leading-zero (token)
    (if (<= (length token) 1)
        (return-from is-leading-zero 0)
        (if (and (char= (char token 0) #\0) (digit-char-p (char token 1))) 1 0)
    )
)

; dfa for fractional values
(defun is-valuef (token cur)
    (cond
    ((and (= cur 0) (= (is-leading-zero token) 1)) (return-from is-valuef 0))
        ((< (length token) 3) (return-from is-valuef 0))
        ((>= cur (length token)) (return-from is-valuef (and numpre_state f_state numpos_state)))
        ((digit-char-p (char token cur)) (cond
                                            ((= f_state 0) (setq numpre_state 1))
                                            ((= f_state 1) (setq numpos_state 1))))
        ((and (char= #\f (char token cur)) (= (is-leading-zero (subseq token (1+ cur))) 0)) (progn (cond 
                                            ((= f_state 1) (return-from is-valuef 0))
                                            ((= numpre_state 0) (return-from is-valuef 0)))
                                            (setq f_state 1)))
        ((alpha-char-p (char token cur)) (return-from is-valuef 0))
    )
    (is-valuef token (1+ cur))
)

; dfa for identifiers
(defun is-identifier (token cur)


     
    (cond 
        ((>= cur (length token))(return-from is-identifier (or prealpha_state preunderscore_state)))

        ((not (or (alpha-char-p (char token cur)) (digit-char-p (char token cur)) (char= #\_ (char token cur)))) 
            (progn (format t "LEXICAL ERROR: ~S cannot be tokenized" token) (exit)))
        ((char= #\_ (char token cur)) (setq preunderscore_state 1))
        ((alpha-char-p (char token cur)) (setq prealpha_state 1))
        ((digit-char-p (char token cur)) (cond
                                        ((and (= prealpha_state 0) (= preunderscore_state 0)) (return-from is-identifier 0))))
    )

    (is-identifier token (1+ cur))
)

; resets all of the states
(defun reset-states ()
    (setq state "start")
    (setq numpre_state 0)
    (setq numpos_state 0)
    (setq f_state 0)
    (setq prealpha_state 0)
    (setq preunderscore_state 0)    
    (setq comment_state 0)
    (setq leading_zero 0)
)