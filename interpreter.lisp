;**********************************
;* CSE341 - Programming Languages *
;* 161044105 HALİL İBRAHİM KÖSE   *
;**********************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar RULES '(

	("and" "EXPB" "EXPB") 
	("or" "EXPB" "EXPB") 
	("not" "EXPB") 
	("equal" "EXPI" "EXPI") 
	("equal" "EXPB" "EXPB") 
	("less" "EXPI" "EXPI") 
	("less" "EXPB" "EXPB") 
	"nil"
	("list" "VALUES")
	("list" "‘()" )
	("list" "null" )
	("append" "EXPI" "EXPLISTI")
	("concat" "EXPLISTI" "EXPLISTI")
	("set" "ID" "EXPI") 
	("deffun" "ID" "IDLIST" "EXPLISTI") 
	("for" "(" "ID" "EXPI" "EXPI" ")" "EXPLISTI") 
	("if" "EXPB" "EXPLISTI" "EXPLISTI") 
	("if" "EXPB" "EXPLISTI") 


	("+" "EXPI" "EXPI")
	("-" "EXPI" "EXPI") 
	("*" "EXPI" "EXPI") 
	("**" "EXPI" "EXPI")
	("/" "EXPI" "EXPI") 
	("'(" "VALUES" ")") 
	("“" "COMMENT" "”")
	(";;" "COMMENTLINE")
	("disp" "ID" "EXPI") 
	("ID" "EXPLISTI")
	  "ID" "VALUES "
	"exit" 
	"load"
	"disp"
	"true"
	"false"
	
	("BinaryValue" "true")
	("BinaryValue" "false")
	"EXPI"
	"integer"
	"IDLIST" 
	"keyword"
	"()"
))

(setq TempInput '())

(setq KEYWORDS '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))

(setq OPERATORS '("+" "-" "/" "*" "(" ")" "**" "“" "”" "," ";;"))

; Read file helper function
(defun read-recursive (strIn strOut)
	(let ((char (read-char strIn nil)))
		(unless (null char)
		  (format strOut "~c" char)
		  (read-recursive strIn strOut))
	)
)

; Read file
(defun read-file (fileName)
	(with-open-file (str fileName)
	  	(with-output-to-string (str-Output)
		    (read-recursive str str-Output)
	    	str-Output
    	)
	)
)

; Delete the "" in the list
(defun deleteEmpty (inList outList)
	(setq firstElement (car inList))
	(if (equal firstElement nil)
		outList
		(if (equal firstElement "")
			(deleteEmpty (cdr inList) outList)
			(progn 
				(setq outList (append outList (list firstElement)))
				(deleteEmpty (cdr inList) outList)
			)
		)
	)
)

; Space Tap and Newline to token separator
(defun tokenizer (string &optional (separator '(#\Space #\Tab #\Newline)))
	(deleteEmpty (tokenizer-helper  string separator) '())
)

(defun tokenizer-helper (string &optional (separator '(#\Space #\Tab #\Newline)) (r nil))
  	(let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
	    (if n
			(tokenizer-helper  (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
		    	(cons string r)
	    )
   	)
)

; The parenthesis are checked and spaces are placed.
(defun check-Parenthesis-And-Add-Space (returnStr readStrList)
	(setq firstElement (car readStrList))
	(if (equal firstElement nil)
		returnStr
		(if (equal (string firstElement) "(")
			(progn
				(setq returnStr (concatenate 'string returnStr (string firstElement)))
				(setq returnStr (concatenate 'string returnStr " "))
				(check-Parenthesis-And-Add-Space returnStr (cdr readStrList))	
			)
			
			(if (equal (string firstElement) ")")
				(progn
					(setq returnStr (concatenate 'string returnStr " "))
					(setq returnStr (concatenate 'string returnStr (string firstElement)))
					(check-Parenthesis-And-Add-Space returnStr (cdr readStrList))
				)
				(progn
					(setq returnStr (concatenate 'string returnStr  (string firstElement)))
					(check-Parenthesis-And-Add-Space returnStr (cdr readStrList))
				)
			)
		)
	)
)

; Identifier control
(defun isIdentifier (strList)
	(setq val (car strList))
	(if (equal val nil)
		t
		(if (or (and (<= (char-code #\a) (char-code val)) (<= (char-code val) (char-code #\z))) (and (<= (char-code #\A) (char-code val)) (<= (char-code val) (char-code #\Z))))
			(isIdentifier (cdr strList))
			nil
		)
	)
)

; Negative integer value control
(defun isNegIntVal (negList)
	(setq val (car negList))
	(if (equal val nil)
		t
		(if (and (<= (char-code #\1) (char-code val)) (<= (char-code val) (char-code #\9)))
			(isPosIntVal (cdr negList))
			nil
		)
	)
)

; Pozitive integer value control
(defun isPosIntVal (posList)
	(setq val (car posList))
	(if (equal val nil)
		t
		(if (and (<= (char-code #\0) (char-code val)) (<= (char-code val) (char-code #\9)))
			(isPosIntVal (cdr posList))
			nil
		)
	)
)

; Integer value control
(defun isIntegerValue (strList)
	(if (equal "-" (string (car strList)))
		(isNegIntVal (concatenate 'list (cdr strList)))
		(isPosIntVal (concatenate 'list strList))
	)
)

; Determines which type of tokens (KEYWORDS, OPERATORS, BINARYVALUE, IDENTIFIER, INTEGERVALUE)
(defun typeCheck (returnList lexerL)
	(setq token (car lexerL))
	(if (equal token nil)
		returnList
		(if (find token KEYWORDS :test #'string=)
			(progn
				(setq returnList (append returnList (list (list "KW" token))))
				(typeCheck returnList (cdr lexerL))
			)
			(if (find token OPERATORS :test #'string=)
				(progn
					(setq returnList (append returnList (list (list "OP" token))))
					(typeCheck returnList (cdr lexerL))
				)
					(if (isIdentifier (concatenate 'list token))
						(progn
							(setq returnList (append returnList (list (list "IDENTIFIER" token))))
							(typeCheck returnList (cdr lexerL))
						)
						(if (isIntegerValue (concatenate 'list token))
							(progn
								(setq returnList (append returnList (list (list "VALUE" token))))
								(typeCheck returnList (cdr lexerL))
							)
							token
						)
					)
			)
		)
	)
)

(defun lexer (fileName)
(setq read-str  fileName)
(if (equal (subseq fileName 0 4) "load") (setq read-str (read-file (subseq fileName 5))))

	
	; The file name is taken from the user.
	(setq lexerList (tokenizer (check-Parenthesis-And-Add-Space "" (concatenate 'list read-str)))) ; The parenthesis are checked and spaces are placed. Then token operation is performed.
	;(write lexerList)
	;(terpri)
	;(terpri)
	(setq resultList (typeCheck '() lexerList)) ; Determines which type of tokens
	(if (listp resultList) resultList	; Error control
		;(write resultList)
		;resultList
		(progn 
			(write-string "Error: ")
			(write resultList)
			(write-string " -> Lexical analysis failed. Because there is an expression that does not conform to the structure of IntegerValue or Identifier.")
			(write "Usage =>  IntegerValue -> [-]*[1-9]*[0-9]+ OR Id -> [a-zA-Z]+")
			(write-string " -> SYNTAX_ERROR Expression not recognized")
			(terpri)
			(parser)
		)
	)

)

; Fonksiyonun baslangic ve bitis parantezlerini belirler 
(defun countBracket(bracketList bracketCheck)
	(setq bracket (car bracketList))
	(if (equal bracket nil)
		0
		(if (or (equal(car (cdr bracket)) "(") (equal(car (cdr bracket)) "'("))
			(+ 1 (countBracket (cdr bracketList) (+ 1 bracketCheck)))
			(if (equal(car (cdr bracket)) ")")
				(if (= (- bracketCheck 1) 0)
					1
					(+ 1 (countBracket (cdr bracketList) (- bracketCheck 1)))
				)
				(+ 1 (countBracket (cdr bracketList) bracketCheck))
			)
		)
	)
)

; Hangi Token'in hangi kurala uydugunu buluyor  (+ 4 5)  -> (+ EXPI EXPI)
(defun checkRule(checkliste ruleList) 
	(setq rule (car ruleList))
	(if (not rule)
		nil
		(if (listp rule)
			(if (equal (car rule) (car (cdr (car checkliste))))
				rule
				(checkRule checkliste (cdr ruleList))
			)
			(if (equal rule (car (cdr (car checkliste))))
				rule
				(checkRule checkliste (cdr ruleList))
			)
		)
	)	
)

; Gelen token kurallar listesinde liste seklinde mi yoksa string seklinde mi kontrol ediyor
(defun checkNotList(cnlliste cnlstring)
	(setq item (car cnlliste))
	(if (equal item nil)
		nil
		(if (listp item)
			(checkNotList (cdr cnlliste) cnlstring)
			(if (equal item cnlstring)
				t
				(checkNotList (cdr cnlliste) cnlstring)
			)
		)
	)
)

; Parse Tree olusturmak icin oncelikle Parse Tree Listesi olusturmak icin olusturulmus fonksiyon
; tokenList -> Gelen tokenleri kurala uygun bir sekilde Parse Tree Listesine koyuyor
; rule -> Tokenlerin hangi kurala uygun oldugu tespit edilip o kuralin parametre olarak verilmesi

(defun Main(reallist tokenList &optional (pTree '("EXPI")))
	(setq bound (countBracket tokenList 0))
	(if (equal (car pTree) "null")
		(progn
			(setq pTree (list(append pTree  (subseq tokenList 1 (- bound 1)) (checkRule (subseq tokenList 1 (- bound 1)) RULES))))
		)
		(if (equal (car pTree) "IDLIST")
			(progn		
				(setq pTree (append pTree (list (append '("IDLIST") (subseq tokenList 0 bound) (checkRule (subseq tokenList 1 (- bound 1)) RULES)))))
			)
			(if (and (equal (car pTree) "EXPLISTI") (equal (car (cdr (car tokenList))) "'("))
				(progn		
					(setq setValues '("VALUES"))
					(setq pTree (append pTree (subseq tokenList 0 bound) setValues))
				)
				(if (equal (car pTree) "VALUES")
					(progn
						(setq setValues '("VALUES"))
						(setq pTree (append pTree (subseq tokenList 2 (- bound 1)) setValues))
						
					)
					(if (and (equal (car pTree) "EXPLISTI") (equal (car (car (subseq tokenList 1 bound))) "KW"))
						(progn
							(setq pTree (append pTree (list (append '("EXPI")  (subseq tokenList 0 bound) (checkRule (subseq tokenList 1 (- bound 1)) RULES)))))
						)
						(if (and (equal (car pTree) "EXPLISTI") (and (car (cdr (car (cdr (subseq tokenList 0 bound))))) (car (checkRule (subseq tokenList 1 (- bound 1)) RULES))))
							(setq pTree (append pTree (list (append '("EXPI") (subseq tokenList 0 bound) (checkRule (subseq tokenList 1 (- bound 1)) RULES)))))
							(setq pTree (append pTree (subseq tokenList 0 bound) (checkRule (subseq tokenList 1 (- bound 1)) RULES)))
						)
					)
				)
			)
		)
	)

	(princ "Parsed Form Of Input : " )
    ;(princ pTree)
    (terpri)
    (princ "SYNTAX OK")
	(DisplayResult reallist)
)


;;math operators
(defun power_help (x y)
            (if (= y 0) 1
                (* x (power_help x (- y 1))))) 
(defun power (x y)
                (power_help (parse-integer x) (parse-integer y)))
(defun minus (x y)
                (- (parse-integer x) (parse-integer y)))
(defun sumation (x y)
                (+ (parse-integer x) (parse-integer y)))
(defun mult (x y)
                (* (parse-integer x) (parse-integer y)))
(defun div (x y)
                (/ (parse-integer x) (parse-integer y)))




(defun boolSymbol(boolType)
	(if (equal boolType "true") 1 
	          0 ))
(defun setSymbol(place value)
	(princ place)
		(write '=) 
			value)
(defun exit()
	(princ 	"the g++ interpreter was closed by the user")
	(quit)
)
(defun deffunSymbol(function)
	
	(princ "function defined called ->>")
	function)
(defun less (x y)
                (if (< (parse-integer x) (parse-integer y))
                	1
                	0))
(defun equalSymbol (x y)
                (if (equal  x  y)
                	1
                	0))
(defun forSymbol (liste)
	 (setq expi liste)
	   (RESULT (cdr expi)))

(defun andSymbol (x y)
                (if (and (equal  (parse-integer x)  1) (equal (parse-integer y) 1))
                	1
                	0))

(defun orSymbol (x y)
                (if (or (equal  (parse-integer x)  1) (equal (parse-integer y) 1))
                	1
                	0))
(defun notSymbol (x y)
                (if (<= (parse-integer x) (parse-integer y))
                	0
                	1))
(defun ifSymbol (liste)
	 (setq expi (cdr liste))
	 (if (equal  (RESULT expi)  1)
                	1
                	0))
	   



(defun DisplayResult(parsed)
    (terpri)
    (princ "RESULT: ")
    (princ (RESULT parsed))
    (terpri)
    (parser)
	)

(defun RESULT(parsed)
	
    (setq operator (car (cdr (nth 1 parsed))))
    
    (if (equal operator "-") (minus (car (cdr (nth 2 parsed))) (car (cdr (nth 3 parsed))))
		(if (equal operator "/") (div (car (cdr (nth 2 parsed)))   (car (cdr (nth 3 parsed))))
			(if (equal operator "*") (mult (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
				(if (equal operator "**") (power (car (cdr (nth 2 parsed))) (car (cdr (nth 3 parsed))))
					(if (equal operator "+") (sumation (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
						(if (or(equal operator "true") (equal operator "false")) (boolSymbol operator)
							(if (equal operator "set")  (setSymbol (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed)))) 
								(if (equal operator "exit")  (exit) 
									(if (equal operator "deffun")  (deffunSymbol(car (cdr (nth 2 parsed))))
										(if (equal operator "less")  (less (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
											(if (equal operator "equal")  (equalSymbol (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
												(if (equal operator "and")  (andSymbol (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
													(if (equal operator "or")  (orSymbol (car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
														(if (equal operator "not")  (notSymbol(car (cdr (nth 2 parsed)))  (car (cdr (nth 3 parsed))))
															(if (equal operator "list") TempInput 
																(if (equal operator "for")  (forSymbol (cdr Parsed))
																	(if (equal operator "if")  (ifSymbol (cdr Parsed)))))))))))))))))))
    )
 

(defun string-to-list (s)
  (let ((L (read-from-string 
           (concatenate 'string "(" s ")"))))
    L))

(defun parser()
    
    (princ ">>>")
    (setq input (read-line))
    (setq TempInput input)
    (setq tokens (lexer input))
    (print tokens)
    (terpri)
	(Main tokens tokens)
	(parser)
)

;; MAIN FUNCTION
(defun gppinterprete ()
	(princ "********************      G++ Language Interpreter      ******************************")
	(terpri)
	(princ " Usage => Enter your command or Use your input file like || load yourfilename.txt || to execute" )
    (terpri)
    (terpri)
	(parser)
	;(terpri)
)

;; CALL MAIN FUNCTION
(gppinterprete)





