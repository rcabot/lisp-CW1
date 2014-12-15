; FUNCTION DEFS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; modified version of YBE's file access syntax
; source: http://stackoverflow.com/questions
;           /9495376/how-to-create-and-write-into-text-file-in-lisp

(defun doTest (test result expected)
    (with-open-file (file "./testresults.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format file "TEST: ~a~%EXPC: ~a~%RESU: ~a~%PASS? ~a~%~%" 
        test expected result (equal expected result)))
    (format t "TEST: ~a~%EXPC: ~a~%RESU: ~a~%PASS? ~a~%~%" 
       test expected result (equal expected result))
)

; end of file writing functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negate (x)
    "negates a term"
    (p* x '(-1 (x 0))))

(defun pIsIn (term expression)
    "gets a exponentiation such as x and a term without a coefficient 
    and returns true if it contains it"
    (if expression
        (if (equal term (caar expression)) 
            t 
            (pIsIn term (cdr expression)))
        nil))

(defun isTerm (exp)
    "returns true if exp is a representational TERM
    and false otherwise"
    (cond ((equal '+ (cadr exp)) nil)
          ((equal '- (cadr exp)) nil)
          ((equal '* (cadr exp)) nil)
          (t exp)))

(defun removec0 (exp)
    "takes a flat list of terms to be summed without and removes 
    a term if it has a zero coefficient"
	(if (> (list-length exp) 1)
		(remove-if #'(lambda (term) (equal 0 (car term))) exp)
		(if (equal 0 (car exp)) 
			'() 
			exp)))

(defun removeLikeTerms (exp term)
    "removes like terms from a flat list"
    (if (> (list-length exp) 1)
        (remove-if #'(lambda (termI) (equal (cdr termI) (cdr term))) exp)
        (if (equal (cdar exp) (cdr term))
            '() 
            exp)))

(defun removex0 (exp) 
    "removes a zero exponent from raw squish'd terms ((x 1) (y 2) ...) 
    if not the only term, 
    and also 0 coeff terms"
	(if (> (list-length exp) 1)
			(remove-if #'(lambda (term) (equal 0 (cadr term))) exp)
			exp)) 

(defun orderInner (exp)
    "gets a TERM without a coefficient and 
    sorts the inner symbols and their exponents
    (extensible wrapper for modified quicksort function)"
    (qsort exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; modified version of brunocodutra's functional qsort
; sourced from: http://stackoverflow.com/questions/19082032/quicksort-in-lisp

(defun qsort (L)
    (cond ((null L) nil)
          (t (append 
                (qsort (list< (car L) (cdr L)))
                (cons (car L) nil) 
                (qsort (list>= (car L) (cdr L)))))))

(defun list< (a b)
    (cond
        ((or (null a) (null b)) nil)
        ((< 
            (char-code (character (car a))) 
            (char-code (character (car (car b))))) 
        (list< a (cdr b)))
        (t (cons (car b) (list< a (cdr b))))))

(defun list>= (a b)
    (cond
        ((or ( null a) (null b)) nil)
        ((>= 
            (char-code (character (car a))) 
            (char-code (character (car (car b))))) (list>= a (cdr b)))
        (t (cons (car b) (list>= a (cdr b))))))

; end of qsort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun binarify (exp)
    "takes a flat list of representational EXPRESSIONs and TERMs delimited by 
    + and returns a single sided binary tree of expressions
    e.g. (a + b + c):=(a + (b + c)"
    (if (> (list-length exp) 1)
        (list (car exp) (cadr exp) (binarify (cddr exp))) 
        (car exp)))

(defun simplify (exp)
    "extensible wrapper for the dive function"
    (dive exp))

(defun dive (exp)
    "gets a representational EXPRESSION and evaluates it 
    depending on it's contents, or returns if exp is a 
    TERM"
    (let ((foo exp))
            (cond ((equal '+ (cadr foo)) 
                    (p+ (car foo) (caddr foo)))
                  ((equal '- (cadr foo)) 
                    (p- (car foo) (caddr foo)))
                  ((equal '* (cadr foo))
                    (p* (car foo) (caddr foo)))
                  (t foo))))

(defun delimit+ (exp)
    "takes a flat list of representational expressions that 
    need to be added together and delimits them with +"
    (if (cdr exp) 
        (append (list (car exp)) (append (list '+) (delimit+ (cdr exp)))) 
        exp))

(defun pickPermute* (x op1 op2 y)
    "gets two representational expressions and their signage,
    and returns their product"
    (if (equal op1 op2) 
        (p* x y)
        (p* y (negate x))))

(defun squish* (x y)
    "takes 2 representative TERMs without 
    coefficients and multiplies them"
    (if y 
        (squish* 
            (append 
                (map 'list 
                    #'(lambda (foo) 
                        (if (equal (caar y) (car foo)) 
                            (list 
                                (car foo) 
                                (+ (cadr foo) (cadar y)))
                            foo))
                    x)
                (if (pIsIn (caar y) x) 
                    '() 
                    (list (car y))))
        (cdr y))
        x))





(defun sumTerms (old new)
    "takes a flat list of representational terms, sums like terms, and returns 
    them appended to new"
    (if old 
        (sumTerms 
            (removeLikeTerms (cdr old) (car old))
            (sumTerms-it (car old) (cdr old) new))
        new))

(defun sumTerms-it (term old new)
    "takes a term and a flat list of representational terms,
    adds it (if it can) to every other term in the list, 
    returns them appended to new"
    (if old
        (if (equal (cdar old) (cdr term))
            (sumTerms-it (p+ term (car old)) (cdr old) new) ;SOMETHING HERE?
            (sumTerms-it term (cdr old) new))
        (append new (list term))))








(defun collect (exp)
    "collects like representational terms in the expression"
    (if (isTerm exp)
            exp
            (binarify (delimit+ (removec0 (sumTerms 
                (collect-it exp '()) 
                '()))))))

(defun collect-it (old new)
    "gets a representational expression and returns a flattened 
    list of terms which can represented as a sum"
    (cond ((equal '+ (cadr old)) 
            (append new 
                (collect-it (car old) new) 
                (collect-it (caddr old) new)))
          ((equal '- (cadr old)) 
            (append new 
                (collect-it (car old) new) 
                (collect-it (negate (caddr old)) new)))
          ((equal '* (cadr old)) 
            (append new (list old)))
          (t 
            (append new (list old)))))

(defun p* (exp1 exp2) 
    "multiplies EXPRESSION or TERM exp1 from 
    EXPRESSION or TERM exp2"
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (not (isTerm x))
            (if (not (isTerm y))
                (let ((p1 (pickPermute* 
                            (car x) '+ '+ (car y)))
                      (p2 (pickPermute* 
                            (car x) 
                            '+ 
                            (cadr y) 
                            (caddr y)))
                      (p3 (pickPermute* 
                            (caddr x) 
                            (cadr y) 
                            '+ 
                            (car y)))
                      (p4 (pickPermute* 
                            (caddr x) 
                            (cadr x) 
                            (cadr y) 
                            (caddr y))))
                    (dive (binarify (delimit+ (remove '() 
                            (list p1 p2 p3 p4))))))
                (let ((p1 (pickPermute* 
                        (car x) '+ '+ y))
                      (p2 (pickPermute* 
                        (caddr x) 
                        '+ 
                        (cadr x) 
                        y)))
                     (dive (binarify (delimit+ (remove '() 
                        (list p1 p2)))))))
            (if (not (isTerm y))
                (let ((p1 (pickPermute* 
                        (car y) '+ '+ x))
                      (p2 (pickPermute* 
                        (caddr y) '+ (cadr y) x)))
                    (dive (binarify (delimit+ (remove '() (list p1 p2))))))
                (append
                    (list (* (car x) (car y))) 
                    (orderInner (removex0 (squish* (cdr x) (cdr y)))))))))

(defun p+ (exp1 exp2)
    "sums two representational EXPRESSIONs or TERMs"
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (and (isTerm x) (isTerm y))
            (if (equal (cdr x) (cdr y))
                (append (list (+ (car x) (car y))) (cdr x))
                (collect (list x '+ y)))
            (collect (list x '+ y)))))

(defun p- (exp1 exp2)
    "subtracts EXPRESSION or TERM exp1 from 
    EXPRESSION or TERM exp2"
    (p+ exp1 (negate exp2)))

(load "./testsuite.lisp")