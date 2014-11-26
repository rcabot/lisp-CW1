; CM20214/CM20221
; Advanced Programming Principles/Programming II
; Assessed Coursework Assignment 1
; Due: 5pm., Friday 12th Dec 2014, via Moodle
; Implement simple polynomial arithmetic in Lisp, with the polynomials represented in some suitable way within Lisp.
; Your code should
; • implement the three polynomial arithmetic operations +, − and ∗. The functions should be named p+, p- and p*
; • expand and collect together like terms, e.g., the sum of x + y and x should be 2x + y rather than x + y + x while the product of (x + y) and (x + z) should be x2 + xz + xy + yz. The order of the terms is your choice.
; This will involve you picking a suitable representation for your polynomials (hint: Lisp has symbols and lists!), and
; then implementing the required functions to manipulate them.
; • p+, p-, p* should all be functions of exactly two arguments, both being polynomials presented in your chosen
; format, and should return a polynomial in your chosen format. Thus, if p1 is x + y + 1 and p2 is 2xy + x + z,
; then (p+ p1 p2) returns your representation for 2xy+2x+y+z+1; and (p- (p+ p1 p2) p2) returns
; your representation for x + y + 1.

; REPRESENTATION:
; (expression)
; where:
; expression = (expression) operator (expression) [EXPRESSION]
; OR
; expression = (coefficient (x exponent)+) [TERM]
; coefficient = [0..9]+
; x = [A..Z,a..z]
; exponent = [0..9]+
; operator = [x, +, -]
; term with (x 0) indicates a constant
; FUNCTION DEFS:
(defun pIsIn (term expression)
    (if expression
        (if (equal term (car (car expression))) 
            t 
            (pIsIn term (cdr expression)))
        nil))
(defun squish* (x y)
    (if y 
        (squish* 
            (append 
                (map 'list 
                    #'(lambda (foo) 
                        (if (equal (car (car y)) (car foo)) 
                            (list (car foo) (+ (car (cdr foo)) (car (cdr (car y)))))
                            foo))
                    x)
                (if (pIsIn (car (car y)) x) 
                    '() 
                    (list (car y))))
        (cdr y))
        x))
(defun removec0 (exp)
	(if (> (list-length exp) 1)
		(remove-if #'(lambda (term) (equal 0 (car term))) exp)
		(if (equal 0 (car exp)) 
			'() 
			exp)))
(defun removex0 (exp)
	(if (> (list-length exp) 1)
			(remove-if #'(lambda (term) (equal 0 (car (cdr term)))) exp)
			exp)) ; removes a zero exponent from squish'd terms ((x 1) (y 2) ...) if not the only term, and also 0 coeff terms
(defun orderInner (exp)
    (qsort exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; modified version of brunocodutra's qsort
; sourced from: http://stackoverflow.com/questions/19082032/quicksort-in-lisp
(defun qsort (L)
    (cond
        ((null L) nil)
            (t
                (append 
                    (qsort (list< (car L) (cdr L)))
                    (cons (car L) nil) 
                    (qsort (list>= (car L) (cdr L)))))))
(defun list< (a b)
    (cond
        ((or (null a) (null b)) nil)
        ((< (char-code (character (car a))) (char-code (character (car (car b))))) (list< a (cdr b)))
        (t(cons (car b) (list< a (cdr b))))))
(defun list>= (a b)
    (cond
        ((or ( null a) (null b)) nil)
        ((>= (char-code (character (car a))) (char-code (character (car (car b))))) (list>= a (cdr b)))
        (t(cons (car b) (list>= a (cdr b))))))
; end of qsort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun binarify (exp)
    (if (> (list-length exp) 1)
        (list (car exp) (car (cdr exp)) (binarify (cdr (cdr exp)))) 
        (car exp)))
(defun dive (exp)
    (let ((foo (collect exp)))
            (cond ((equal '+ (car (cdr foo))) (p+ (car foo) (car (cdr (cdr foo)))))
                  ((equal '- (car (cdr foo))) (p- (car foo) (car (cdr (cdr foo)))))
                  ((equal '* (car (cdr foo))) (p* (car foo) (car (cdr (cdr foo)))))
                  (t foo))))
(defun p+ (exp1 exp2)
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (equal (cdr x) (cdr y))
            (list (+ (car x) (car y)) (car (cdr x)))
            (list x '+ y))))
(defun p- (exp1 exp2) ; produce is terms of p+
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (equal (cdr x) (cdr y))
            (list (- (car x) (car y)) (car (cdr x)))
            (list x '- y))))
(defun pickPermute* (x op1 op2 y)
    (if (equal op1 op2) 
        (p* x y)
        (p* y (p* x '(-1 (x 0)))))) ; negate for different op1 and op2
(defun delimit+ (exp)       
    (if (cdr exp) 
        (append (list (car exp)) (append (list '+) (delimit+ (cdr exp)))) 
        exp))
(defun p* (exp1 exp2) 
    (let ((x (dive exp1)) (y (dive exp2)))
            (if (not (isTerm x))  ; if one of the arguments is more than one term
                (if (not (isTerm y))
                	; in the form (x1 +/- x2) * (y1 +/- y2)

	                (let ((p1 (pickPermute* (car x) '+ '+ (car y)))  ; x1 + y1
	                      (p2 (pickPermute* (car x) '+ (car (cdr y)) (car (cdr (cdr y))))) ; x1 + y2
	                      (p3 (pickPermute* (car (cdr (cdr x))) (car (cdr y)) '+ (car y))) ; x2 + y 1
	                      (p4 (pickPermute* (car (cdr (cdr x))) (car (cdr x)) (car (cdr y)) (car (cdr (cdr y))))))  ; x2+ y2
	                    (dive (binarify (delimit+ (remove '() (list p1 p2 p3 p4)))))) ; deal with each item and arrange permutations 
	            	
	            	; in the form (x1 +/- x2) * y

	            	(let ((p1 (pickPermute* (car x) '+ '+ y)) ; x1 + y
	                      (p2 (pickPermute* (car (cdr (cdr x))) '+ (car (cdr x)) y))) ; x2 + y
	                    (dive (binarify (delimit+ (remove '() (list p1 p2))))))
	            ) ; deal with each item and arrange permutations
                (if (not (isTerm y)) 
                	; in the form x * (y1 +/- y2)

                	(let ((p1 (pickPermute* (car y) '+ '+ x)) ; y1 + x
	                      (p2 (pickPermute* (car (cdr (cdr y))) '+ (car (cdr y)) x))) ; y2 + x
	                    (dive (binarify (delimit+ (remove '() (list p1 p2)))))) ;TODO
                	; in the form x * y
                	(append ; otherwise multiply two terms
	                    (list (* (car x) (car y))) 
	                    (orderInner (removex0 (squish* (cdr x) (cdr y)))))
                )
            )
		))


                

(defun simplify (exp)
    (dive exp))

(defun debugPrint (exp message)
	(print message)
	(print exp))


(defun isTerm (exp)
    (cond ((equal '+ (car (cdr exp))) nil)
          ((equal '- (car (cdr exp))) nil)
          ((equal '* (car (cdr exp))) nil)
          (t exp)))
(defun sumTerms-it (term old new)
    (if old
            (if (equal (cdr (car old)) (cdr term)) 
                (sumTerms-it (p+ term (car old)) (cdr old) new) 
                (sumTerms-it term (cdr old) (append new (list (car old)))))
            (list old new term)))
(defun sumTerms (old new)
    (let ((it (sumTerms-it (car old) (cdr old) new)))
            (if old 
                (sumTerms (car it) (append (list (car (cdr (cdr it)))) (car (cdr it))))
                new)))
(defun collect-it (old new)
    (cond ((equal '+ (car (cdr old))) (append new (collect-it (car old) new) (collect-it (car (cdr (cdr old))) new)))
                  ((equal '- (car (cdr old))) (append new (collect-it (car old) new) (collect-it (p* '(-1 (x 0)) (car (cdr (cdr old)))) new)))
                  ((equal '* (car (cdr old))) (append new (list old)))
                  ;((not (isTerm (car old))) (append new (collect-it (car old) '())))
                  (t (append new (list old)))))
(defun collect (exp)
    (if (isTerm exp) ; if the argument is one term
            exp
            (binarify (delimit+ (removec0 (sumTerms (collect-it exp '()) '()))))))

; (defun collect (exp)
; 	exp)
; TESTS:
(format t "~%Hello world! Let's do some maths.")
(format t "~%~%Poly functions:")




;TESTBED

;ENDBED



(print (p+ '(4 (x 2)) '(3 (x 2))))
(print (equal (p+ '(4 (x 2)) '(3 (x 2))) '(7 (x 2))))
(print (p- '(4 (x 2)) '(3 (x 2))))
(print (equal (p- '(4 (x 2)) '(3 (x 2))) '(1 (x 2))))
(print (p* '(4 (x 2)) '(3 (x 2))))
(print (equal (p* '(4 (x 2)) '(3 (x 2))) '(12 (x 4))))
(print (p+ '(4 (x 2)) '(3 (y 2))))
(print (equal (p+ '(4 (x 2)) '(3 (y 2))) '((4 (x 2)) + (3 (y 2)))))
(print (p- '(4 (x 2)) '(3 (y 2))))
(print (equal (p- '(4 (x 2)) '(3 (y 2))) '((4 (x 2)) - (3 (y 2)))))
(print (p* '(4 (x 2)) '(3 (y 2))))
(print (equal (p* '(4 (x 2)) '(3 (y 2))) '(12 (x 2) (y 2))))
(print (p* '(4 (x 2) (y 2)) '(3 (x 3) (y 2))))
(print (equal (p* '(4 (x 2) (y 2)) '(3 (x 3) (y 2))) '(12 (x 5) (y 4))))
(format t "~%~%Full simplification:")
(print (simplify '(4 (x 2))))
;= (4 (x 2))
(print (simplify '((4 (x 1) (y 5)) * (1 (x 4)))))
;= (4 (x 5) (y 5))
(print (simplify '((4 (x 2)) * (2 (y 5)))))
;= (8 (x 2) (y 5))
(print (simplify '((4 (x 2)) + (2 (y 5)))))
;= ((4 (x 2)) + (2 (y 5)))
(print (simplify '((4 (x 2)) - (2 (y 5)))))
;= ((4 (x 2)) - (2 (y 5)))
(print (simplify '((4 (x 2)) + ((2 (y 5)) + (7 (z 2))))))
;= ((4 (x 2)) + (2 (y 5))) + (7 (z 2))
(print (simplify '((4 (x 2)) - ((2 (y 5)) + (7 (z 2))))))
;= ((4 (x 2)) - (2 (y 5))) + (7 (z 2))
(print (simplify '((4 (x 2)) + ((2 (y 5)) * (7 (z 2))))))
;= ((4 (x 2)) + (14 (y 5) (z 2)))
(print (simplify '(((1 (x 1)) + (1 (y 1))) * ((1 (x 1)) + (1 (y 1))))))
;= (1 (x 2)) + (2 (x 1) (y 1)) + (1 (y 2))
(print (simplify '(((1 (x 1)) + (1 (y 1))) * ((1 (x 1)) - (1 (y 1))))))
;=?
(format t "~%~%MEGA TEST:")
(print (simplify '(((((((10 (x 2)) + (1 (y 2))) * (2 (x 0))) * ((27 (x 1)) + (12 (y 7)))) * ((1 (y 1)) - ((1 (y 2)) - (1 (y 2))))) - (1 (z 2))) * (7 (x 1)))))
; (print (equal '((3780 (x 4) (y 1)) + ((378 (x 2) (y 3)) + ((1680 (x 3) (y 8)) + ((168 (x 1) (y 10)) - (7 (x 1) (z 2)))))) (simplify '(((((((10 (x 2)) + (1 (y 2))) * (2 (x 0))) * ((27 (x 1)) + (12 (y 7)))) * ((1 (y 1)) - ((1 (y 2)) - (1 (y 2))))) - (1 (z 2))) * (7 (x 1)))))))