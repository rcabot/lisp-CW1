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
				(if (pIsIn (car (car y)) x) '() (list (car y))))
		(cdr y))
		x))
(defun orderInner (exp)
	;(qsort exp)) ; TODO: fix
	exp)
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
    ((or (null a)(null b)) nil)
    (( < (char-code (character (car a))) (car (char-code (character (car b))))) (list< a (cdr b)))
    (t(cons (car b) (list< a (cdr b))))))
(defun list>= (a b)
  (cond
    ((or ( null a)(null b)) nil)
    (( >= (char-code (character (car a))) (car (char-code (character (car b))))) (list>= a (cdr b)))
    (t(cons (car b) (list>= a (cdr b))))))
; end of qsort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun binarify (exp)
	exp)
(defun simplify (exp) ; TODO: handle many term addition and subtraction
	(cond ((equal '+ (car (cdr exp))) (p+ (car exp) (car (cdr (cdr exp)))))
		  ((equal '- (car (cdr exp))) (p- (car exp) (car (cdr (cdr exp)))))
		  ((equal '* (car (cdr exp))) (p* (car exp) (car (cdr (cdr exp)))))
		  (t exp)))
(defun p+ (exp1 exp2)
	(let ((x (simplify exp1)) (y (simplify exp2)))
		(if (equal (car (cdr x)) (car (cdr y)))
			(list (+ (car x) (car y)) (car (cdr x)))
			(list x '+ y))))
(defun p- (exp1 exp2)
	(let ((x (simplify exp1)) (y (simplify exp2)))
		(if (equal (car (cdr x)) (car (cdr y)))
			(list (- (car x) (car y)) (car (cdr x)))
			(list x '- y))))
(defun pickPermute* (x op1 op2 y)
	(if (not (or (listp x) (listp y))) ; test if x or y is atomic
		'() 
		(if (equal op1 op2) 
			(p* x y)
			'(0)))) ; TODO: negate for different op1 and op2
(defun delimit+ (exp)		
	(if (cdr exp) 
		(append (list (car exp)) (append (list '+) (delimit+ (cdr exp)))) 
		exp))
(defun p* (exp1 exp2)
	(let ((x (simplify exp1)) (y (simplify exp2)))
		(if (or (listp (car x)) (listp (car y))) ; if one of the arguments is more than one term
			(let ((p1 (pickPermute* (car x) '+ '+ (car y))) 
				  (p2 (pickPermute* (car x) '+ (car (cdr y)) (car (cdr (cdr y)))))
				  (p3 (pickPermute* (car (cdr (cdr x))) (car (cdr y)) '+ (car y)))
				  (p4 (pickPermute* (car (cdr (cdr x))) (car (cdr x)) (car (cdr y)) (car (cdr (cdr y)))))) ; in the form (p1 +/- p2) * (p3 +/- p4)
				(simplify (delimit+ (remove '() (list p1 p2 p3 p4))))) ; deal with each item and create permutations 
			(append ; otherwise multiply two terms
				(list (* (car x) (car y))) 
				(orderInner (squish* (cdr x) (cdr y)))))))
; TESTS:
(format t "Hello world! Let's do some maths.")
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