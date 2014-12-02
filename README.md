;;;;;;;;;
; BRIEF ;
;;;;;;;;;
; CM20214/CM20221
; Advanced Programming Principles/Programming II
; Assessed Coursework Assignment 1
; Due: 5pm., Friday 12th Dec 2014, via Moodle
; Implement simple polynomial arithmetic in Lisp, with the polynomials 
; represented in some suitable way within Lisp.
; Your code should
; • implement the three polynomial arithmetic operations +, − and ∗. 
;   The functions should be named p+, p- and p*
; • expand and collect together like terms, 
;   e.g., the sum of x + y and x should be 2x + y 
;   rather than x + y + x while the product of (x + y) and (x + z) 
;   should be x2 + xz + xy + yz. The order of the terms is your choice.
; • p+, p-, p* should all be functions of exactly 
;   two arguments, both being polynomials presented in your chosen 
;   format, and should return a polynomial in your chosen format. 
;   Thus, if p1 is x + y + 1 and p2 is 2xy + x + z,
;   then (p+ p1 p2) returns your representation for 2xy+2x+y+z+1; 
;   and (p- (p+ p1 p2) p2) returns
;   your representation for x + y + 1.

;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM INFORMATION ;
;;;;;;;;;;;;;;;;;;;;;;;
; Written by rc566@bath.ac.uk
; Written for GNU CLISP 2.49 (2010-07-07) (http://clisp.cons.org/)
; 80 character line width enforced

;;;;;;;;;;;;;;;;;;
; REPRESENTATION ;
;;;;;;;;;;;;;;;;;;
; (expression)
; where:
; expression = ((expression) operator (expression)) [EXPRESSION]
; OR
; expression = (coefficient (x exponent)+) [TERM]
; coefficient = some signed integer
; x = arbitrary single character
; exponent = some signed integer
; operator = [*, +]
; term with (x 0) indicates a constant
; (x exponent)s are sorted alphabetically
; inputs and results must always be a list of binary operations, such as:
; ((x + (2x + y)) * 2x)