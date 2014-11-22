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
(defun removex0 (exp)
    (if (> (list-length exp) 1)
        (remove-if #'(lambda (term) (equal 0 (car (cdr term)))) exp)
        exp)) ; removes a zero exponent from squish'd terms ((x 1) (y 2) ...) if not the only term
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
(defun dive (exp) ; TODO: handle many term addition and subtraction
    (let ((foo (collect exp)))
        (cond ((equal '+ (car (cdr foo))) (p+ (car foo) (car (cdr (cdr foo)))))
              ((equal '- (car (cdr foo))) (p- (car foo) (car (cdr (cdr foo)))))
              ((equal '* (car (cdr foo))) (p* (car foo) (car (cdr (cdr foo)))))
              (t foo))))
(defun p+ (exp1 exp2)
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (equal (car (cdr x)) (car (cdr y)))
            (list (+ (car x) (car y)) (car (cdr x)))
            (list x '+ y))))
(defun p- (exp1 exp2)
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (equal (car (cdr x)) (car (cdr y)))
            (list (- (car x) (car y)) (car (cdr x)))
            (list x '- y))))
(defun pickPermute* (x op1 op2 y)
    (if (not (or (listp x) (listp y))) ; test if x or y is atomic
        '() 
        (if (equal op1 op2) 
            (p* x y)
            (p* x (list '(-1 (x 0)) '* y))))) ; negate for different op1 and op2
(defun delimit+ (exp)       
    (if (cdr exp) 
        (append (list (car exp)) (append (list '+) (delimit+ (cdr exp)))) 
        exp))
(defun p* (exp1 exp2) ; TODO: don't pass on (x 0) terms
    (let ((x (dive exp1)) (y (dive exp2)))
        (if (or (listp (car x)) (listp (car y))) ; if one of the arguments is more than one term
            (let ((p1 (pickPermute* (car x) '+ '+ (car y))) 
                  (p2 (pickPermute* (car x) '+ (car (cdr y)) (car (cdr (cdr y)))))
                  (p3 (pickPermute* (car (cdr (cdr x))) (car (cdr y)) '+ (car y)))
                  (p4 (pickPermute* (car (cdr (cdr x))) (car (cdr x)) (car (cdr y)) (car (cdr (cdr y)))))) ; in the form (p1 +/- p2) * (p3 +/- p4)
                (dive (binarify (delimit+ (remove '() (list p1 p2 p3 p4)))))) ; deal with each item and arrange permutations 
            (append ; otherwise multiply two terms
                (list (* (car x) (car y))) 
                (orderInner (removex0 (squish* (cdr x) (cdr y))))))))

(defun simplify (exp)
    (dive exp))


(defun collect*it (old new)
    (cond ((equal '+ (car (cdr old))) (append new (list (collect*it (car old) new) (collect*it (car (cdr (cdr old))) new))))
          ((equal '- (car (cdr old))) (collect*it (p* (car old) (list '(-1 (x 0)) '* (car (cdr (cdr old))))) new))
          ((equal '* (car (cdr old))) (append new old))
          (t (append new old))))
(defun collect (exp) ; TODO: now iteratively add terms up!
    (if (listp (car exp)) ; if the argument is more than one term) 
        (binarify (delimit+ (sumTerms (collect*it exp '())))) 
        exp))

(print (collect '((2 (x 1) (y 2) + ((2 (x 1) (y 2)) + (3 (x 2)))))))
(print (collect '((2 (x 1) (y 2) + ((2 (x 1) (y 2)) * (3 (x 2)))))))