(defun sumTerms-it (term old new)

    (if old
            (if (equal (cdr (car old)) (cdr term)) 
                (sumTerms-it (p+ term (car old)) (cdr old) new) 
                (sumTerms-it term (cdr old) (append new (list (car old)))))
            (list old new term)))

(defun sumTerms (old new)
    ""
    (let ((it (sumTerms-it (car old) (cdr old) new)))
            (if old 
                (sumTerms 
                    (car it) 
                    (append (list (car (cdr (cdr it)))) (car (cdr it))))
                new)))

(print (collect '((2 (x 1) (y 2)) + ((2 (x 1) (y 2)) + (3 (x 2))))))
(print "^ answer")
(print (collect '((2 (x 1) (y 2)) + ((2 (x 1) (y 2)) * (3 (x 2))))))
(print "^ answer")
(print (collect '((2 (x 1) (y 2)) + ((2 (x 1) (y 2)) - (3 (x 2))))))
(print "^ answer")