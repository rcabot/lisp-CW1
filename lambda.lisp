

(print (collect '((2 (x 1) (y 2)) + ((2 (x 1) (y 2)) + (3 (x 2))))))
(print "^ answer")
(print (collect '((2 (x 1) (y 2)) + ((2 (x 1) (y 2)) * (3 (x 2))))))
(print "^ answer")
(print (collect '((2 (x 1) (y 2)) + ((2 (x 1) (y 2)) - (3 (x 2))))))
(print "^ answer")