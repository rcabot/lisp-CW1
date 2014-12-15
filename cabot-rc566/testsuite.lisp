;TEST SUITE:

(format t "Hello world! Let's do some maths.~%")

(format t "Poly functions:~%~%")
(doTest "Add 2x and 2xy" 
    (p+ '(2 (x 1)) '(2 (x 1) (y 1))) '((2 (x 1)) + (2 (x 1) (y 1))))
(doTest "Mult 2 and 2" (p* '(2 (x 0)) '(2 (x 0))) '(4 (x 0)))
(doTest "Add x and x+y" (p+ '(1 (x 1)) '((1 (x 1)) + (1 (y 1)))) 
    '((2 (X 1)) + (1 (Y 1))))
(doTest "Mult x+y and x+y^2" 
    (p* '((1 (x 1)) + (1 (y 1))) '((1 (x 1)) + (1 (y 2)))) 
    '((1 (X 2)) + ((1 (X 1) (Y 2)) + ((1 (X 1) (Y 1)) + (1 (Y 3))))))
(doTest "Add 4x^2 and 3x^2" (p+ '(4 (x 2)) '(3 (x 2))) '(7 (x 2)))
(doTest "Subtr 3x^2 from 4x^2" (p- '(4 (x 2)) '(3 (x 2))) '(1 (x 2)))
(doTest "Mult 4x^2 and 3x^2" (p* '(4 (x 2)) '(3 (x 2))) '(12 (x 4)))
(doTest "Add 4x^2 and 3y^2" 
    (p+ '(4 (x 2)) '(3 (y 2))) '((4 (x 2)) + (3 (y 2))))
(doTest "Subtr 3y^2 from 4x^2" 
    (p- '(4 (x 2)) '(3 (y 2))) '((4 (x 2)) + (-3 (y 2))))
(doTest "Mult 4x^2 and 3y^2" (p* '(4 (x 2)) '(3 (y 2))) '(12 (x 2) (y 2)))
(doTest "Mult 4(x^2)(y^2) and 3(x^3)(y^2)" 
    (p* '(4 (x 2) (y 2)) '(3 (x 3) (y 2))) '(12 (x 5) (y 4)))

(format t "~%Composed Poly Functions:~%~%")
(let ((p1 '((1 (x 1)) + ((1 (y 1)) + (1 (x 0)))))
      (p2 '((2 (x 1) (y 1)) + ((1 (x 1)) + (1 (z 1))))))
(doTest "(p* p1 p2)" (p* p1 p2) 
    '((2 (X 2) (Y 1)) +
 ((1 (X 2)) +
  ((1 (X 1) (Z 1)) +
   ((2 (X 1) (Y 2)) +
    ((3 (X 1) (Y 1)) + ((1 (Y 1) (Z 1)) + ((1 (X 1)) + (1 (Z 1))))))))))
(doTest "(p+ p1 p2)" (p+ p1 p2) 
    '((2 (X 1)) + ((1 (Y 1)) + ((1 (X 0)) + ((2 (X 1) (Y 1)) + (1 (Z 1)))))))
(doTest "(p- p1 p2)" (p- p1 p2) 
    '((1 (Y 1)) + ((1 (X 0)) + ((-2 (X 1) (Y 1)) + (-1 (Z 1))))))
(doTest "(p- (p+ p1 p2) p2)" (p- (p+ p1 p2) p2) 
    '((1 (x 1)) + ((1 (y 1)) + (1 (x 0)))))
(doTest "(p+ (p- p1 p2) p2)" (p+ (p- p1 p2) p2) 
    '((1 (Y 1)) + ((1 (X 0)) + (1 (X 1)))))
(doTest "(p- (p- (p+ p1 p2) p2) p2)" (p- (p- (p+ p1 p2) p2) p2) 
    '((1 (Y 1)) + ((1 (X 0)) + ((-2 (X 1) (Y 1)) + (-1 (Z 1))))))
)
(format t "~%Unit Tests:~%~%")
(doTest "negate - expression -> expression
    (negate '((1 (x 1)) + (1 (y 1))))" 
    (negate '((1 (x 1)) + (1 (y 1)))) '((-1 (x 1)) + (-1 (y 1))))
(doTest "pIsIn - expo, term with no coeff -> bool
    (pIsIn 'z '((x 2) (y 1) (x 10) (z 1)))" 
    (pIsIn 'z '((x 2) (y 1) (x 10) (z 1)))
    t)
(doTest "isTerm - expression -> bool
    (isTerm '(2 (x 2) (z 2) (y 3)))" 
    (isTerm '((2 (x 2) (z 2) (y 3)) + (2 (x 0))))
    nil)
(doTest "removec0 - flat list of terms -> flat list of terms
    (removec0 '((2 (x 0)) (0 (x 2)) (0 (x 1))))" 
    (removec0 '((2 (x 0)) (0 (x 2)) (0 (x 1))))
    '((2 (x 0))))
(doTest "removex0 - term with no coeff -> term with no coeff
    (removex0 '((x 0) (x 2) (x 1)))" 
    (removex0 '((x 0) (x 2) (x 1)))
    '((x 2) (x 1)))
(doTest "removex0 - term with no coeff -> term with no coeff
    (removex0 '((x 0) (y 0)))" 
    (removex0 '((x 0) (y 0)))
    '((x 0)))
(doTest "orderInner - term with no coeff -> term with no coeff
    (orderInner '((x 0) (u 0) (a 2) (z 2)))" 
    (orderInner '((x 0) (u 0) (a 2) (z 2)))
    '((a 2) (u 0) (x 0) (z 2)))
(doTest "binarify - flat list of expressions with pluses -> binary expression
    (binarify '())" 
    (binarify '())
    '())
(doTest "binarify - flat list of expressions with pluses -> binary expression
    (binarify '((1 (x 1))))" 
    (binarify '((1 (x 1))))
    '(1 (x 1)))
(doTest "binarify - flat list of expressions with pluses -> binary expression
    (binarify '((1 (x 1)) + (2 (x 1))))" 
    (binarify '((1 (x 1)) + (2 (x 1))))
    '((1 (x 1)) + (2 (x 1))))
(doTest "binarify - flat list of expressions with pluses -> binary expression
    (binarify '((1 (x 1)) + (2 (x 1)) + (1 (x 1)) + (2 (x 1))))" 
    (binarify '((1 (x 1)) + (2 (x 1)) + (1 (x 1)) + (2 (x 1))))
    '((1 (X 1)) + ((2 (X 1)) + ((1 (X 1)) + (2 (X 1))))))
(doTest "delimit+ - flat list of expressions -> flat list of 
    expressions with pluses
    (delimit+ '((2 (x 0)) (0 (x 2)) (0 (x 1))))" 
    (delimit+ '((2 (x 0)) (0 (x 2)) (0 (x 1))))
    '((2 (x 0)) + (0 (x 2)) + (0 (x 1))))
(doTest "delimit+ - flat list of expressions -> flat list of 
    expressions with pluses
    (delimit+ '())" 
    (delimit+ '())
    '())
(doTest "pickPermute* - expression, sign, sign, expression -> expression
    (pickPermute* '(2 (x 2)) '+ '- '(2 (y 2)))" 
    (pickPermute* '(2 (x 2)) '+ '- '(2 (y 2)))
    '(-4 (x 2) (y 2)))
(doTest "squish* - term with no coeff, term with no coeff -> term with no coeff
    (squish* '((x 0) (x 1) (y 2)) '((y 2) (x -1) (z 2) (y 3)))" 
    (squish* '((x 0) (x 1) (y 2)) '((y 2) (x -1) (z 2) (y 3)))
    '((y 7) (z 2)))
(doTest "sumTerms - flat list of expressions, empty list -> flat list of 
    expressions
    (sumTerms '() '())" 
    (sumTerms '() '())
    '())
(doTest "sumTerms - flat list of expressions, empty list -> flat list of 
    expressions
    (sumTerms '((4 (x 1)) (3 (x 2) (y 3)) (2 (x 1)) ((2 (x 1)) * (9 (y 9)))) 
              '())" 
    (sumTerms '((4 (x 1)) (3 (x 2) (y 3)) (2 (x 1)) ((2 (x 1)) * (9 (y 9)))) 
              '())
    '((6 (X 1)) (3 (X 2) (Y 3)) ((2 (X 1)) * (9 (Y 9)))))
(doTest "sumTerms - flat list of expressions, empty list -> flat list of 
    expressions
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-2 (X 1) (Y 1)) (-1 (Z 1)))
              '())" 
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-2 (X 1) (Y 1)) (-1 (Z 1)))
              '())
    '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (0 (X 1) (Y 1)) (0 (Z 1))))
(doTest "sumTerms - flat list of expressions, empty list -> flat list of 
    expressions
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-2 (X 1) (Y 1)))
              '())" 
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-2 (X 1) (Y 1)))
              '())
    '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (0 (X 1) (Y 1)) (1 (Z 1))))
(doTest "sumTerms - flat list of expressions, empty list -> flat list of 
    expressions
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-2 (X 1) (Y 1)) (-1 (Z 1)))
              '())" 
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-4 (X 1) (Y 1)) (-2 (Z 1)))
              '())
    '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (-2 (X 1) (Y 1)) (-1 (Z 1))))
(doTest "sumTerms - flat list of expressions, empty list -> flat list of 
    expressions
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-2 (X 1) (Y 1)) (-1 (Z 1)) (2 (X 1)))
              '())" 
    (sumTerms '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
                (-4 (X 1) (Y 1)) (-2 (Z 1)) (2 (X 1)))
              '())
    '((3 (X 1)) (1 (Y 1)) (1 (X 0)) (-2 (X 1) (Y 1)) (-1 (Z 1))))
(doTest "sumTerms-it - term, flat list of expression, empty list -> (flat list, 
    flat list, term)
    (sumTerms-it '() '() '())" 
    (sumTerms-it '() '() '())
    '(()))
(doTest "sumTerms-it - term, flat list of expression, empty list -> (flat list, 
    flat list, term)
    (sumTerms-it 
        '(3 (x 9)) 
        '((4 (x 1)) (3 (x 2) (y 3)) (2 (x 1)) ((2 (x 1)) * (9 (y 9))))  
        '())" 
    (sumTerms-it 
        '(3 (x 9)) 
        '((4 (x 1)) (3 (x 2) (y 3)) (2 (x 1)) ((2 (x 1)) * (9 (y 9))))  
        '())
    '((3 (x 9))))
(doTest "sumTerms-it - term, flat list of expression, empty list -> (flat list, 
    flat list, term)
    (sumTerms-it 
        '(3 (x 1)) 
        '((4 (x 1)) (3 (x 2) (y 3)) (2 (x 1)) ((2 (x 1)) * (9 (y 9))))  
        '())" 
    (sumTerms-it 
        '(3 (x 1)) 
        '((4 (x 1)) (3 (x 2) (y 3)) (2 (x 1)) ((2 (x 1)) * (9 (y 9))))  
        '())
    '((9 (x 1))))
(doTest "collect - expression -> expression
    (collect '((1 (x 1)) + ((1 (y 1)) + ((1 (x 0)) +
   ((2 (x 1) (y 1)) + ((1 (z 1)) + ((-2 (x 1) (y 1)) + (-1 (z 1)))))))))" 
    (collect '((1 (x 1)) + ((1 (y 1)) + ((1 (x 0)) +
   ((2 (x 1) (y 1)) + ((1 (z 1)) + ((-2 (x 1) (y 1)) + (-1 (z 1)))))))))
    '((1 (x 1)) + ((1 (y 1)) + (1 (x 0)))))
(doTest "collect-it - expression, empty list -> flattened list
    (collect-it '((1 (x 1)) + ((1 (y 1)) + ((1 (x 0)) +
   ((2 (x 1) (y 1)) + ((1 (z 1)) + ((-2 (x 1) (y 1)) + (-1 (z 1)))))))) '())" 
    (collect-it '((1 (x 1)) + ((1 (y 1)) + ((1 (x 0)) +
   ((2 (x 1) (y 1)) + ((1 (z 1)) + ((-2 (x 1) (y 1)) + (-1 (z 1)))))))) '())
    '((1 (X 1)) (1 (Y 1)) (1 (X 0)) (2 (X 1) (Y 1)) (1 (Z 1)) 
        (-2 (X 1) (Y 1)) (-1 (Z 1))))

(format t "~%Full simplification:~%~%")
(doTest "Simplification of 4x^2" (simplify '(4 (x 2))) '(4 (x 2)))
(doTest "Simplification of 4x(y^5) * x^4" 
    (simplify '((4 (x 1) (y 5)) * (1 (x 4)))) '(4 (x 5) (y 5)))
(doTest "Simplification of 4x^2 * 2y^5" 
    (simplify '((4 (x 2)) * (2 (y 5)))) '(8 (x 2) (y 5)))
(doTest "Simplification of 4x^2 + 2y^5" 
    (simplify '((4 (x 2)) + (2 (y 5)))) '((4 (x 2)) + (2 (y 5))))
(doTest "Simplification of 4x^2 - 2y^5" 
    (simplify '((4 (x 2)) - (2 (y 5)))) '((4 (x 2)) + (-2 (y 5))))
(doTest "Simplification of 4x^2 + 2y^5 + 7z^2" 
    (simplify '((4 (x 2)) + ((2 (y 5)) + (7 (z 2))))) 
    '((4 (X 2)) + ((2 (Y 5)) + (7 (Z 2)))))
(doTest "Simplification of 4x^2 - 2y^5 + 7z^2" 
    (simplify '((4 (x 2)) - ((2 (y 5)) + (7 (z 2))))) 
    '((4 (X 2)) + ((-2 (Y 5)) + (-7 (Z 2)))))
(doTest "Simplification of 4x^2 + 2y^5 * 7z^2" 
    (simplify '((4 (x 2)) + ((2 (y 5)) * (7 (z 2))))) 
    '((4 (x 2)) + (14 (y 5) (z 2))))
(doTest "Simplification of (x + y) * (x + y)" 
    (simplify '(((1 (x 1)) + (1 (y 1))) * ((1 (x 1)) + (1 (y 1))))) 
    '((1 (X 2)) + ((2 (X 1) (Y 1)) + (1 (Y 2)))))
(doTest "Simplification of (x + y) * (x - y)" 
    (simplify '(((1 (x 1)) + (1 (y 1))) * ((1 (x 1)) - (1 (y 1))))) 
    '((1 (X 2)) + (-1 (Y 2))))

(format t "~%Mega test:~%~%")
(doTest "Simplification of: 
(((((((10 (x 2))
 + (1 (y 2)))
  * (2 (x 0)))
   * ((27 (x 1))
    + (12 (y 7))))
     * ((1 (y 1))
      - ((1 (y 2))
       - (1 (y 2)))))
        - (1 (z 2)))
         * (7 (x 1)))
    " (simplify 
'(((((((10 (x 2))
 + (1 (y 2)))
  * (2 (x 0)))
   * ((27 (x 1))
    + (12 (y 7))))
     * ((1 (y 1))
      - ((1 (y 2))
       - (1 (y 2)))))
        - (1 (z 2)))
         * (7 (x 1))))
'((3780 (x 4) (y 1)) +
 ((1680 (x 3) (y 8)) +
  ((378 (x 2) (y 3)) + 
    ((168 (x 1) (y 10)) + 
        (-7 (x 1) (z 2)))))))