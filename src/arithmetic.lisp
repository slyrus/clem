
(in-package :clem)

;;;;
;;;; level-1 arithmetic functions

(defmethod m+ (&rest matrices)
  (reduce #'mat-add matrices))

(defmethod m- (&rest matrices)
  (if (cdr matrices)
      (reduce #'mat-subtr matrices)
      (mat-scale (car matrices) -1)))

(defmethod m* (&rest matrices)
  (reduce
   #'(lambda (x y)
       (cond ((and (typep y 'matrix)
                   (typep x 'matrix))
              (mat-mult x y))
             ((and (typep x 'matrix)
                   (numberp y))
              (mat-scale x y))
             ((and (numberp x)
                   (typep y 'matrix))
              (mat-scale y x))
             (t (error 'matrix-argument-error
                       :cause "at least one argument must be a MATRIX."))))
   matrices))

(defmethod m.* (&rest matrices)
  (reduce #'mat-hprod matrices))

