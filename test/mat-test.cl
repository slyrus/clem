
(in-package :matrix)

(let ((m (array->matrix #2A((1 2 3)(4 1 6)(7 1 9)))))
  (print-matrix (invert-matrix m)))
  
(let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9)))))
  (print-matrix m)
  (swap-rows m 0 2)
  (swap-cols m 0 2)
  (print-matrix m)
  (let ((q (val m 0 0)))
    (map-row m 0 #'(lambda (x) (/ x q)))
    (scalar-mult-col m 0 4)
    (scalar-divide-col m 0 3)
    )
  (print-matrix m)
  )

(let ((a #2A((1 2)(3 4))))
  (print (aref a 0 0)))
(defvar m1 (array->matrix #2A((1 2 3)(4 5 6)(7 8 9) (10 11 12))))
(print-matrix m1)
(print (dim m1))

(defvar t1 (transpose m1))
(print-matrix t1)
(print (dim t1))

(defvar m2 (array->matrix #2A((1 2 3 4 5 6 7 8 9))))
(print m2)
(print-matrix m2)
(print (dim m2))

(defvar m3 (array->matrix #2A((1) (2) (3) (4) (5) (6) (7) (8) (9))))
(print-matrix m3)
(print (dim m3))


(print 'bogus)

(let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9)))))
  (print-matrix (mat-mult m m))
  (let ((inv (invert-matrix m)))
    (if inv (print-matrix (invert-matrix m)))
    ))

