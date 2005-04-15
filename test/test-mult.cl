
(in-package :matrix)

(setf g (matrix::gaussian-kernel 2 1))
(print-matrix g)
(matrix::scalar-mult-row g 0 5)
(print-matrix g)

