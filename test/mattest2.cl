
(in-package :matrix)

(let ((x (transpose (array->matrix #2A((1 2 3)(4 5 6)(7 8 9)))))
      (h (transpose (array->matrix #2A((0 0 0)(1 0 -1)(0 0 0))))))
  (print-matrix (discrete-convolve x h)))

