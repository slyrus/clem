
(in-package :matrix)

; (matrix::print-matrix (matrix::scalar-mult (matrix::gaussian-kernel 3 1) 255))

(defun mean-orig (m)
  (destructuring-bind (r c) (dim m)
    (let ((acc 0))
      (dotimes (i r)
	(dotimes (j c)
	  (incf acc (val m i j))))
      (/ acc (* r c)))))

(let ((x (array->matrix #2A((213 43 56)(29 104 191)(2 45 112)))))
  (print-matrix x)
  (print (matrix::mean x))
  (print (sum x))
  (print (variance x))
  (print (sqrt (variance x)))
  (print (sample-variance x))
  (print (sqrt (sample-variance x))))

;(let ((x (copy-to-ub8-matrix (normalize (random-matrix 32 32)))))
;  (print-matrix x)
;  (print (matrix::mean x))
;  (print-matrix (subset-matrix x 2 4 2 4))
;  (print (matrix::mean-range x 2 4 2 4))
;  (print (sum x))
;  (print (variance-range x 2 4 2 4))
;  (print (sqrt (variance-range x 2 4 2 4)))
;  )
	 
