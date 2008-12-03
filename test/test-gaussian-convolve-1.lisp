
(in-package :matrix)

(let ((x (normalize (random-matrix 3 3)))
      (h (matrix::gaussian-kernel 2 1)))
  (print-matrix x)
;  (print-matrix h)
;  (print (sum h))
  
  (let* ((rowstart (floor (/ (1- (matrix::rows h)) 2)))
	 (rowend (floor (/ (matrix::rows h) 2)))
	 
	 (colstart (floor (/ (1- (matrix::cols h)) 2)))
	 (colend (floor (/ (matrix::cols h) 2)))

	 (h1 (matrix::subset-matrix h rowstart rowend 0 (1- (matrix::cols h))))
	 (h2 (matrix::subset-matrix h 0 (1- (matrix::rows h)) colstart colend)))

    (matrix::scalar-divide h1 (matrix::sum h1))
    (matrix::scalar-divide h2 (matrix::sum h2))

    (matrix::print-matrix h1)
    (matrix::print-matrix h2)
    (let* 
	((x1 (matrix::discrete-convolve-ppc x h1))
	 (x2 (matrix::discrete-convolve-ppc x1 h2)))
      (matrix::print-matrix x1)
      (matrix::print-matrix x2))
    )

  (print-matrix (gaussian-blur x 2 1))
  )

;(in-package :cl-user)
;(defvar h)
;(setf h (matrix::gaussian-kernel 1 1))
;(setf k (matrix::gaussian-kernel 1 1))
;(matrix::print-matrix (matrix::gradmag h))