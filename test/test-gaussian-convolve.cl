

(in-package :matrix)

; (matrix::print-matrix (matrix::scalar-mult (matrix::gaussian-kernel 3 1) 255))

(let ((x (copy-to-unsigned-byte-matrix (normalize (random-matrix 2 2)))))
  (let ((xb1 (time (gaussian-blur-word x 1 1 :ppc nil)))
	(xb2 (time (gaussian-blur x 1 1 :ppc t)))
	)
;    (print-matrix x)
;    (print-matrix xb1)
;    (print-matrix xb2)
    (print (sum (mat-subtr xb1 xb2)))
    ))

;(let ((h (matrix::copy-to-unsigned-byte-matrix
;	  (matrix::scalar-mult
;	   (matrix::gaussian-kernel 2 1) 255))))
;  (print-matrix h))
