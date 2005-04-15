
(in-package :matrix)

(let ((x (copy-to-unsigned-byte-matrix (normalize (random-matrix 10 10))))
      (h (array->matrix #2A((0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0))))
;      (h (array->matrix #2A((0 0 0)(0 0 0)(0 0 0))))
      )
;  (print-matrix x)
;  (print-matrix (trim-one (dilate2 x h) 1))
;  (print-matrix (trim-one (time (dilate3 x h)) 1)))
  (let* ((d (trim-one (dilate x h) 2))
	 (e (trim-one (erode d h) 2)))
    
    (print-matrix d)
    (print-matrix e)
    (print-matrix (mat-subtr d e))
    )  )

