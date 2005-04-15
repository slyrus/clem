
(in-package :clem-test)

(defun matrix-bench-1 ()
  (let ((x (copy-to-unsigned-byte-matrix (normalize (random-matrix 128 128)))))
    (let ((xb1 (time (gaussian-blur-word x 1 1 :ppc nil)))
	  (xb2 (time (gaussian-blur x 1 1 :ppc t)))
	  )
      (print-range xb1 0 3 0 3)
      (print-range xb2 0 3 0 3)
      (print (sum (mat-subtr xb1 xb2)))
      )))

(defun matrix-bench-2 ()
  (let* ((r 64) (c 64)
	 (x (copy-to-unsigned-byte-matrix (normalize (random-matrix r c)))))
    (time
     (dotimes (i r)
       (dotimes (j c)
	 (val x i j))))))

(defun matrix-bench-3 ()
  (let* ((x (random-fixnum-matrix 512 512 :max 255))
	 (y (random-fixnum-matrix 5 5 :max 255)))
    (time
     (let ((conv (discrete-convolve x y :truncate t)))))))

(defun matrix-bench-4 ()
  (let* ((x (random-unsigned-byte-matrix 512 512 :max 255))
	 (y (random-unsigned-byte-matrix 5 5 :max 255)))
    (time
     (let ((conv (discrete-convolve x y :matrix-class 'unsigned-long-matrix :truncate t :norm-v nil)))))
    ))

(defun matrix-bench-5 ()
  (let* ((x (random-double-float-matrix 512 512 :max 1.0d0))
	 (y (random-double-float-matrix 5 5 :max 1.0d0)))
    (time
     (let ((conv (discrete-convolve x y :matrix-class 'double-float-matrix :truncate t :norm-v nil)))))
    ))

(defun run-bench ()
;  (matrix-bench-3)
  (matrix-bench-4)
;  (matrix-bench-5)
  t)

