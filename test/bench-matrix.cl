
(in-package :clem-test)

(defun matrix-bench-1 ()
  (let ((x (random-matrix 512 512 :matrix-class 'double-float-matrix :limit 255d0)))
    (print-range x 0 3 0 3)
    (let ((xb1 (time (copy-to-unsigned-byte-matrix (gaussian-blur x :k 1 :sigma 1))))
	  (xb2 (gaussian-blur x :k 1 :sigma 1)))
      (time (dotimes (i 10) (setf xb2 (gaussian-blur xb2 :k 1 :sigma 1))))
      (print-range xb1 0 3 0 3)
      (print-range xb2 0 3 0 3)
      (print (cons (class-of xb1) (class-of xb2)))
      (let ((ub2 (copy-to-unsigned-byte-matrix (clem::subset-matrix xb2 10 523 10 523))))
	(print-range ub2 0 3 0 3)
	(print (dim xb1))
	(print (dim ub2))
	(let ((q (mat-subtr xb1 ub2)))
	  (print q)
	  (print (sum q))))
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


(defun matrix-bench-6 ()
  (let ((x (random-matrix 512 512 :matrix-class 'double-float-matrix :limit 255d0)))
    (print-range x  0 5 0 5)
    (let ((xb1 (time (gaussian-blur x :k 3 :sigma 1))))
      (print-range xb1 0 5 0 5)
      (print (cons (dim x) (dim xb1))))))

(defun run-bench ()
;  (matrix-bench-3)
  (matrix-bench-4)
;  (matrix-bench-5)
  t)

