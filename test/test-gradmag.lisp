
(in-package :clem-test)

(defun gradmag-test-matrix (k &key (matrix-class 'double-float-matrix))
  (let ((m (make-instance matrix-class :rows k :cols k)))
    (dotimes (i k)
      (dotimes (j k)
        (clem::set-val-fit m i j (/ (* i j) 2) :truncate t)))
    m))

(defun gradmag-test-1 ()
  (let* ((g (gradmag-test-matrix 8 :matrix-class 'sb8-matrix)))
    (print g)
    (clem:gradmag g)))

(defun gradmag-test-2 ()
  (let* ((g (gradmag-test-matrix 8 :matrix-class 'sb8-matrix)))
    (print g)
    (clem:gradmag g)))
