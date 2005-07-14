
(in-package :clem-test)

(defun convolve-test ()
  (let* ((h (clem::mat-scale (clem::gaussian-kernel 2 1) 16d0))
	 (g (make-instance 'double-float-matrix :rows 512 :cols 512
                           :initial-element 16d0))
	 (m (time (clem::discrete-convolve g h :truncate t))))
    m))
