
(defun convolve-test ()
  (let* ((h (matrix::scalar-mult (matrix::gaussian-kernel 4 1) 16))
	 (g (matrix::scalar-mult (matrix::gaussian-kernel 4 1) 16))
	 (m (time (matrix::discrete-convolve g h :truncate t))))
    m))

