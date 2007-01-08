
(in-package :clem-test)

(defun test-normalize ()
  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                          :matrix-class 'double-float-matrix)))
    (let ((n (clem::norm-0-255 m)))
      (print n))))

(defun test-normalize/3d ()
  (let ((m (array->matrix #3A(((1 2 3)(4 5 6)(7 8 9))
                              ((11 12 13)(14 15 16)(17 18 19)))
                          :matrix-class 'double-float-matrix)))
    (let ((n (clem::norm-0-255 m)))
      (print n))))
