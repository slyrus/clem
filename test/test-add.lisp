
(in-package :clem-test)

(defun test-add/3d ()
  (let ((m
         (clem::m+ (make-instance 'double-float-matrix :dimensions '(3 3 3) :initial-element 2d0)
                   (make-instance 'double-float-matrix :dimensions '(3 3 3) :initial-element 3d0)
                   (make-instance 'double-float-matrix :dimensions '(3 3 3) :initial-element 4d0)))
        (acc 0d0))
    (dotimes (i 3)
      (dotimes (j 3)
        (dotimes (k 3)
          (incf acc (mref m i j k)))))
    (print acc)))
