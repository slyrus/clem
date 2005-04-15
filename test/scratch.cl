;;; scratch stuff for matrix messin'

(in-package :clem-test)
(defparameter m1 (make-instance 'clem::double-float-matrix :rows 512 :cols 512))
(time (clem::sum m1))
