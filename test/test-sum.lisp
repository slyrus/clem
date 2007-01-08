
(in-package :clem-test)

(macrolet ((define-test (type)
             `(progn
                (defun ,(ch-util:make-intern (concatenate 'string "test-sum/" type)) ()
                  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (let ((n (clem::sum m)))
                      (print n))))
                (defun ,(ch-util:make-intern (concatenate 'string "test-sum/" type "/3d")) ()
                  (let ((m (array->matrix #3A(((1 2 3)(4 5 6)(7 8 9))
                                              ((11 12 13)(14 15 16)(17 18 19)))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (let ((n (clem::sum m)))
                      (print n))))

                (defun ,(ch-util:make-intern (concatenate 'string "test-sum-square/" type)) ()
                  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (let ((n (clem::sum-square m)))
                      (print n))))
                (defun ,(ch-util:make-intern (concatenate 'string "test-sum-square/" type "/3d")) ()
                  (let ((m (array->matrix #3A(((1 2 3)(4 5 6)(7 8 9))
                                              ((11 12 13)(14 15 16)(17 18 19)))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (let ((n (clem::sum-square m)))
                      (print n)))))))

  (define-test "double-float")
  (define-test "single-float")

  (define-test "ub8")
  (define-test "ub16")
  (define-test "ub32")
  (define-test "sb8")
  (define-test "sb16")
  (define-test "sb32")

  (define-test "integer")
  (define-test "fixnum")
  (define-test "number")
  (define-test "real"))

(defun test-sum-square/double-float ()
  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                          :matrix-class 'double-float-matrix)))
    (let ((n (clem::sum-square m)))
      (print n))))

(defun test-sum-square/single-float ()
  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                          :matrix-class 'double-float-matrix)))
    (let ((n (clem::sum-square m)))
      (print n))))

