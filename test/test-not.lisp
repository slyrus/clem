
(in-package :clem-test)

(macrolet ((define-test (type)
             `(progn
                (defun ,(ch-util:make-intern (concatenate 'string "test-not/" type)) ()
                  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (let ((n (clem::mlognot m)))
                      (print n))))
                (defun ,(ch-util:make-intern (concatenate 'string "test-not/" type "/in-place")) ()
                  (let ((m (array->matrix #2A((1 2 3)(4 5 6)(7 8 9))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (clem::mlognot m :in-place t)
                    (print m)))
                (defun ,(ch-util:make-intern (concatenate 'string "test-not/" type "/3d")) ()
                  (let ((m (array->matrix #3A(((1 2 3)(4 5 6)(7 8 9))
                                              ((11 12 13)(14 15 16)(17 18 19)))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (let ((n (clem::mlognot m)))
                      (print n))))
                (defun ,(ch-util:make-intern (concatenate 'string "test-not/" type "/3d/in-place")) ()
                  (let ((m (array->matrix #3A(((1 2 3)(4 5 6)(7 8 9))
                                              ((11 12 13)(14 15 16)(17 18 19)))
                                          :matrix-class ',(ch-util:make-intern
                                                           (concatenate 'string type "-matrix")))))
                    (clem::mlognot m :in-place t)
                    (print m))))))
  (define-test "integer")
  (define-test "fixnum")
  (define-test "sb8")
  (define-test "sb16")
  (define-test "sb32"))

