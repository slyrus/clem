
(in-package :clem-test)

(defstruct %matrix-rep
  (matrix nil :type matrix)
  (array nil :type array))

(handler-bind ((simple-error #'continue))
  (sb-c:defknown mref4 (t t &rest sb-c::index) t
                 (sb-c:flushable sb-c:movable)))

(sb-c::defoptimizer (mref4 sb-c::derive-type) ((matrix array &rest indices) node)
  (sb-c::assert-array-rank array (length indices))
  (sb-c::extract-upgraded-element-type array))

(macrolet ((with-row-major-index ((matrix array indices index &optional new-value)
                                  &rest body)
             `(let (n-indices
                    dims
                    (mt (clem::element-type (sb-c::classoid-pcl-class (sb-c::lvar-type matrix)))))
                (dotimes (i (length ,indices))
                  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
                  (push (make-symbol (format nil "DIM-~D" i)) dims))
                (setf n-indices (nreverse n-indices))
                (setf dims (nreverse dims))
                `(lambda (,',matrix ,',array ,@n-indices
                          ,@',(when new-value (list new-value)))
                   (declare (ignore ,',matrix)
                            (type (simple-array ,mt (* *)) ,',array))
                   (let* (,@(let ((,index -1))
                                 (mapcar (lambda (name)
                                           `(,name (array-dimension
                                                    ,',array
                                                    ,(incf ,index))))
                                         dims))
                          (,',index
                           ,(if (null dims)
                                0
                                (do* ((dims dims (cdr dims))
                                      (indices n-indices (cdr indices))
                                      (last-dim nil (car dims))
                                      (form `(sb-kernel::%check-bound ,',array
                                                                      ,(car dims)
                                                                      ,(car indices))
                                            `(sb-kernel::truly-the
                                              sb-kernel::index
                                              (+ (sb-kernel::truly-the sb-kernel::index
                                                                       (* ,form
                                                                          ,last-dim))
                                                 (sb-kernel::%check-bound
                                                  ,',array
                                                  ,(car dims)
                                                  ,(car indices))))))
                                     ((null (cdr dims)) form)))))
                     ,',@body)))))

  (sb-c:deftransform mref4 ((matrix mr &rest indices))
    (with-row-major-index
        (matrix (%matrix-rep-array mr) indices index)
      (sb-kernel::hairy-data-vector-ref
       (%matrix-rep-array mr)
       (sb-ext:truly-the sb-kernel::index index)))))

(defmacro with-typed-matrix-vals-2 ((m) &body body)
  `(let ((mr (make-%matrix-rep
              :matrix ,m
              :array (clem::matrix-vals ,m))))
     ,@body))


(defun matrix-bench/ub8-matrix-add-4 ()
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (with-typed-matrix-vals-2 (x)
        (declare (type (unsigned-byte 32) acc)
                 (type fixnum rows cols))
        (let ((v (clem::matrix-vals x)))
          (dotimes (i rows)
            (declare (type fixnum i))
            (dotimes (j cols)
              (declare (type fixnum j))
              (setf acc (+ acc  (mref4 x  i j))))))
        (print acc)))))

  

