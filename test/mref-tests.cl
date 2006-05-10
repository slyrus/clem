;;; with-row-major-index taken from SBCL, and the check-bound bit
;;; removed. The intention here is to eventually swap in a 1-d array
;;; on which we can do funky things with the indicies, like recycling.
;;; 
(macrolet ((with-row-major-index ((array indices index &optional new-value)
                                  &rest body)
             `(let (n-indices dims)
                (dotimes (i (length ,indices))
                  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
                  (push (make-symbol (format nil "DIM-~D" i)) dims))
                (setf n-indices (nreverse n-indices))
                (setf dims (nreverse dims))
                `(lambda (,',array ,@n-indices
                          ,@',(when new-value (list new-value)))
                   (let* (,@(let ((,index -1))
                                 (mapcar (lambda (name)
                                           `(,name (dim1
                                                    ,',array
                                                    ,(incf ,index))))
                                         dims))
                          (,',index
                           ,(if (null dims)
                                0
                                (do* ((dims dims (cdr dims))
                                      (indices n-indices (cdr indices))
                                      (last-dim nil (car dims))
                                      (form (sb-ext::truly-the sb-kernel::index (car indices))
                                            `(sb-ext:truly-the
                                              sb-kernel::index
                                              (+ (sb-ext:truly-the sb-kernel::index
                                                                   (* (sb-ext:truly-the sb-kernel::index ,form)
                                                                      (sb-ext:truly-the sb-kernel::index ,last-dim)))
                                                 ,(sb-ext::truly-the sb-kernel::index (car indices))))))
                                     ((null (cdr dims)) form)))))
                     (declare (ignorable ,(car dims)))
                     ,',@body)))))
  
  (handler-bind ((simple-error #'continue))
    (sb-c:defknown mref2 (matrix fixnum fixnum) number
                   (sb-c:flushable sb-c:movable)))
  
  (sb-c:deftransform mref2 ((m &rest indices))
    (cond
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-kernel::find-classoid 'double-float-matrix))
       (with-row-major-index (m indices index)
         (row-major-aref
          (the (simple-array double-float (* *))
            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-kernel::find-classoid 'double-float-matrix))
       (with-row-major-index (m indices index)
         (row-major-aref
          (the (simple-array single-float (* *))
            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'ub8-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (unsigned-byte 8) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'ub16-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (unsigned-byte 16) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'ub32-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (unsigned-byte 32) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'sb8-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (signed-byte 8) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'sb16-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (signed-byte 16) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'sb32-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (signed-byte 32) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'bit-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array (unsigned-byte 1) (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'real-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array real (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'complex-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array complex (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'number-matrix))
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (sb-ext:truly-the (simple-array number (* *))
                            (standard-instance-access m 0))
          (sb-ext:truly-the sb-kernel::index index))))
      (t
       (with-row-major-index (m indices index)
         (sb-kernel::hairy-data-vector-ref
          (standard-instance-access m 0)
          (sb-ext:truly-the sb-kernel::index index))))))  
  
  (handler-bind ((simple-error #'continue))
    (sb-c:defknown (setf mref2) (number matrix fixnum fixnum) number
                   (sb-c:flushable sb-c:movable)))
  
  (sb-c:deftransform (setf mref2) ((val m row col))
    (cond
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'double-float-matrix))
       `(setf (aref (the (simple-array double-float (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'single-float-matrix))
       `(setf (aref (the (simple-array single-float (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'ub8-matrix))
       `(setf (aref (the (simple-array (unsigned-byte 8) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'ub16-matrix))
       `(setf (aref (the (simple-array (unsigned-byte 16) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'ub32-matrix))
       `(setf (aref (the (simple-array (unsigned-byte 32) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'sb8-matrix))
       `(setf (aref (the (simple-array (signed-byte 8) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'sb16-matrix))
       `(setf (aref (the (simple-array (signed-byte 16) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'sb32-matrix))
       `(setf (aref (the (simple-array (signed-byte 32) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'bit-matrix))
       `(setf (aref (the (simple-array (unsigned-byte 1) (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'real-matrix))
       `(setf (aref (the (simple-array real (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'complex-matrix))
       `(setf (aref (the (simple-array complex (* *)) (standard-instance-access m 0)) row col) val))
      ((sb-c::csubtypep (sb-c::lvar-type m) (sb-c::find-classoid 'number-matrix))
       `(setf (aref (the (simple-array number (* *)) (standard-instance-access m 0)) row col) val))
      (t
       `(setf (aref (standard-instance-access m 0) row col) val)))))

(defmacro mref3 (a &rest indices)
  `(aref ,a ,@indices))

(defun maref (matrix array &rest subscripts)
  (declare (dynamic-extent subscripts)
           (ignore matrix))
  (apply #'aref array subscripts))

(handler-bind ((simple-error #'continue))
  (sb-c:defknown maref (matrix array &rest sb-c::index) number
                 (sb-c:flushable sb-c:movable)))

(macrolet (;; This is a handy macro for computing the row-major index
           ;; given a set of indices. We wrap each index with a call
           ;; to %CHECK-BOUND to ensure that everything works out
           ;; correctly. We can wrap all the interior arithmetic with
           ;; TRULY-THE INDEX because we know the resultant
           ;; row-major index must be an index.
           (with-row-major-index ((matrix array indices index &optional new-value)
                                  &rest body)
             `(let (n-indices dims)
                (dotimes (i (length ,indices))
                  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
                  (push (make-symbol (format nil "DIM-~D" i)) dims))
                (setf n-indices (nreverse n-indices))
                (setf dims (nreverse dims))
                `(lambda (,',matrix ,',array ,@n-indices
                          ,@',(when new-value (list new-value)))
                   (declare (ignorable ,',matrix))
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

  (sb-c:deftransform maref ((matrix array &rest indices) (t t &rest t))
    (print (sb-kernel::array-type-element-type (sb-c::lvar-type array)))
    (print (sb-c::lvar-type matrix))
    (with-row-major-index (matrix array indices index)
      (sb-kernel::hairy-data-vector-ref
       array
       (sb-ext:truly-the sb-kernel::index index)))))



(define-compiler-macro cm-mref (m a row col)
  (print (sb-cltl2:variable-information m))
  (typecase m
    (double-float-matrix
     `(with-typed-matrix-vals (,m double-float a)
        (declare type (simple-array double-float (* *)) a)
        (aref a ,row ,col)))
    (ub8-matrix
     `(aref ,a ,row ,col))
    (t
     `(aref (matrix-vals ,m) ,row ,col))))




(defun matrix-bench-17 ()
  (let ((x (make-instance 'ub32-matrix :rows 1024 :cols 1024 :initial-element 1)))
    (declare (type ub32-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf acc (+ acc (the (unsigned-byte 32) (clem::mref2 x i j))))))
      (print acc))))



(defun matrix-bench-20 (x)
  (declare (type double-float-matrix x))
  (let ((acc 0d0)
        (rows (rows x))
        (cols (cols x)))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf acc (+ acc (clem::mref2 x i j)))))
    (print acc)))

(defun matrix-bench-21 ()
  (declare (optimize (speed 3)
                     (space 0)
                     (safety 0)))
  (let ((x (make-instance 'double-float-matrix :rows 1024 :cols 1024 :initial-element 1d0)))
    (declare (type double-float-matrix x))
    (let ((acc 0d0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type double-float acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the double-float (clem::mref2 x i j))))))
      (print acc))))

(defun matrix-bench-22 ()
  (declare (optimize (speed 3)
                     (space 0)
                     (safety 0)))
  (let ((x (make-instance 'double-float-matrix :rows 1024 :cols 1024 :initial-element 2d0)))
    (declare (type double-float-matrix x))
    (let ((acc 0d0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type double-float acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the double-float (* (clem::mref2 x i j)
                                                (clem::mref2 x i j)))))))
      (print acc))))

(defun matrix-bench-25 (x)
  (declare (type double-float-matrix x))
  (let ((y (clem:mat-copy-proto x)))
    (declare (type double-float-matrix y))
    (let ((rows (rows x))
          (cols (cols x)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (clem::mref2 y i j) (- (clem::mref2 x i j))))))
    y))

(defun matrix-bench-11 ()
  (declare (optimize (speed 3)
                     (space 0)
                     (safety 0)))
  (let ((x (make-instance 'double-float-matrix :rows 1024 :cols 1024 :initial-element 1d0)))
    (declare (type double-float-matrix x))
    (let ((acc 0d0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type double-float acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the double-float (clem::mref2 x i j))))))
      (print acc))))


(defun matrix-bench-12 ()
  (declare (optimize (speed 3)
                     (space 0)
                     (safety 0)))
  (let ((x (make-instance 'single-float-matrix :rows 1024 :cols 1024 :initial-element 1f0)))
    (declare (type single-float-matrix x))
    (let ((acc 0f0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type single-float acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the single-float (clem::mref2 x i j))))))
      (print acc))))

(defun matrix-bench-14 ()
  (let ((x (make-instance 'single-float-matrix :rows 1024 :cols 1024 :initial-element 1f0)))
    (declare (type single-float-matrix x))
    (let ((acc 0f0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type single-float acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the single-float (clem::mref2 x i j))))))
      (print acc))))

(defun matrix-bench/ub8-matrix-add-mref2 ()
  (let ((x (make-instance 'ub8-matrix :rows 1024 :cols 1024 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the (unsigned-byte 8) (clem::mref2 x i j))))))
      (print acc))))

(defun matrix-bench/ub8-matrix-add-maref ()
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x))
          (v (clem::matrix-vals x)))
      (declare (type (simple-array (unsigned-byte 8) (* *)) v)
               (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf acc (+ acc (the (unsigned-byte 8) (clem::maref x v i j))))))
      (print acc))))

(defun matrix-bench/ub8-matrix-add-wv ()
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (clem::with-matrix-vals (x (unsigned-byte 8) v)
        (dotimes (i rows)
          (declare (type fixnum i))
          (dotimes (j cols)
            (declare (type fixnum j))
            (setf acc (+ acc (the (unsigned-byte 8) (clem::maref x v i j)))))))
      (print acc))))

(defmacro with-typed-matrix-vals ((m element-type a) &body body)
  `(let ((,a (matrix-vals ,m)))
     (declare (type (simple-array ,element-type (* *)) ,a))
     ,@body))

(defun matrix-bench/ub8-matrix-add-wtv ()
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (clem::with-typed-matrix-vals (x (unsigned-byte 8) v)
        (dotimes (i rows)
          (declare (type fixnum i))
          (dotimes (j cols)
            (declare (type fixnum j))
            (setf acc (+ acc (the (unsigned-byte 8) (clem::aref v i j)))))))
      (print acc))))

(defun matrix-bench/ub8-matrix-add-cm ()
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (clem::with-matrix-vals (x (unsigned-byte 8) v)
        (dotimes (i rows)
          (declare (type fixnum i))
          (dotimes (j cols)
            (declare (type fixnum j))
            (setf acc (+ acc (the (unsigned-byte 8) (clem::cm-mref x v i j)))))))
      (print acc))))

