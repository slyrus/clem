
(in-package :clem)

(defgeneric mat-scale-range-2 (m n q startr endr startc endc))
(defgeneric mat-scale-2 (m n q))

(defmacro def-matrix-scale-2 (input-class result-class)
  (let ((input-class-element-type (element-type (find-class `,input-class)))
	(result-class-element-type (element-type (find-class `,result-class))))
    `(progn
       (defmethod mat-scale-range-2
	   ((m ,input-class) (n ,result-class) q startr endr startc endc)
         (with-typed-mref (m ,input-class-element-type)
           (with-typed-mref (n ,result-class-element-type)
             (do ((i startr (1+ i)))
                 ((> i endr))
               (declare (dynamic-extent i) (type fixnum i))
               (do ((j startc (1+ j)))
                   ((> j endc))
                 (declare (dynamic-extent j) (type fixnum j))
                 (setf (mref n i j) (* (mref m i j) q))))))
         n)
       
       (defmethod mat-scale-2
	   ((m ,input-class) (n ,result-class) q)
         (with-typed-mref (m ,input-class-element-type)
           (with-typed-mref (n ,result-class-element-type)
             (loop for i from 0 below (matrix-total-size m)
                do (setf (row-major-mref n i)
                         (* (row-major-mref m i) q))))
           n)))))

(progn
  (def-matrix-scale-2 double-float-matrix double-float-matrix)
  (def-matrix-scale-2 single-float-matrix single-float-matrix)
  (def-matrix-scale-2 ub8-matrix ub8-matrix)
  (def-matrix-scale-2 ub8-matrix ub16-matrix)
  (def-matrix-scale-2 ub8-matrix ub32-matrix)
  (def-matrix-scale-2 ub16-matrix ub16-matrix)
  (def-matrix-scale-2 ub16-matrix ub32-matrix)
  (def-matrix-scale-2 ub32-matrix ub32-matrix)
  (def-matrix-scale-2 sb8-matrix sb8-matrix)
  (def-matrix-scale-2 sb8-matrix sb16-matrix)
  (def-matrix-scale-2 sb8-matrix sb32-matrix)
  (def-matrix-scale-2 sb16-matrix sb16-matrix)
  (def-matrix-scale-2 sb16-matrix sb32-matrix)
  (def-matrix-scale-2 sb32-matrix sb32-matrix)
  (def-matrix-scale-2 bit-matrix ub8-matrix)
  (def-matrix-scale-2 bit-matrix sb32-matrix)
  (def-matrix-scale-2 fixnum-matrix fixnum-matrix)
  (def-matrix-scale-2 real-matrix real-matrix)
  (def-matrix-scale-2 integer-matrix integer-matrix)
  (def-matrix-scale-2 complex-matrix complex-matrix)
  (def-matrix-scale-2 t-matrix t-matrix))

(defgeneric compute-mat-scale-result-class (m q))

(defmethod compute-mat-scale-result-class (m q)
  (typecase m
    (double-float-matrix 'double-float-matrix)
    (single-float-matrix
     (typecase q
       (double-float 'double-float-matrix)
       (t 'single-float-matrix)))
    (unsigned-byte-matrix
     (typecase q
       (double-float 'double-float-matrix)
       (single-float 'single-float-matrix)
       (t (cond
            ((floatp q) 'double-float-matrix)
            ((minusp q) 'sb32-matrix)
            (t 'ub32-matrix)))))
    (integer-matrix
     (typecase q
       (double-float 'double-float-matrix)
       (single-float 'single-float-matrix)
       (t (cond
            ((floatp q) 'double-float-matrix)
            (t 'sb32-matrix)))))
    (bit-matrix
     (typecase q
       (double-float 'double-float-matrix)
       (single-float 'single-float-matrix)
       (t (cond
            ((floatp q) 'double-float-matrix)
            (t 'sb32-matrix)))))))

(defmethod mat-scale (m q
                      &key
                      in-place
                      (result-class (unless in-place
                                      (compute-mat-scale-result-class m q))))
  (if in-place
      (mat-scale-2 m m q)
      (let ((n (make-instance result-class :dimensions (dim m))))
        (mat-scale-2 m n q))))
