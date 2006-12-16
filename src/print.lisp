
(in-package :clem)

(defparameter *print-matrix-newlines* t)
(defparameter *print-matrix-float-format* nil)
(defparameter *matrix-print* t)
(defparameter *matrix-print-row-limit* 8)
(defparameter *matrix-print-col-limit* 8)

(defgeneric print-range (m startr endr startc endc &optional stream))
(defmethod print-range ((m matrix)
			(startr fixnum) (endr fixnum)
			(startc fixnum) (endc fixnum)
                        &optional (stream t))
  (let ((val-format-spec (if *print-matrix-float-format*
                             *print-matrix-float-format*
                             (val-format (class-of m)))))
    (format stream "[")
    (do ((i startr (1+ i)))
        ((> i endr))
      (unless (= i startr)
        (princ "; " stream)
        (if *print-matrix-newlines*
            (progn
              (format stream "~&~1,0T"))))
      (do ((j startc (1+ j)))
          ((> j endc))
        (format stream (if (= j startc)
                      val-format-spec
                      (concatenate 'string " " val-format-spec)) (val m i j))))
    (format stream "]")))

(defgeneric print-matrix (m))
(defmethod print-matrix ((m matrix))
  (destructuring-bind (endr endc) (mapcar #'1- (dim m))
    (print-range m 0 endr 0 endc))
  m)

(defun print-matrix-line (obj stream val-format-spec i startc endc lastcol)
  (do ((j startc (1+ j)))
      ((> j endc))
    (format stream (if (= j startc)
                       val-format-spec
                       (concatenate 'string " " val-format-spec)) (val obj i j)))
  (cond ((>= lastcol (1- *matrix-print-col-limit*))
         (if (= lastcol (1- *matrix-print-col-limit*))
             (format stream (concatenate 'string " " val-format-spec) (val obj i lastcol))
             (format stream (concatenate 'string " ... " val-format-spec) (val obj i lastcol))))))

(defmethod print-object ((obj matrix) stream)
  (print-unreadable-object (obj stream :type t :identity (not *matrix-print*))
    (when *matrix-print*
      (let ((startr 0) (endr (min (1- (rows obj)) (- *matrix-print-row-limit* 2)))
            (startc 0) (endc (min (1- (cols obj)) (- *matrix-print-col-limit* 2))))
        (let ((val-format-spec (if *print-matrix-float-format*
                                   *print-matrix-float-format*
                                   (val-format (class-of obj))))
              (lastrow (1- (rows obj)))
              (lastcol (1- (cols obj))))
          (format stream "[")
          (do ((i startr (1+ i)))
              ((> i endr))
            (unless (= i startr)
              (princ "; " stream)
              (if *print-matrix-newlines*
                  (progn
                    (format stream "~&~1,0T"))))
            (print-matrix-line obj stream val-format-spec i startc endc lastcol)) 
          (cond ((>= lastrow (1- *matrix-print-row-limit*))
                 (if (= lastrow (1- *matrix-print-row-limit*))
                     (format stream (concatenate 'string ";~&~1,0T"))
                     (format stream (concatenate 'string ";~& ... ~&~1,0T")))
                 (print-matrix-line obj stream val-format-spec lastrow startc endc lastcol)))
          (format stream "]"))))))

            
