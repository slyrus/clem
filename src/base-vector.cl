;;;
;;; file: base-vector.cl
;;; author: cyrus harmon
;;;

;;; this might all go away soon, but for the moment put some stuff
;;; that is common between row-vector and column-vector in here.

(in-package :clem)

(defclass base-vector (matrix) ())

(defgeneric vec-dim (vec))
(defgeneric vec-val (vec i))
(defgeneric vec-set-val (vec i val))
