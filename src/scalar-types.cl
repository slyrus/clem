
(in-package :clem)

(defclass bit-scalar (bit-matrix scalar)
  ((initial-element :accessor initial-element
                    :initarg :initial-element :initform 0))
  (:metaclass standard-matrix-class)
  (:element-type (unsigned-byte 1))
  (:accumulator-type (unsigned-byte 32))
  (:minval 0)
  (:maxval 1))

(defclass ub8-scalar (ub8-matrix scalar)
  ((initial-element :accessor initial-element
                    :initarg :initial-element :initform 0))
  (:metaclass standard-matrix-class)
  (:element-type (unsigned-byte 8))
  (:accumulator-type (unsigned-byte 32))
  (:minval 0)
  (:maxval #.(- (expt 2 8) 1)))

(defclass sb8-scalar (sb8-matrix scalar)
  ((initial-element :accessor initial-element
                    :initarg :initial-element :initform 0))
  (:metaclass standard-matrix-class)
  (:element-type (signed-byte 8))
  (:accumulator-type (signed-byte 32))
  (:minval #.(- (expt 2 7)))
  (:maxval #.(- (expt 2 7) 1)))

