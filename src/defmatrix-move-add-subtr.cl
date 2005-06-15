
(in-package :clem)

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2)
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-add! ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr! ,type-1 ,type-2 ,type-3 :suffix ,suffix))))

  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix ub8-matrix double-float-matrix)
  (frob double-float-matrix ub16-matrix double-float-matrix)
  (frob double-float-matrix ub32-matrix double-float-matrix)
  (frob double-float-matrix sb8-matrix double-float-matrix)
  (frob double-float-matrix sb16-matrix double-float-matrix)
  (frob double-float-matrix sb32-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob single-float-matrix ub8-matrix single-float-matrix)
  (frob single-float-matrix ub16-matrix single-float-matrix)
  (frob single-float-matrix ub32-matrix single-float-matrix)
  (frob single-float-matrix sb8-matrix single-float-matrix)
  (frob single-float-matrix sb16-matrix single-float-matrix)
  (frob single-float-matrix sb32-matrix single-float-matrix)
  (frob single-float-matrix bit-matrix single-float-matrix)
  (frob single-float-matrix fixnum-matrix single-float-matrix)

  (frob ub8-matrix ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix ub32-matrix)

  (frob ub8-matrix bit-matrix ub8-matrix)
  (frob ub16-matrix bit-matrix ub16-matrix)
  (frob ub32-matrix bit-matrix ub32-matrix)

  (frob sb8-matrix bit-matrix sb8-matrix)
  (frob sb16-matrix bit-matrix sb16-matrix)
  (frob sb32-matrix bit-matrix sb32-matrix)
  
  (frob sb32-matrix ub8-matrix sb32-matrix)
  (frob sb32-matrix ub16-matrix sb32-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2)
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  (frob single-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix ub8-matrix double-float-matrix)
  (frob double-float-matrix ub16-matrix double-float-matrix)
  (frob double-float-matrix ub32-matrix double-float-matrix)
  (frob double-float-matrix sb8-matrix double-float-matrix)
  (frob double-float-matrix sb16-matrix double-float-matrix)
  (frob double-float-matrix sb32-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob ub8-matrix double-float-matrix double-float-matrix)
  (frob ub8-matrix single-float-matrix single-float-matrix)

  (frob ub16-matrix double-float-matrix double-float-matrix)
  (frob ub16-matrix single-float-matrix single-float-matrix)

  (frob ub32-matrix double-float-matrix double-float-matrix)
  (frob ub32-matrix single-float-matrix single-float-matrix)

  (frob sb8-matrix double-float-matrix double-float-matrix)
  (frob sb8-matrix single-float-matrix single-float-matrix)

  (frob sb16-matrix double-float-matrix double-float-matrix)
  (frob sb16-matrix single-float-matrix single-float-matrix)

  (frob sb32-matrix double-float-matrix double-float-matrix)
  (frob sb32-matrix single-float-matrix single-float-matrix))


