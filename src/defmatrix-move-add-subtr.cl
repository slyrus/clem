
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
  (frob double-float-matrix unsigned-byte-matrix double-float-matrix)
  (frob double-float-matrix unsigned-word-matrix double-float-matrix)
  (frob double-float-matrix unsigned-long-matrix double-float-matrix)
  (frob double-float-matrix signed-byte-matrix double-float-matrix)
  (frob double-float-matrix signed-word-matrix double-float-matrix)
  (frob double-float-matrix signed-long-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob single-float-matrix unsigned-byte-matrix single-float-matrix)
  (frob single-float-matrix unsigned-word-matrix single-float-matrix)
  (frob single-float-matrix unsigned-long-matrix single-float-matrix)
  (frob single-float-matrix signed-byte-matrix single-float-matrix)
  (frob single-float-matrix signed-word-matrix single-float-matrix)
  (frob single-float-matrix signed-long-matrix single-float-matrix)
  (frob single-float-matrix bit-matrix single-float-matrix)
  (frob single-float-matrix fixnum-matrix single-float-matrix)

  (frob unsigned-byte-matrix unsigned-byte-matrix unsigned-byte-matrix)
  (frob unsigned-word-matrix unsigned-word-matrix unsigned-word-matrix)
  (frob unsigned-long-matrix unsigned-long-matrix unsigned-long-matrix)

  (frob unsigned-byte-matrix bit-matrix unsigned-byte-matrix)
  (frob unsigned-word-matrix bit-matrix unsigned-word-matrix)
  (frob unsigned-long-matrix bit-matrix unsigned-long-matrix)

  (frob signed-byte-matrix bit-matrix signed-byte-matrix)
  (frob signed-word-matrix bit-matrix signed-word-matrix)
  (frob signed-long-matrix bit-matrix signed-long-matrix)
  
  (frob signed-long-matrix unsigned-byte-matrix signed-long-matrix)
  (frob signed-long-matrix unsigned-word-matrix signed-long-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2)
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  (frob single-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix unsigned-byte-matrix double-float-matrix)
  (frob double-float-matrix unsigned-word-matrix double-float-matrix)
  (frob double-float-matrix unsigned-long-matrix double-float-matrix)
  (frob double-float-matrix signed-byte-matrix double-float-matrix)
  (frob double-float-matrix signed-word-matrix double-float-matrix)
  (frob double-float-matrix signed-long-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob unsigned-byte-matrix double-float-matrix double-float-matrix)
  (frob unsigned-byte-matrix single-float-matrix single-float-matrix)

  (frob unsigned-word-matrix double-float-matrix double-float-matrix)
  (frob unsigned-word-matrix single-float-matrix single-float-matrix)

  (frob unsigned-long-matrix double-float-matrix double-float-matrix)
  (frob unsigned-long-matrix single-float-matrix single-float-matrix)

  (frob signed-byte-matrix double-float-matrix double-float-matrix)
  (frob signed-byte-matrix single-float-matrix single-float-matrix)

  (frob signed-word-matrix double-float-matrix double-float-matrix)
  (frob signed-word-matrix single-float-matrix single-float-matrix)

  (frob signed-long-matrix double-float-matrix double-float-matrix)
  (frob signed-long-matrix single-float-matrix single-float-matrix))


