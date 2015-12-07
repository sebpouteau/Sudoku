
;;========================================
;;==           Implémentation           ==
;;==      IA - STRATEGIE ALEATOIRE      ==
;;========================================

(in-package :sudoku)


(defun random-from-list(l n)
  (nth (random n) l))

(defun strategy (game)
  (loop for x from 0 to (1- *size*) do
    (loop for y from 0 to (1- *size*) do
      (let* ((square (aref (squares-array (game-squares game)) x y))
	       (l (possible-digits square))
	       )
	(when (and (not (protected square))
		   (not (endp l)))
	  (let ((value (random-from-list l (length l))))
	    (return-from strategy (values x y value))
	    )
	  )))))

