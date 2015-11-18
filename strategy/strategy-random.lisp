
;;========================================
;;==           Implémentation           ==
;;==      IA - STRATEGIE ALEATOIRE      ==
;;========================================

(in-package :sudoku)


(defun random-from-list(l n)
  (nth (random n) l))

(defun strategy (game)
  (update-possibility-all-square (game-squares game))
  (loop for x from 0 to (1- *size*) do
    (loop for y from 0 to (1- *size*) do
      (let* ((square (aref (squares-array (game-squares game)) x y))
	       (l (possible-digits square))
	       )
	(when (and (not (protected square))
		   (not (endp l)))
	  (let ((value (random-from-list l (length l))))
	    (change-digit game x y value)
	    (return-from strategy (values x y value))
	    )
	  )))))

(defun update-after-change-digit(game x y)
  "Update les possibilités de la ligne, colonne, sous-carré en [x,y]"
  (update-possibility-subsquares (game-squares game) x y)
  (update-possibility-line (game-squares game) x 'line)
  (update-possibility-line (game-squares game) y 'column)
  )
