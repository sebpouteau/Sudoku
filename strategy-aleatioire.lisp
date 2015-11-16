;;============================
;;==     Implementation     ==
;;==    Strategy Aleatoire  ==
;;============================

(in-package :sudoku)

(defun random-from-list(l n)
  (nth (random n) l))

(defun strat(game)
  (update-possibility-all-square (game-squares game))
  (loop
    for x from 0 to (1- *size*)
    do
       (loop
	 for y from 0 to (1- *size*)
	 do
	    (let* ((square (aref (squares-array (game-squares game))
				 x y))
		   (l (possible-digits square)))
	      
	      (if (not (game-over game))
		  (if (not (protected square))
		      (change-digit (game-squares game) x y (random-from-list l (length l)))
		      nil)))))
  (game-over game))

		    
		  
