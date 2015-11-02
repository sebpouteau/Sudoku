
;;===========================================
;;==           Implementation              ==
;;==             Fonction IA               ==
;;===========================================

(in-package :sudoku)

(defvar x nil)
(defvar y nil)
(defmethod strategy (game)
  (update-possibility-all-square (game-squares game))
  (block fin
    ;; Test si il existe un carré n'ayant qu'une possibilité
    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
	(when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
		   (= 1 (length (get-possibility game x y)))
		   (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
	  (return-from fin (values x y (car (get-possibility game x y)))))))
    
    ;; test plus approfondit
    ;(loop for y from 0 to (1- *size*) do
       ;(loop for x from 0 to (1- *size*) do
	 ;(when (not (protected ( aref (squares-array (game-squares game)) x y)))

    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
	(let ((list (verifier-subsquare game x y )))
	  (if (and (= (length list) 1)
		   (not (protected ( aref (squares-array (game-squares game)) x y)))
		   (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
	      (return-from fin (values x y (car list))))
      
    
    )))))

 
(defun verifier-subsquare(game coorX coorY)
   (let ((list (get-possibility game coorX coorY))
	
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ coorX *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ coorX *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ coorY *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ coorY *sqrt-size*))) *sqrt-size*) 1)) )
     (loop for x from departX to finX do
       (loop for y from departY to finY do
	 (unless (or (and (eq x coorX)
			  (eq y coorY))
		     (protected ( aref (squares-array (game-squares game)) x y)))
	   (setf list (remove-sublist (get-possibility game x y) list)))
	 
	     ))
     (unless (endp list)
       list)
))

(defun test (game)
  (loop while (not (eq (strategy game) NIL)) do
       (multiple-value-bind (x y value) (strategy game)
	 (change-digit game x y value))
       (setf (
       
  (print-grid game)))

