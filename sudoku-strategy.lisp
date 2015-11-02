
;;===========================================
;;==           Implementation              ==
;;==             Fonction IA               ==
;;===========================================

(in-package :sudoku)

(defmethod strategy (game)
(block fin
  ;; Test si il existe un carré n'ayant qu'une possibilité
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (when (not (protected ( aref (squares-array (game-squares game)) x y)))
	      (when (= 1 (length (get-possibility game x y)))
		(return-from fin (values x y (car (get-possibility game x y))))))))
  ;; test plus approfondit
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
		 
	       

))))

(defun verifier-subsquare(game x y)
   (let ((list '())
	(array (squares-array squares))
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ x *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ x *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ y *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ y *sqrt-size*))) *sqrt-size*) 1)) )
     (loop for x from departX to finX do
	  (loop for y from departY to finY do
	       (append (get-possibility game x y) list)))
     ))
