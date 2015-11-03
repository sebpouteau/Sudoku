
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
    
    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
	(let ((list (verifier-subsquare game x y )))
	  (if (and (= (length list) 1)
		   (not (protected ( aref (squares-array (game-squares game)) x y)))
		   (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
	      (return-from fin (values x y (car list)))))))

    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
	(let ((listL (verifier-line game x y 'line))
	      (listC (verifier-line game x y 'column)))3
	  (when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
		     (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
	    (when (= (length listL) 1)
	      (return-from fin (values x y (car listL))))
	    (when (= (length listC) 1)
	      (return-from fin (values x y (car listC))))	    
	    ))))
    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
	(when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
		   (not (eq (get-possibility game x y) NIL))
		   (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
	  (print "profondeur")
	  (loop for v in (get-possibility game x y) do
	    (if (etude-profondeur game x y v)
		(return-from fin (values x y v))
		
		)))))))

 
(defun verifier-subsquare(game coorX coorY)
  (let ((list (get-possibility game coorX coorY))
	
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ coorX *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ coorX *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ coorY *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ coorY *sqrt-size*))) *sqrt-size*) 1)) )
    (loop for x from departX to finX do
      (loop for y from departY to finY do
	;; si non protéger et différent de a coordonné de départ
	(unless (and (eq x coorX)
			 (eq y coorY))
		    ;(protected ( aref (squares-array (game-squares game)) x y)))
	  (setf list (remove-sublist (get-possibility game x y) list)))
	    ))
    (unless (endp list)
      list)
    ))


(defun verifier-line ( game coorX coorY &optional (sens 'line))
  (assert (or (eq sens 'line) (eq sens 'column)))
  (labels ((line-column (x y)
	     (if (eq sens 'line)
		 (list y x)
		 (list x y))))
    (let ((list (get-possibility game coorX coorY))
	  (indiceStatic (if (eq sens 'line)
			    coorX
			    coorY)))
      
	  (loop for indiceMovible from 0 to (1- *size*) do    
	 ;; pour parcourir les colonnes c'est le x qui est statique 
	    ;; pour parcourir les lignes c'est le y qui est statique
	    (let* ((coor (line-column indiceMovible indiceStatic)))
		   (unless (and (eq (car coor) coorX)
				    (eq (cadr coor)  coorY))
			       ;(protected ( aref (squares-array (game-squares game)) (car coor ) (cadr coor))))
		     (setf list (remove-sublist (get-possibility game (car coor) (cadr coor)) list)))
		   ))
	  (unless (endp list)
	    list))))

      
(defun jeu (game)
  (block fin
    (loop while (and (not (eq (strategy game) NIL))
		     (not (game-over game))) do
      (multiple-value-bind (x y value) (strategy game)
	(change-digit game x y value))
      (print-grid game)
      )))

(defun test (game)
 
    (loop while (not (eq (strategy game) NIL)) do
      (multiple-value-bind (x y value) (strategy game)
	(change-digit game x y value))       
	  )
  (game-over game :print T))

(defun etude-profondeur(game x y value)
  (let ((oldSquares (copy-squares (game-squares game)))
	(bool NIL))
    (change-digit game x y value)
    (update-possibility-all-square (game-squares game))
    
    (when (eq (test game) 'win)
      (setf bool T))
    
    
					;     (when (eq (test game) NIL)
					;	(loop for y from 0 to (1- *size*) do
					;	  (loop for x from 0 to (1- *size*) do
					;	    (when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
					;		       (not (eq (get-possibility game x y) NIL))
					;		       (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
					;	      (print "prof2")
					;	      (loop for v in (get-possibility game x y) do
					;		(when (etude-profondeur game x y v)
					;		    (setf (game-squares game) oldSquares)
					;		    (return-from fin T)))))))
    (setf (game-squares game) oldSquares)
    bool
    )
  )


  
