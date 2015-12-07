
;;===========================================
;;==           Implémentation              ==
;;==          FONCTIONS SUDOKU             ==
;;===========================================

(in-package :sudoku)


;; ---------------------
;; --   Coordonnées   --
;; ---------------------

(defmethod make-coor (x y)
  (make-instance 'coor :x x :y y)
  )


;; ---------------------
;; --      Carré      --
;; ---------------------

(defmethod make-square (coor &optional digit)
  (make-instance 'square :coor coor :digit digit)
  )


(defmethod assigned-p (square)
  (and (< 0 (digit square)) (<= (digit square) *size*))
  )


(defmethod copy-square(square)
  (let ((s (make-square (coor square) (digit square))))
    (setf (possible-digits s) (possible-digits square))
    (setf (protected s) (protected square))
    s)
  )


(defmethod coor-square (squares coor)
  (aref (squares-array squares) (x-coor coor) (y-coor coor))
  )


;; ----------------------
;; --      Grille      --
;; ----------------------

(defmethod make-squares()
  (let (( squares (make-instance 'squares)))
    (grid-to-square squares)
    squares)
  )


(defmethod make-squares-array (size)
  (make-array (list size size))
  )


(defmethod grid-to-square (squares)
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (setf (aref (squares-array squares) x y)
		  (make-square (make-coor x y) 0))))
  )


(defmethod change-digit ((squares squares) x y value)
  (if (= (digit (aref (squares-array squares) x y)) 0)
      (progn
	(setf (digit (aref (squares-array squares) x y)) value)
	(setf (to-fill squares) (1- (to-fill squares))))
      (progn
	(setf (digit (aref (squares-array squares) x y)) value)
	(update-possibility-all-square squares)))
  )


(defmethod copy-squares(squares)
  (let ((s (make-squares)))
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (setf (aref (squares-array s) x y)
		    (copy-square (aref (squares-array squares) x y)))))
    (setf (to-fill s) (to-fill squares))
    s)
  )


(defmethod update-possibility (squares x y list)
  (if (eq (digit (aref (squares-array squares) x y)) 0) 
      (setf (possible-digits (aref (squares-array squares) x y))
	    (remove-sublist list (get-possibility squares x y)))
      (setf (possible-digits (aref (squares-array squares) x y))
	    NIL))
  )

(defmethod remove-sublist(list1 list2)
  (set-difference list2 list1))


(defmethod update-possibility-line (squares indiceStatic sens)
  (assert (or (eq sens 'line) (eq sens 'column)))
  (let ((list '())
	(array (squares-array squares)))
    (loop for indiceMovible from 0 to (1- *size*) do    
       ;; Pour parcourir les colonnes c'est le x qui est statique 
       ;; Pour parcourir les lignes c'est le y qui est statique
	 (let* ((coor (line-column sens indiceMovible indiceStatic))
		(value (digit(aref array (car coor) (cadr coor)))))
	   (if (not (zerop value))
	       (setf list (cons value list)))))
    (loop for indiceMovible from 0 to (1- *size*) do    
	 (let ((coor (line-column sens indiceMovible indiceStatic)))
	   (update-possibility squares (car coor) (cadr coor) list))))
  )

 
(defmethod update-possibility-subsquares (squares x y)
  (let ((list '())
	(array (squares-array squares))
	;; Définition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ x *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ x *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ y *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ y *sqrt-size*))) *sqrt-size*) 1)))
    
    (loop for x from departX to finX do
	 (loop for y from departY to finY do
	      (let ((value (digit(aref array x y))))
		(if (not (zerop value))
		    (setf list (cons value list))))))
    (loop for x from departX to finX do
	 (loop for y from departY to finY do
	      (update-possibility squares x y list))))
  )


(defmethod update-possibility-all-square (squares)
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (setf (possible-digits (aref (squares-array squares) x y))
		  *digits*)))
  ;; Met à jour les possible-digits de chaque carré, par rapport aux lignes et aux colonnes
  (loop for x from 0 to (1- *size*) do
       (update-possibility-line squares x 'line )
       (update-possibility-line squares x 'column ))
  ;; Met à jour les possible-digits des 9 sous-carrés
  (loop for y from 0 to (1- *sqrt-size*) do
       (loop for x from 0 to (1- *sqrt-size*) do
	    (update-possibility-subsquares squares 
					   (* x *sqrt-size*) 
					   (* y *sqrt-size*))))
  )


(defmethod update-after-change-digit(game x y)
  (update-possibility-subsquares (game-squares game) x y)
  (update-possibility-line (game-squares game) x 'line)
  (update-possibility-line (game-squares game) y 'column)
  )

(defmethod make-game (grid)
  (let ((game (make-instance 'game
			     :game-squares (make-squares)
			     :initial-grid (list-to-2d-array
					    (read (open grid))))))
    (init-game game)
    game)
  )


(defmethod init-game (game)
  (setf (to-fill (game-squares game)) (* *size* *size*))
  (loop for x from 0 to (1- *size*) do
       (loop for y from 0 to (1- *size*) do
	    (setf (digit (aref (squares-array (game-squares game))
			       x y))
		  (aref (initial-grid game) x y))
	    (unless (zerop (aref (initial-grid game) x y))
	      (setf (to-fill (game-squares game)) (1- (to-fill (game-squares game))))
	      (setf (possible-digits (aref (squares-array (game-squares game)) x y)) '() )
  	      (setf (protected (aref (squares-array (game-squares game)) x y)) T ))))
  (update-possibility-all-square (game-squares game))
  )


(defmethod change-digit ((squares game) x y value)
  (change-digit (game-squares squares) x y value)
  )


(defmethod game-do (game square)
  (change-digit game 
		(x-coor (coor square)) 
		(y-coor (coor  square)) 
		(digit square))
  (update-after-change-digit game
			     (x-coor (coor square)) 
			     (y-coor (coor  square)))
  )


(defmethod get-possibility ((square squares) x y)
  (possible-digits (aref (squares-array square) x y))
  )

(defmethod get-possibility ((square game) x y)
  (get-possibility (game-squares square) x y)
  )


(defmethod game-over(game)
  (loop for x from 0 to (1- *size*) do
       (loop for y from 0 to (1- *size*) do
	    (let ((square (aref (squares-array (game-squares game)) x y)))
	      (cond ((and (= (digit square) 0)
			  (eq (possible-digits square) NIL))
		     (return-from game-over 'lose))
		    ((= (to-fill (game-squares game)) 0)
		     (return-from game-over 'win))
		    ))))
  )

;; cette fonction ne sert que pour l'interface web
(defmethod game-with-new-grid(&optional strategy)
  (declare (ignore strategy))
  (let ((game (make-game "Grids/1.sudoku")))
    (init-game game)
    game)
  )
