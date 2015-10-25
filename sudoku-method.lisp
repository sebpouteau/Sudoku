;;===========================================
;;==           Implémentation              ==
;;==          FONCTIONS SUDOKU             ==
;;===========================================


(in-package :sudoku)


;; ========================
;; ==     Coordonnées    ==
;; ========================

(defmethod make-coor (x y)
  (make-instance 'coor :x x :y y))


;; ===================
;; ==     Carre     ==
;; ===================

(defmethod make-square (coor &optional digit)
  (make-instance 'square :coor coor :digit digit))


(defmethod assigned-p (square)
  (and (< 0 (digit square)) (<= (digit square) *size*)))


(defmethod copy-square(square)
  (let (( s (make-square
	     (coor square) (digit square)))) ;; s -> nouveau square
    (setf (possible-digits s) (possible-digits square)) ;; copie de possible-digits de square
    (setf (protected s) (protected square)) ;; copie de protected de square
    s))


;; =====================
;; ==     Squares     ==
;; =====================

(defmethod make-squares()
  (let (( squares (make-instance 'squares)))
    (grid-to-square squares)
    squares))


(defmethod make-squares-array (size)
  (make-array (list size size)))


(defmethod grid-to-square (squares)
  (loop for x from 0 to (1- *size*) do
       (loop for y from 0 to (1- *size*) do
	    (setf (aref (squares-array squares) x y)
		  (make-square (make-coor x y)  0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod change-digit ((squares squares) x y value)
  (when (protected (aref (squares-array squares) x y))
    (print "Impossible de changer cette case, Valeur Initial"))
  (unless (protected (aref (squares-array squares) x y))
    (setf (digit (aref (squares-array squares) x y)) value)
    (setf (possible-digits (aref (squares-array squares) x y)) '())
    (update-possibility-line squares y 'line 'update-possibility)
    (update-possibility-line squares x 'column 'update-possibility)
    (update-possibility-subsquares squares x y 'update-possibility)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(defmethod copy-squares(squares)
  (let ((s (make-squares)))
    ;; copie de tous les carrés de squares et stocke le résultat dans s
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (setf (aref (squares-array s) x y)
		    (copy-square (aref (squares-array squares) x y))
		    )
	      ))
    (setf (to-fill s) (to-fill squares)) ;; copie to-fill de square
    s))


(defmethod update-possibility (squares x y list)
  (setf (possible-digits (aref (squares-array squares) x y))
	(remove-sublist list *digits*)))


(defmethod remove-sublist(list1 list2)
  (if (endp list1)
      list2
      (remove-sublist (cdr list1) (remove (car list1) list2))))


(defmethod update-possibility-line (squares indiceStatic &optional (sens 'line) (comportement 'update-possibility))
  (assert (or (eq comportement 'update-possibility) (eq comportement 'list-digits) (eq comportement 'update-and-list)))
  (assert (or (eq sens 'line) (eq sens 'column)))
  (labels ((line-column (x y)
	     (if (eq sens 'line)
		 (list x y)
		 (list y x))))
    (let ((list '())
	  (array (squares-array squares)))
      (loop for indiceMovible from 0 to (1- *size*) do    
	 ;; pour parcourir les colonnes c'est le x qui est statique 
	 ;; pour parcourir les lignes c'est le y qui est statique
	   (let* ((coor (line-column indiceMovible indiceStatic))
		  (value (digit(aref array (car coor) (cadr coor))))
		  )
	     (if (not (zerop value))
		 (setf list (cons value list)))))
      
      (unless (eq comportement 'list-digits)
	    (loop for indiceMovible from 0 to (1- *size*) do    
	      (let ((coor (line-column indiceMovible indiceStatic)))
	        (update-possibility squares (car coor) (cadr coor) list))))
      (unless (eq comportement 'update-possibility)
	     list))))


(defmethod update-possibility-subsquares (squares x y &optional (comportement 'update-possibility))
  (assert (or (eq comportement 'update-possibility) 
	      (eq comportement 'list-digits)
	      (eq comportement 'update-and-list)))
  (let ((list '())
	(array (squares-array squares))
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ x *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ x *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ y *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ y *sqrt-size*))) *sqrt-size*) 1)) )
    
    (loop for x from departX to finX do
	 (loop for y from departY to finY do
	      (let ((value (digit(aref array x y))))
		(if (not (zerop value))
		    (setf list (cons value list))))))
    (unless (eq comportement 'list-digits)
      (loop for x from departX to finX do
	   (loop for y from departY to finY do
		(update-possibility squares x y list))))
    (unless (eq comportement 'update-possibility)
      list)))


(defmethod update-possibility-all-square (squares)
  ;; met à jour les possible-digits de chaque carré, par rapport aux lignes et aux colonne
  (loop for x from 0 to (1- *size*) do
       (update-possibility-line squares x 'line 'update-possibility)
       (update-possibility-line squares x 'column 'update-possibility)
       )
  ;; met a jour les possible-digits des 9 sous-carrés
  (loop for y from 0 to (1- *sqrt-size*) do
       (loop for x from 0 to (1- *sqrt-size*) do
	    (update-possibility-subsquares squares 
					   (* x *sqrt-size*) 
					   (* y *sqrt-size*) 
					   'update-possibility))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defmethod make-game (grid)
  (make-instance 'game
		 :game-squares (make-squares)
		 :initial-grid (list-to-2d-array
				(read (open grid)))))


			   
(defmethod init-game (game)
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	      (setf (digit (aref (squares-array (game-squares game))
			  x y))
		    (aref (initial-grid game) x y))
	    (unless (zerop (aref (initial-grid game) x y))
	      (setf (to-fill (game-squares game)) (1- (to-fill (game-squares game))))
	      (setf (possible-digits (aref (squares-array (game-squares game)) x y)) '() )
  	      (setf (protected (aref (squares-array (game-squares game)) x y)) T ))))
  (update-possibility-all-square (game-squares game)))

(defmethod change-digit ((squares game) x y value)
  (change-digit (game-squares squares) x y value))

(defmethod get-possibility ((square game) x y)
  (possible-digits (aref (squares-array (game-squares square)) x y)))
(defmethod get-possibility ((square squares) x y)
  (possible-digits (aref (squares-array square) x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod game-over(game)
  (let ((bool '()))
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (let ((square (aref (squares-array (game-squares game))
				  y x)))
		(cond ((and (= (digit square) 0) (eq (possible-digits square) NIL))
		       (setf bool t))
		      
		      ((= (to-fill (game-squares game)) 0)
		       (setf bool t)))
		)))
    bool))

