
;;===========================================
;;==           Implémentation              ==
;;==                                       ==
;;===========================================


(in-package :sudoku)

;; ========================
;; ==     Coordonnée     ==
;; ========================
(defmethod make-coor (x y)
  (make-instance 'coor :x x :y y))


  
;; ===================
;; ==     Carre     ==
;; ===================


(defmethod make-square (coor &optional digit)
  (make-instance 'square :coor coor :digit digit))

(defmethod assigned-p (square)
  (if (and (< 0 (digit square)) (< (digit square) 10))
      T
      NIL))

;; ==================
;; ==     Grid     ==
;; ==================

(defmethod make-squares()
  (let (( squares (make-instance 'squares)))
   (grid-to-square squares)
    squares))


(defmethod make-squares-array (size)
  (make-array (list size size)))

(defmethod grid-to-square (squares)
  (loop for x from 0 to (1- *size*)
	do
	   (loop for y from 0 to (1- *size*)
		 do
		    (setf (aref (squares-array squares) x y)
			  (make-square (make-coor x y)  0))))
  )

(defun printCoor (squares x y)
  (print (list
	  (x-coor (coor (aref (squares-array squares) x y )))
	  (y-coor (coor (aref (squares-array squares) x y))))))


(defun printgrid (squares)    
    (loop for y from 0 to (1- *size*)
	  do
	     (let ((list-line '()))
	       (loop for x from 0 to (1- *size*)
		   do
		      (setf list-line
			    (cons
			     (digit (aref (squares-array squares) x y))
			     list-line )))
	       (print (reverse list-line)))
	  ))

;; ==================
;; ==     Game     ==
;; ==================

(defmethod copy-square(square)
  (let (( s (make-square
	     (coor square) (digit square))));; s nouveau square
    (setf (possible-digits s) (possible-digits square)) ;; copie posibilité
    (setf (protected s) (protected square)) ;; copie de la protection
    s))

(defmethod copy-squares(squares)
  (let ((s (make-squares)))
    ;; copie de tout les square de squares et stockage dans s
    (loop for y from 0 to (1- *size*)
	  do
	     (loop for x from 0 to (1- *size*)
		   do
		      (setf (aref (squares-array s) x y)
			    (copy-square (aref (squares-array squares) x y))
			    )
		   ))
    (setf (to-fill s) (to-fill squares)) ;; copie to-fill
    s))


(defmethod init-game (game)
  (setf (game-squares game) (copy-squares (initial-grid game))))



(defun line-colone(x y sens)
;;retourn une liste de coordonné selon le sens choisit
  (assert (or (eq sens 'line) (eq sens 'colonne)))
  (if (eq sens 'line)
      (list x y)
      (list y x)))


;;si sens est à 0 parcours des ligne sinon si sens est a 1 alors pacour colonne
(defun list-digit-line (squares indiceStatic &key (sens 'line) (comportement 'update-possibility))
  (assert (or (eq comportement 'update-possibility) (eq comportement 'list-digits)))
  (assert (or (eq sens 'line) (eq sens 'colonne)))
   (let ((list '())
	 (array (squares-array squares)))
    (loop for indiceMovible from 0 to (1- *size*)
	  do    
             ;; pour parcourire les colonnes c'est le x qui est static 
	     ;; pour parcourire les line c'est le y qui est static 
	     (let* ((coor (line-colone indiceMovible indiceStatic sens))
		    (value (digit(aref array (car coor) (cadr coor))))
		    )
	       (if (not (zerop value))
		   (setf list (cons value list)))))
    
    (if (eq comportement 'list-digits)
        ;; retourne la list des digit present dans la line ou la colonne
	list
        ;; sinon on met à jour les possible-digit de toutes les case de la ligne ou colonne
	(loop for indiceMovible from 0 to (1- *size*)
	  do    
	     (let ((coor (line-colone indiceMovible indiceStatic sens)))
	       (update-possible squares (car coor) (cadr coor) list))))))
     


(defun list-digit-square-interior (squares x y &key (comportement 'update-possibility))
  (assert (or (eq comportement 'update-possibility) (eq comportement 'list-digits)))
  (let ((list '())
	(array (squares-array squares))
	;; definition des debut et fin du petit carré qui contient square[x,y]
	(departX (* (truncate (/ x *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ x *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ y *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ y *sqrt-size*))) *sqrt-size*) 1)) )
    
    (loop for x from departX to finX
       do
	 (loop for y from departY to finY
	    do
	      (let ((value (digit(aref array x y))))
		(if (not (zerop value))
		    (setf list (cons value list))))))
    (if (eq comportement 'list-digits)
	;;si oui alors on retourne la liste des digit du petit carré
	list
        ;; sinon on met à jour les possible-digit de toutes les case du petit carré
        (loop for x from departX to finX
	   do
	     (loop for y from departY to finY
		do
		  (update-possible squares x y list))))))
	

(defun update-possible (squares x y list)
  ;;modifie le possible-digit du carré[x,y] en focntion de la liste
  (setf (possible-digits (aref (squares-array squares) x y))
		       (remove-possibility list (possible-digits (aref (squares-array squares) x y))))
       )

(defun remove-possibility (list1 list2)
;; renvoie list2 priver des éléménts de list1 
  (if (endp list1)
      list2
      (remove-possibility (cdr list1) (remove (car list1) list2))))


;; (defun update-possibility-line (square x)
;;   (let ((list '())
;; 	(array (squares-array squares)))
;;     (loop for x from 0 to (1- *size*)
;; 	  do
;; 	     (let ((digit (digit(aref array x y))))
;; 	       (if (not (zerop digit))
;; 		   (setf list (cons digit list)))))
;;     (loop for x from 0 to (1- *size*)
;; 	  do
;; 	     (setf (possible-digits (aref array x y))
;; 		   (set-exclusive-or (possible-digits (aref array x y)) list)))))


;; (defun update-possibility (squares)
;;   (loop for y from 0 to (1- *size*)
;; 	do
;; 	   (let ((list '())
;; 		 (array (squares-array squares)))
;; 	     (loop for x from 0 to (1- *size*)
;; 		   do
;; 		      (let ((digit (digit(aref array x y))))
;; 			(if (not (zerop digit))
;; 			    (setf list (cons digit list)))))
	     
;; 	      (loop for x from 0 to (1- *size*)
;; 		    do
;; 		       (setf (possible-digits (aref array x y))
;; 			     (set-exclusive-or (possible-digits (aref array x y)) list))))))
		       
			       
