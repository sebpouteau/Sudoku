
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
  (if (zerop sens)
      (list x y)
      (list y x)))

(defun list-digit-line (squares indiceY &key (sens 0))
   (let ((list '())
	 (array (squares-array squares)))
    
    (loop for indiceX from 0 to (1- *size*)
	  do    
	     (let* ((coor (line-colone indiceX indiceY sens))
		    (value (digit(aref array (car coor) (cadr coor))))
		    )
	       (if (eq 1 sens)
		   (setf coor (reverse coor)))
	       (if (not (zerop value))
		   (setf list (cons value list)))))
     list
     ))



     
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
		       
			       
