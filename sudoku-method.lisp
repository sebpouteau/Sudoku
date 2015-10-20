
					;===========================================
					;==           Implémentation              ==
					;==                                       ==
					;===========================================


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


