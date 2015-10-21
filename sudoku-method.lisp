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

;; Affiche le jeu
(defun printgrid (squares)
  ; Affiche le titre
  (printTitle)

  ; Affiche la barre au dessus des lettres
  (printBar "    ┌" "┬" "┐")

  ; Affiche les lettres des colonnes
  (princ "    |")
  (loop for cpt from 0 to (1- *size*) do
       (princ " ")
       (princ (code-char (+ cpt 65)))
       (if (eq (mod (1+ cpt) *sqrt-size*) 0)
	   (princ " |")
	   (princ " ")))
  (terpri)

  ; Affiche la barre en dessous des lettres
  (printBar "┌───┼" "┤" "┤")

  ; Affiche les chiffres des lignes, ainsi que la grille
  (loop for y from 0 to (1- *size*)
     do
       (princ "| ")
       (princ (1+ y))
       (princ " |")
       (loop for x from 0 to (1- *size*)
	  do
	    (princ " ")
	    (princ (digit (aref (squares-array squares) x y)))
	    (if (eq (mod (1+ x) *sqrt-size*) 0)
		(princ " |")
		(princ " "))
	    )
       (terpri)

       (when (and (eq (mod (1+ y) *sqrt-size*) 0)
		(not (eq y (1- *size*))))
	 ; Affiche les barres séparant les zones
	 (printBar "├───┼" "┼" "┤")
	 ))

  ; Affiche la barre de fin
  (printBar "└───┴" "┴" "┘")


  (defparameter line 0)
  (defparameter column 0)
  (askCase 'line 'column)
)

(defun printTitle ()
  (princ "
 ╔═══╗ ╔   ╗ ╔══╗ ╔═══╗ ╔ ╔═ ╔   ╗
 ╚═══╗ ║   ║    ║ ║   ║ ╠═╣  ║   ║
 ╚═══╝ ╚═══╝ ╚══╝ ╚═══╝ ╚ ╚═ ╚═══╝

"))

(defun printBar (debut milieu fin)
  (princ debut)
  (loop for cpt from 0 to (1- *size*) do
       (princ "───")
       (when (and (eq (mod (1+ cpt) *sqrt-size*) 0)
		  (not (eq (1+ cpt) *size*)))
	 (princ milieu)))
  (princ fin)
  (terpri))

(defun askCase(line column)
  (terpri)
  (princ "Enter the column letter: ")
  (setf column(read))
  (loop while (not (or (<= 65 (char-code #'column) 90)
		       (<= 97 (char-code #'column) 122))) do
       (princ "You need to enter a letter. Try Again : ")
       (setf column(read)))

  (princ "Enter the line number: ")
  (setq line(read))
  (loop while (not (numberp line)) do
       (princ "You need to enter a number. Try Again : ")
       (setq line(read)))
)




       


;; ==================
;; ==     Game     ==
;; ==================


