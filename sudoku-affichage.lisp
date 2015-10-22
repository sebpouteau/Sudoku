;;===========================================
;;==           Implémentation              ==
;;==             AFFICHAGE                 ==
;;===========================================


(in-package :sudoku)

(defvar *line*)
(defvar *column*)


;; Affiche le jeu
(defun printgrid (squares)
  ;; Affiche le titre
  (printTitle)

  ;; Affiche la barre au dessus des lettres
  (printBar "    ┌" "┬" "┐")

  ;; Affiche les lettres des colonnes
  (princ "    |")
  (loop for cpt from 0 to (1- *size*) do
       (princ " ")
       (princ (code-char (+ cpt 65)))
       (if (eq (mod (1+ cpt) *sqrt-size*) 0)
	   (princ " |")
	   (princ " ")))
  (terpri)

  ;; Affiche la barre en dessous des lettres
  (printBar "┌───┼" "┤" "┤")

  ;; Affiche les chiffres des lignes, ainsi que la grille
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
	 ;; Affiche les barres séparant les zones
	 (printBar "├───┼" "┼" "┤")
	 ))

  ;; Affiche la barre de fin
  (printBar "└───┴" "┴" "┘")

  ;; Demande la ligne, la colonne et la valeur de la case à modifier
  (askCase '*line* '*column*)
)


;; Affiche le titre
(defun printTitle ()
  (princ "
 ╔═══╗ ╔   ╗ ╔══╗ ╔═══╗ ╔ ╔═ ╔   ╗
 ╚═══╗ ║   ║    ║ ║   ║ ╠═╣  ║   ║
 ╚═══╝ ╚═══╝ ╚══╝ ╚═══╝ ╚ ╚═ ╚═══╝

"))


;; Affiche une barre en fonction *size* et des paramètres rentrés
(defun printBar (debut milieu fin)
  (princ debut)
  (loop for cpt from 1 to *size* do
       (princ "───")
       (when (and (eq (mod cpt *sqrt-size*) 0)
		  (not (eq cpt *size*)))
	 (princ milieu)))
  (princ fin)
  (terpri))


;; Affiche la demande la ligne, la colonne et la valeur de la case à modifier
(defun askCase(line column)
  (terpri)
  (princ "Enter the column letter: ")
  (setf column(read))
  (loop while (not (or (<= 65 (char-code column) 90)
		       (<= 97 (char-code column) 122))) do
       (princ "You need to enter a letter. Try Again : ")
       (setf column(read)))

  (princ "Enter the line number: ")
  (setq line(read))
  (loop while (not (numberp line)) do
       (princ "You need to enter a number. Try Again : ")
       (setq line(read)))

  (princ "Enter the digit: ")
  (setq line(read))
  (loop while (not (numberp line)) do
       (princ "You need to enter a number. Try Again : ")
       (setq line(read)))
)
