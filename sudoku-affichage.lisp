;;===========================================
;;==           Implémentation              ==
;;==             AFFICHAGE                 ==
;;===========================================


(in-package :sudoku)

(defvar *line*)
(defvar *column*)
(defvar *value-digit*)

;; Affiche le jeu Sudoku
(defun sudoku(game)
  ;; Affiche la grille
  (printgrid game)
  (terpri)

  ;; Demande la ligne, la colonne et la valeur de la case à modifier
  (askCase)

  ;; Change la valeur de la case
  (change-digit (game-squares game)
  		(- *column* 65)
  		(1- *line*) 
  		*value-digit*)
  (terpri)
  )


;; Affiche la grille du jeu
(defmethod printgrid ((squares game))
  (printgrid (game-squares squares)))


;; Affiche le grille
(defmethod printgrid ((squares squares))
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
  (printBar "└───┴" "┴" "┘"))


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
(defun askCase ()
  ;; Demande la ligne
  (princ "Enter the column letter: ")
  (setf *column* (read-line))
  (loop while (or (not (eq (length *column*) 1)) 
		  (not (or 
			(<= 65 (char-code (char *column* 0)) (+ 65 *size* -1))
			(<= 97 (char-code (char *column* 0)) (+ 97 *size* -1))))) do
       (princ "You need to enter a letter between A and ")
       (princ (code-char (+ 65 *size* -1)))
       (princ " : ")
       (setf *column* (read-line)))
  ;; Transforme la valeur de *column* en valeur numérique (utilisable plus facilement)
  (if (<= 97 (char-code (char *column* 0)) (+ 97 *size* -1))
      (setf *column* (- (char-code (char (string *column*) 0)) 32))
      (setf *column* (char-code (char (string *column*) 0))))
  ;; Affiche le résultat
  (princ "  column : ")
  (princ (code-char *column*))
  (terpri)
  (terpri)


  ;; Demande la colonne
  (princ "Enter the line number: ")
  (setf *line* (read-line))
  (loop while (or (eq (length *line*) 0)
		  (not (numberp (parse-integer (string *line*) :junk-allowed t)))
		  (not (<= 1 (parse-integer (string *line*) :junk-allowed t) *size*))) do
       (princ "You need to enter a number between 1 and ")
       (princ *size*)
       (princ " : ")
       (setf *line* (read-line)))
  ;; Transforme la valeur de *line* en valeur numérique (utilisable plus facilement)
  (setf *line* (parse-integer *line*))
  ;; Affiche le résultat
  (princ "  line : ")
  (princ *line*)
  (terpri)
  (terpri)


  ;; Demande la valeur de la case
  (princ "Enter the digit: ")
  (setf *value-digit* (read-line))
  (loop while (or (eq (length *value-digit*) 0)
		  (not (numberp (parse-integer (string *value-digit*) :junk-allowed t)))
		  (not (<= 1 (parse-integer (string *value-digit*) :junk-allowed t) *size*))) do
       (princ "You need to enter a number between 1 and ")
       (princ *size*)
       (princ " : ")
       (setf *value-digit* (read-line)))
  ;; Transforme la valeur de *line* en valeur numérique (utilisable plus facilement)
  (setf *value-digit* (parse-integer *value-digit*))
  ;; Affiche le résultat
  (princ "  value-digit : ")
  (princ *value-digit*)
  (terpri)
)

