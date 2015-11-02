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
  (print-grid game)

  ;; Demande la ligne, la colonne et la valeur de la case à modifier
  (ask-case)

  ;; Change la valeur de la case
  (change-digit (game-squares game)
  		(- *column* 65)
  		(1- *line*) 
  		*value-digit*)
  )


;; Affiche la grille du jeu
(defmethod print-grid ((squares game))
  (print-grid (game-squares squares)))


;; Affiche le grille passé en paramètre
(defmethod print-grid ((squares squares))
  ;; Affiche le titre
  (print-title)

  ;; Affiche la barre au dessus des lettres
  (print-bar " " " " "┌" "┬" "┐")

  ;; Affiche les lettres des colonnes
  (print-column)

  ;; Affiche la barre en dessous des lettres
  (print-bar "─" "┌" "┼" "┼" "┤")

  ;; Affiche les chiffres des lignes, ainsi que la grille
  (print-line squares)

  ;; Affiche la barre de fin
  (print-bar "─" "└" "┴" "┴" "┘")

  (terpri)
  )


;; Affiche le titre
(defun print-title ()
  (princ"
 ╔═══╗ ╔   ╗ ╔══╗  ╔═══╗ ╔ ╔ ╔   ╗
 ╚═══╗ ║   ║ ║   ║ ║   ║ ╠╣  ║   ║
 ╚═══╝ ╚═══╝ ╚══╝  ╚═══╝ ╚ ╚ ╚═══╝

"))


;; Affiche une barre en fonction *size* et des paramètres rentrés
(defun print-bar (espacement debut debut2 milieu fin)
  (princ debut)
  (loop for cpt from 1 to *sqrt-size* do
    (princ espacement))
  (princ debut2)
  (loop for cpt from 1 to *size* do
    (princ "───")
    (if (< 9 *size*) 
	(princ "─"))
    (when (and (eq (mod cpt *sqrt-size*) 0)
	       (not (eq cpt *size*)))
      (princ milieu)))
  (princ fin)
  (terpri))


;; Affiche les lettres des colonnes
(defun print-column()
  (princ " ")
  (loop for cpt from 1 to *sqrt-size* do 
    (princ " ")) ;; permet l'extensibilité de la grille
  (princ "| ")
  (loop for cpt from 0 to (1- *size*) do
    (when (< 9 *size*)
	(princ " "))
    (format T "~d " (code-char (+ cpt 65)))
    (if (eq (mod (1+ cpt) *sqrt-size*) 0)
	(princ "| ")
	(princ " ")))
  (terpri)
  )


;; Affiche les chiffres des lignes, ainsi que la grille
(defun print-line (squares)
  (loop for y from 0 to (1- *size*) do
    (if (< 9 *size*) 
	(format T "| ~2d |" (1+ y))
	(format T "| ~d |" (1+ y)))
    (loop for x from 0 to (1- *size*) do
      (if (< 9 *size*) 
	  (format T " ~2d " (digit (aref (squares-array squares) x y)))
	  (format T " ~d " (digit (aref (squares-array squares) x y))))
      (when (eq (mod (1+ x) *sqrt-size*) 0)
	(princ "|")))
    (terpri)
    (when (and (eq (mod (1+ y) *sqrt-size*) 0)
	       (not (eq y (1- *size*))))
      ;; Affiche les barres séparant les zones
      (print-bar "─" "├" "┼" "┼" "┤")
      ))
  )



;; Affiche la demande la ligne, la colonne et la valeur de la case à modifier
(defun ask-case ()
  ;; Demande la ligne
  (ask-line)

  ;; Demande la colonne
  (ask-column)

  ;; Demande la valeur de la case
  (ask-digit)
  )

;; Demande la ligne
(defun ask-line ()
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
  (terpri))


;; Demande la colonne
(defun ask-column ()
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
  (terpri))


;; Demande la valeur de la case
(defun ask-digit ()
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



;; Affiche "Game Over !"
(defun print-game-over ()
  (princ "
 ╔══╗ ╔══╗ ╔╗╔╗ ╔═╗     ╔══╗ ╗  ╔ ╔═╗ ╔═╗   ║
 ║ ═╗ ╠══╣ ║╚╝║ ╠╣      ║  ║ ║  ║ ╠╣  ╠╣    ║
 ╚══╝ ╚  ╝ ╝  ╚ ╚═╝     ╚══╝  ╚╝  ╚═╝ ╚ ╚   ═

"))

;; Affiche "You Win !"
(defun print-win ()
  (princ"
                        ═
 ╗ ╔ ╔══╗ ╗  ╔     ╗  ╔ ╔ ╔╗  ╗   ║
 ╠═╝ ║  ║ ║  ║     ║╔╗║ ║ ║ ╚ ║   ║
 ╚   ╚══╝ ╚══╝     ╚╝╚╝ ╚ ╚  ╚╝   ═

"))
