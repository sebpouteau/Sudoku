
;;===========================================
;;==           Implémentation              ==
;;==             AFFICHAGE                 ==
;;===========================================


(in-package :sudoku)


;; Définition des variables locales
(defvar *line*)
(defvar *column*)
(defvar *value-digit*)
(defvar *code-ascii* 65)


(defun sudoku (game)
  "Lance le jeu passé en paramètre"

  (loop while (not (game-over game)) do
       (launcher-sudoku game))
  (print-grid *game*)
  (game-over game :print T)
  )


(defun launcher-sudoku (game)
  "Affiche le jeu Sudoku"

  ;; Affiche la grille
  (print-grid game)

  ;; Demande la ligne, la colonne et la valeur de la case à modifier
  (ask-case)

  ;; Change la valeur de la case
  (let* ((squares (game-squares game))
	 (x 1- *line*)
	 (y 1 *column* 65))
    (if (protected (aref (squares-array squares) x y))
	(print "Impossible de changer cette case : valeur initial")
	(change-digit squares x y *value-digit*)))
  )


(defmethod print-grid ((game game))
  "Affiche la grille du jeu passé en paramètre"

  (print-grid (game-squares game))
  )


(defmethod print-grid ((squares squares))
  "Affiche la grille passée en paramètre"

  ;; Affiche le titre
  (print-title)

  ;; Affiche la barre au dessus des lettres
  (print-bar " " " " "┌" "┬" "┐")

  ;; Affiche les lettres des colonnes
  (print-column)

  ;; Affiche la barre en-dessous des lettres
  (print-bar "─" "┌" "┼" "┼" "┤")

  ;; Affiche les chiffres des lignes, ainsi que la grille
  (print-line squares)

  ;; Affiche la barre de fin
  (print-bar "─" "└" "┴" "┴" "┘")
  )


(defun print-bar (espacement debut debut2 milieu fin)
  "Affiche une barre en fonction de *size* et des paramètres rentrés"

  (princ debut)
  (loop for cpt from 0 to 2 do
       (princ espacement))
  (when (< 9 *size*)
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
  (terpri)
  )


(defun print-column()
  "Affiche les lettres des colonnes"

  (loop for cpt from 0 to 3 do
       (princ " "))
  (when (< 9 *size*)
    (princ " "))
  (princ "| ")
  (loop for cpt from 0 to (1- *size*) do
       (when (< 9 *size*)
	 (princ " "))
       (format T "~d " (code-char (+ cpt *code-ascii*)))
       (if (eq (mod (1+ cpt) *sqrt-size*) 0)
	   (princ "| ")
	   (princ " ")))
  (terpri)
  )


(defun print-line (squares)
  "Affiche les chiffres des lignes, ainsi que la grille"

  (loop for x from 0 to (1- *size*) do
       (if (< 9 *size*) 
	   (format T "| ~2d |" (1+ x))
	   (format T "| ~d |" (1+ x)))
       (loop for y from 0 to (1- *size*) do
	    (let ((digit (digit (aref (squares-array squares) x y))))
	      (when (= digit 0)
		(if (< 9 *size*)
		    (setf digit "  ")
		    (setf digit " ")))
	      (if (< 9 *size*)
		  (format T " ~2d " digit)
		  (format T " ~d " digit)))
	    (when (eq (mod (1+ y) *sqrt-size*) 0)
	      (princ "|")))
       (terpri)
       (when (and (eq (mod (1+ x) *sqrt-size*) 0)
		  (not (eq x (1- *size*))))
	 ;; Affiche les barres séparant les différentes zones
	 (print-bar "─" "├" "┼" "┼" "┤")
	 ))
  )


(defun ask-case ()
  "Demande la ligne, la colonne et la valeur de la case à modifier"

  ;; Demande la colonne
  (ask-column)

  ;; Demande la ligne
  (ask-line)

  ;; Demande la valeur de la case
  (ask-digit)
  )



(defun ask-column ()
  "Demande la colonne à l'utilisateur"

  (princ "Enter the column letter: ")
  (force-output) ;; force la sortie (utile pour jouer dans le terminal)
  (setf *column* (read-line))
  (loop while (or (not (eq (length *column*) 1)) 
		  (not (<= 65 (char-code (char *column* 0)) (+ 65 *size* -1)))) do
       (format T "You need to enter a letter between A and ~d : " (code-char (+ *code-ascii* *size* -1)))
       (force-output)
       (setf *column* (read-line)))
  ;; Transforme la valeur de *column* en valeur numérique (utilisable plus facilement)
  (setf *column* (char-code (char (string *column*) 0)))
  ;; Affiche le résultat
  (format T "  column : ~d ~%~%" (code-char *column*))
  )


(defun ask-line ()
  "Demande la ligne à l'utilisateur"

  (princ "Enter the line number : ")
  (force-output) ;; force la sortie (utile pour jouer dans le terminal)
  (setf *line* (read-line))
  (loop while (or (eq (length *line*) 0)
		  (not (numberp (parse-integer (string *line*) :junk-allowed t)))
		  (not (<= 1 (parse-integer (string *line*) :junk-allowed t) *size*))) do
       (format T "You need to enter a number between 1 and ~d : " *size*)
       (force-output)
       (setf *line* (read-line)))
  ;; Transforme la valeur de *line* en valeur numérique (utilisable plus facilement)
  (setf *line* (parse-integer *line*))
  ;; Affiche le résultat
  (format T "  line : ~d ~%~%" *line*)
  )


(defun ask-digit ()
  "Demande la valeur de la case à l'utilisateur"

  (princ "Enter the digit: ")
  (force-output) ;; force la sortie (utile pour jouer dans le terminal)
  (setf *value-digit* (read-line))
  (loop while (or (eq (length *value-digit*) 0)
		  (not (numberp (parse-integer (string *value-digit*) :junk-allowed t)))
		  (not (<= 1 (parse-integer (string *value-digit*) :junk-allowed t) *size*))) do
       (format T "You need to enter a number between 1 and ~d : " *size*)
       (force-output)
       (setf *value-digit* (read-line)))
  ;; Transforme la valeur de *value-digit* en valeur numérique (utilisable plus facilement)
  (setf *value-digit* (parse-integer *value-digit*))
  ;; Affiche le résultat
  (format T "  value-digit : ~d ~%~%" *value-digit*)
  )


(defun print-title ()
  "Affiche le titre"

  (princ"
 ╔═══╗ ╔   ╗ ╔══╗  ╔═══╗ ╔ ╔ ╔   ╗
 ╚═══╗ ║   ║ ║   ║ ║   ║ ╠╣  ║   ║
 ╚═══╝ ╚═══╝ ╚══╝  ╚═══╝ ╚ ╚ ╚═══╝

"))


(defun print-game-over ()
  "Affiche 'Game Over !' "

  (princ "
 ╔══╗ ╔══╗ ╔╗╔╗ ╔═╗     ╔══╗ ╗  ╔ ╔═╗ ╔═╗   ║
 ║ ═╗ ╠══╣ ║╚╝║ ╠╣      ║  ║ ║  ║ ╠╣  ╠╣    ║
 ╚══╝ ╚  ╝ ╝  ╚ ╚═╝     ╚══╝  ╚╝  ╚═╝ ╚ ╚   ═

"))

 
(defun print-win ()
  "Affiche 'You Win !' "
  
  (princ 
"                         ═
 ╗  ╔ ╔══╗ ╗  ╔     ╗  ╔ ╔ ╔╗  ╗   ║
 ╚══╣ ║  ║ ║  ║     ║╔╗║ ║ ║ ╚ ║   ║
  ══╝ ╚══╝ ╚══╝     ╚╝╚╝ ╚ ╚  ╚╝   ═

"))
