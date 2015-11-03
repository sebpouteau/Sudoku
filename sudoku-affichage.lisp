
;;===========================================
;;==           Implémentation              ==
;;==             AFFICHAGE                 ==
;;===========================================


(in-package :sudoku)

(defvar *line*)
(defvar *column*)
(defvar *value-digit*)
(defvar *code-ascii* 65)

(defun sudoku (game)
  (loop while (not (game-over game)) do 
       (game-do game))
  (print-grid *game*)
  (game-over game :print T )
  )

;; Affiche le jeu Sudoku
(defun game-do (game)
  ;; Affiche la grille
  (print-grid game)

  ;; Demande la ligne, la colonne et la valeur de la case à modifier
  (ask-case)

  ;; Change la valeur de la case
  (change-digit (game-squares game)
  		(1- *line*)
  		(- *column* 65) 
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
  (terpri))


;; Affiche les lettres des colonnes
(defun print-column()
  (loop for cpt from 0 to 3 do
       (princ " "))
  (when (< 9 *size*)
    (princ " ")) ;; permet l'extensibilité de la grille
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


;; Affiche les chiffres des lignes, ainsi que la grille
(defun print-line (squares)
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

;; Affiche la demande la ligne, la colonne et la valeur de la case à modifier
(defun ask-case ()
  ;; Demande la colonne
  (ask-column)

  ;; Demande la ligne
  (ask-line)

  ;; Demande la valeur de la case
  (ask-digit)
  )

;; Demande la colonne à l'utilisateur
(defun ask-column ()
  (princ "Enter the column letter: ")
  (setf *column* (read-line))
  (loop while (or (not (eq (length *column*) 1)) 
		  (not (<= 65 (char-code (char *column* 0)) (+ 65 *size* -1)))) do
       (format T "You need to enter a letter between A and ~d : " (code-char (+ *code-ascii* *size* -1)))
       (setf *column* (read-line)))
  ;; Transforme la valeur de *column* en valeur numérique (utilisable plus facilement)
  (setf *column* (char-code (char (string *column*) 0)))
  ;; Affiche le résultat
  (format T "  column : ~d ~%~%" (code-char *column*))
  )


;; Demande la ligne à l'utilisateur
(defun ask-line ()
  (princ "Enter the line number : ")
  (setf *line* (read-line))
  (loop while (or (eq (length *line*) 0)
		  (not (numberp (parse-integer (string *line*) :junk-allowed t)))
		  (not (<= 1 (parse-integer (string *line*) :junk-allowed t) *size*))) do
       (format T "You need to enter a number between 1 and ~d : " *size*)
       (setf *line* (read-line)))
  ;; Transforme la valeur de *line* en valeur numérique (utilisable plus facilement)
  (setf *line* (parse-integer *line*))
  ;; Affiche le résultat
  (format T "  line : ~d ~%~%" *line*)
  )


;; Demande la valeur de la case à l'utilisateur
(defun ask-digit ()
  (princ "Enter the digit: ")
  (setf *value-digit* (read-line))
  (loop while (or (eq (length *value-digit*) 0)
		  (not (numberp (parse-integer (string *value-digit*) :junk-allowed t)))
		  (not (<= 1 (parse-integer (string *value-digit*) :junk-allowed t) *size*))) do
       (format T "You need to enter a number between 1 and ~d : " *size*)
       (setf *value-digit* (read-line)))
  ;; Transforme la valeur de *value-digit* en valeur numérique (utilisable plus facilement)
  (setf *value-digit* (parse-integer *value-digit*))
  ;; Affiche le résultat
  (format T "  value-digit : ~d ~%~%" *value-digit*)
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
  (princ 
"                        ═
 ╗  ╔ ╔══╗ ╗  ╔     ╗  ╔ ╔ ╔╗  ╗   ║
 ╚══╣ ║  ║ ║  ║     ║╔╗║ ║ ║ ╚ ║   ║
  ══╝ ╚══╝ ╚══╝     ╚╝╚╝ ╚ ╚  ╚╝   ═

"))
