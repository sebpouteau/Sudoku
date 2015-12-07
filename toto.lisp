
;;===========================================
;;==           Implémentation              ==
;;==     FONCTIONS STATIQUES SUDOKU        ==
;;===========================================



(defun list-to-2d-array (list)
  "Créer un tableau 2D à partir d'une liste"
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun line-column (sens x y)
  "Renvoie la liste '(x y) ou '(y x) selon si l'on traite les colonnes ou les lignes"
  (if (eq sens 'line)
      (list y x)
      (list x y)))

(defun init-sudoku ()
  "Méthode utilisée pour l'interface web"
  )

;;=============================================
;;==             Interface                   ==
;;==     FONCTIONS SUDOKU, AFFICHAGE, IA     ==
;;=============================================



;; ---------------------
;; --     Données     --
;; ---------------------

(defun create-list-possibility (size list)
  "créer la liste des possibilités"
  (if (= size 0)
      list
      (create-list-possibility (1- size) (cons size list))))

(defvar *sqrt-size* 3 "Side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "The current instance of a game")
(defvar *digits* (create-list-possibility *size* '()))


;; ---------------------
;; --   Coordonnées   --
;; ---------------------

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "Class for coordinates in the squares grid"))

(defgeneric make-coor (x y)
  (:documentation "Instance of a coor [x,y]"))


;; ---------------------
;; --      Carré      --
;; ---------------------

(defclass square ()
  ((coor :initarg :coor :accessor coor) 
   (possible-digits :initform (copy-list *digits*)
		    :initarg :possible-digits :accessor possible-digits)
   (digit :initarg :digit :accessor digit :initform 0)
   (protected :initform nil :initarg :protected :accessor protected))
  (:documentation "One square of the squares
         coor -> coordonnée du carré dans la grille
         possible-digits -> valeurs (nombres) possibles du carré
         digit -> valeur (nombre) affiché du carré
         protected -> vaut true si le nombre était au début => non-modifiable"))

(defgeneric make-square (coor &optional digit)
  (:documentation "Creates a square containing coor and digit"))

(defgeneric assigned-p (square)
  (:documentation "T if the digit slot a striclty positive and inferior to 10"))

(defgeneric copy-square(square)
  (:documentation "Copie un square et retourne la copie"))

(defgeneric coor-square (squares coor)
  (:documentation "Retourne le square aux coordonnées coor"))


;; ----------------------
;; --      Grille      --
;; ----------------------

(defclass squares ()
  ((squares-array :initarg :squares-array :reader squares-array :type array
		  :initform (make-squares-array *size*))
   (to-fill :initform *nb-squares* :accessor to-fill))
  (:documentation "Class containing a two dimensional array of instances of the square class \
                   to-fill -> nombre de cases vides (= à remplir)"))

(defgeneric make-squares()
  (:documentation "Return array two dimensional (size,  size)"))

(defgeneric make-squares-array (size)
  (:documentation "Return array two dimensional (size,  size)"))

(defgeneric grid-to-square (squares)
  (:documentation "Attribue the right coordinates to square-array"))

(defgeneric change-digit (squares x y value)
  (:documentation "Change la valeur digit du carré en (x,y)
                  et met à jour les possibilités de la ligne, colonne et du sous-carré")) 

(defgeneric copy-squares(squares)
  (:documentation "Copie le squares passé en paramètre et retourne la copie"))

(defgeneric update-possibility (squares x y list)
  (:documentation "Met à jour possible-digits du carré en (x,y) par rapport à list.
                   Il supprime les éléments de possible-digits présents dans list."))

(defgeneric remove-sublist (list1 list2)
  (:documentation "Supprime les éléments de list2 présents dans list1 et renvoie list2"))

(defgeneric update-possibility-line (squares indiceStatic sens)
  (:documentation "Met à jours les possible-digits de tous les carrés de squares selon le paramètre sens :
                         sens -> 'line ou 'column permet de parcourir les lignes ou les colonnes"))

(defgeneric update-possibility-subsquares (squares x y)
  (:documentation "Met à jour les possible-digits des carrés du sous-carrés"))

(defgeneric update-possibility-all-square (squares)
  (:documentation "Met à jour les possible-digits de tous les carrés de la grille passée en paramètre"))

(defgeneric update-after-change-digit(game x y)
  (:documentation "Met à jour les cases affecté par le changement de valeur en (x,y)"))

;; ----------------------
;; --       Game       --
;; ----------------------

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :accessor initial-grid :initarg :initial-grid))
  (:documentation "Class for game instances"))

(defgeneric make-game (grid)
  (:documentation "Créer une instance de game avec le lien d'une grille passé en paramètre"))

(defgeneric game-over (game)
  (:documentation "If the game is over (either won or lost"))

(defgeneric init-game (game)
  (:documentation "Initializes GAME with its initial-grid"))

(defgeneric game-with-new-grid (&optional strategy)
  (:documentation "Instance of game with a grid and STRATEGY"))

(defgeneric game-do (game square)
  (:documentation "Plays coor/digit of square in the coor-square in squares of GAME"))

(defgeneric get-possibility (square x y)
  (:documentation "Retourne la liste des possibilitées du carré (x,y)"))


;; ---------------------
;; --    Affichage    --
;; ---------------------

(defgeneric main ()
  (:documentation "Jeu du sudoku complet (demande la grille, et lance le jeu)"))

(defgeneric sudoku (game)
  (:documentation "Lance le jeu passé en paramètre"))

(defgeneric launcher-sudoku (game)
  (:documentation "Affiche le jeu Sudoku"))

(defgeneric print-grid (game)
  (:documentation "Affiche la grille du jeu passé en paramètre"))

(defgeneric print-grid (squares)
  (:documentation "Affiche la grille passée en paramètre"))

(defgeneric print-bar (espacement debut debut2 milieu fin)
  (:documentation "Affiche une barre en fonction de *size* et des paramètres rentrés"))

(defgeneric print-column ()
  (:documentation "Affiche les lettres des colonnes"))

(defgeneric print-line (game)
  (:documentation "Affiche les chiffres des lignes, ainsi que la grille du jeu passé en paramètre"))

(defgeneric print-line (squares)
  (:documentation "Affiche les chiffres des lignes, ainsi que la grille"))

(defgeneric print-title ()
  (:documentation "Affiche le titre"))

(defgeneric print-game-over ()
  (:documentation "Affiche 'Game Over !' "))
 
(defgeneric print-win ()
  (:documentation "Affiche 'You Win !' "))

(defgeneric print-end (game)
  (:documentation "Affiche le message de 'win' ou de 'game over', selon la game passée en paramètre"))

(defgeneric ask-case ()
  (:documentation "Demande la ligne, la colonne et la valeur de la case à modifier"))

(defgeneric ask-column ()
  (:documentation "Demande la colonne à l'utilisateur"))

(defgeneric ask-line ()
  (:documentation "Demande la ligne à l'utilisateur"))

(defgeneric ask-digit ()
  (:documentation "Demande la valeur de la case à l'utilisateur"))


;; ---------------------
;; --    Stratégie    --
;; ---------------------

(defgeneric init-standalone (tab)
  (:documentation "Créer toute la structure pour exécuter la stratégie. Il prend un tableau 2D en paramètre"))

(defgeneric main-standalone ()
  (:documentation "Renvoie un coup à jouer [x,y] value"))

;;===========================================
;;==           Implémentation              ==
;;==          FONCTIONS SUDOKU             ==
;;===========================================



;; ---------------------
;; --   Coordonnées   --
;; ---------------------

(defmethod make-coor (x y)
  (make-instance 'coor :x x :y y)
  )


;; ---------------------
;; --      Carré      --
;; ---------------------

(defmethod make-square (coor &optional digit)
  (make-instance 'square :coor coor :digit digit)
  )


(defmethod assigned-p (square)
  (and (< 0 (digit square)) (<= (digit square) *size*))
  )


(defmethod copy-square(square)
  (let ((s (make-square (coor square) (digit square))))
    (setf (possible-digits s) (possible-digits square))
    (setf (protected s) (protected square))
    s)
  )


(defmethod coor-square (squares coor)
  (aref (squares-array squares) (x-coor coor) (y-coor coor))
  )


;; ----------------------
;; --      Grille      --
;; ----------------------

(defmethod make-squares()
  (let (( squares (make-instance 'squares)))
    (grid-to-square squares)
    squares)
  )


(defmethod make-squares-array (size)
  (make-array (list size size))
  )


(defmethod grid-to-square (squares)
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (setf (aref (squares-array squares) x y)
		  (make-square (make-coor x y) 0))))
  )


(defmethod change-digit ((squares squares) x y value)
  (if (= (digit (aref (squares-array squares) x y)) 0)
      (progn
	(setf (digit (aref (squares-array squares) x y)) value)
	(setf (to-fill squares) (1- (to-fill squares))))
      (progn
	(setf (digit (aref (squares-array squares) x y)) value)
	(update-possibility-all-square squares)))
  )


(defmethod copy-squares(squares)
  (let ((s (make-squares)))
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (setf (aref (squares-array s) x y)
		    (copy-square (aref (squares-array squares) x y)))))
    (setf (to-fill s) (to-fill squares))
    s)
  )


(defmethod update-possibility (squares x y list)
  (if (eq (digit (aref (squares-array squares) x y)) 0) 
      (setf (possible-digits (aref (squares-array squares) x y))
	    (remove-sublist list (get-possibility squares x y)))
      (setf (possible-digits (aref (squares-array squares) x y))
	    NIL))
  )

(defmethod remove-sublist(list1 list2)
  (set-difference list2 list1))


(defmethod update-possibility-line (squares indiceStatic sens)
  (assert (or (eq sens 'line) (eq sens 'column)))
  (let ((list '())
	(array (squares-array squares)))
    (loop for indiceMovible from 0 to (1- *size*) do    
       ;; Pour parcourir les colonnes c'est le x qui est statique 
       ;; Pour parcourir les lignes c'est le y qui est statique
	 (let* ((coor (line-column sens indiceMovible indiceStatic))
		(value (digit(aref array (car coor) (cadr coor)))))
	   (if (not (zerop value))
	       (setf list (cons value list)))))
    (loop for indiceMovible from 0 to (1- *size*) do    
	 (let ((coor (line-column sens indiceMovible indiceStatic)))
	   (update-possibility squares (car coor) (cadr coor) list))))
  )

 
(defmethod update-possibility-subsquares (squares x y)
  (let ((list '())
	(array (squares-array squares))
	;; Définition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ x *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ x *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ y *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ y *sqrt-size*))) *sqrt-size*) 1)))
    
    (loop for x from departX to finX do
	 (loop for y from departY to finY do
	      (let ((value (digit(aref array x y))))
		(if (not (zerop value))
		    (setf list (cons value list))))))
    (loop for x from departX to finX do
	 (loop for y from departY to finY do
	      (update-possibility squares x y list))))
  )


(defmethod update-possibility-all-square (squares)
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (setf (possible-digits (aref (squares-array squares) x y))
		  *digits*)))
  ;; Met à jour les possible-digits de chaque carré, par rapport aux lignes et aux colonnes
  (loop for x from 0 to (1- *size*) do
       (update-possibility-line squares x 'line )
       (update-possibility-line squares x 'column ))
  ;; Met à jour les possible-digits des 9 sous-carrés
  (loop for y from 0 to (1- *sqrt-size*) do
       (loop for x from 0 to (1- *sqrt-size*) do
	    (update-possibility-subsquares squares 
					   (* x *sqrt-size*) 
					   (* y *sqrt-size*))))
  )


(defmethod update-after-change-digit(game x y)
  (update-possibility-subsquares (game-squares game) x y)
  (update-possibility-line (game-squares game) x 'line)
  (update-possibility-line (game-squares game) y 'column)
  )

(defmethod make-game (grid)
  (let ((game (make-instance 'game
			     :game-squares (make-squares)
			     :initial-grid (list-to-2d-array
					    (read (open grid))))))
    (init-game game)
    game)
  )


(defmethod init-game (game)
  (setf (to-fill (game-squares game)) (* *size* *size*))
  (loop for x from 0 to (1- *size*) do
       (loop for y from 0 to (1- *size*) do
	    (setf (digit (aref (squares-array (game-squares game))
			       x y))
		  (aref (initial-grid game) x y))
	    (unless (zerop (aref (initial-grid game) x y))
	      (setf (to-fill (game-squares game)) (1- (to-fill (game-squares game))))
	      (setf (possible-digits (aref (squares-array (game-squares game)) x y)) '() )
  	      (setf (protected (aref (squares-array (game-squares game)) x y)) T ))))
  (update-possibility-all-square (game-squares game))
  )


(defmethod change-digit ((squares game) x y value)
  (change-digit (game-squares squares) x y value)
  )


(defmethod game-do (game square)
  (change-digit game 
		(x-coor (coor square)) 
		(y-coor (coor  square)) 
		(digit square))
  (update-after-change-digit game
			     (x-coor (coor square)) 
			     (y-coor (coor  square)))
  )


(defmethod get-possibility ((square squares) x y)
  (possible-digits (aref (squares-array square) x y))
  )

(defmethod get-possibility ((square game) x y)
  (get-possibility (game-squares square) x y)
  )


(defmethod game-over(game)
  (loop for x from 0 to (1- *size*) do
       (loop for y from 0 to (1- *size*) do
	    (let ((square (aref (squares-array (game-squares game)) x y)))
	      (cond ((and (= (digit square) 0)
			  (eq (possible-digits square) NIL))
		     (return-from game-over 'lose))
		    ((= (to-fill (game-squares game)) 0)
		     (return-from game-over 'win))
		    ))))
  )


(defmethod game-with-new-grid(&optional strategy)
  (declare (ignore strategy))
  (let ((game (make-game "Grids/1.sudoku")))
    (init-game game)
    game)
  )

;;========================================
;;==           Implémentation           ==
;;==         FONCTIONS AFFICHAGE        ==
;;========================================




;; ---------------------
;; --     Données     --
;; ---------------------

(defvar *line*)
(defvar *column*)
(defvar *value-digit*)
(defvar *code-ascii* 65)

;; -------------------------------
;; --   Méthodes de lancement   --
;; -------------------------------

(defmethod main ()
  (terpri)
  (princ "Saisir la grille de jeu (Exemple : Grids/1.sudoku) : ")
  (force-output)
  (handler-case
      (let* ((grille (read-line))
	     (game (make-game grille)))
	(init-game game)
	(sudoku game))
    (T (c)
      (format T "~%~a~%" c)
      (sb-ext:exit)))
  )


(defmethod sudoku (game)
  (loop while (not (game-over game)) do
       (launcher-sudoku game))
  (print-grid game)
  (print-end (game-over game))
  )


(defmethod launcher-sudoku (game)
  ;; Affiche la grille
  (print-grid game)

  ;; Demande la ligne, la colonne et la valeur de la case à modifier
  (ask-case)

  ;; Change la valeur de la case
  (let* ((squares (game-squares game))
	 (x (1- *line*))
	 (y (- *column* 65)))
    (if (protected (aref (squares-array squares) x y))
	(progn
	  (princ "Impossible de changer cette case : valeur initial")
	  (terpri)
	  (terpri))
	(change-digit squares x y *value-digit*)))
  )


;; ------------------------------
;; --   Méthodes d'affichage   --
;; ------------------------------

(defmethod print-grid ((game game))
  (print-grid (game-squares game))
  )


(defmethod print-grid ((squares squares))
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


(defmethod print-bar (espacement debut debut2 milieu fin)
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


(defmethod print-column()
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


(defmethod print-line ((game game))
  (print-line (game-squares game))
  )


(defmethod print-line ((squares squares))
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


(defmethod print-title ()
  (princ"
 ╔═══╗ ╔   ╗ ╔══╗  ╔═══╗ ╔ ╔ ╔   ╗
 ╚═══╗ ║   ║ ║   ║ ║   ║ ╠╣  ║   ║
 ╚═══╝ ╚═══╝ ╚══╝  ╚═══╝ ╚ ╚ ╚═══╝

"))


(defmethod print-game-over ()
  (princ "
 ╔══╗ ╔══╗ ╔╗╔╗ ╔═╗     ╔══╗ ╗  ╔ ╔═╗ ╔═╗   ║
 ║ ═╗ ╠══╣ ║╚╝║ ╠╣      ║  ║ ║  ║ ╠╣  ╠╣    ║
 ╚══╝ ╚  ╝ ╝  ╚ ╚═╝     ╚══╝  ╚╝  ╚═╝ ╚ ╚   ═

"))


(defmethod print-win ()
  (princ 
   "                         ═
 ╗  ╔ ╔══╗ ╗  ╔     ╗  ╔ ╔ ╔╗  ╗   ║
 ╚══╣ ║  ║ ║  ║     ║╔╗║ ║ ║ ╚ ║   ║
  ══╝ ╚══╝ ╚══╝     ╚╝╚╝ ╚ ╚  ╚╝   ═

"))


(defmethod print-end (end)
  (if (eq end 'win)
      (print-win)
      (print-game-over))
  )


;; ---------------------------------------------------
;; --   Méthodes d'intéraction avec l'utilisateur   --
;; ---------------------------------------------------

(defmethod ask-case ()
  ;; Demande la colonne
  (ask-column)

  ;; Demande la ligne
  (ask-line)

  ;; Demande la valeur de la case
  (ask-digit)
  )


(defmethod ask-column ()
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


(defmethod ask-line ()
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


(defmethod ask-digit ()
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

;;=========================================
;;==           Implémentation            ==
;;==      FONCTIONS IA INTELLIGENTE      ==
;;=========================================



;; -----------------------------
;; --   Fonctions statiques   --
;; -----------------------------

(defun strategy (game)
  ;; On essaye toutes les fonctions pour trouver un coup à jouer
  (substrategy game '(one-possibility-square
		      one-possibility-in-line
		      one-possibility-in-subsquare
		      depth-study-possibility))
  )

(defun substrategy (game list)
  "lance la stratégie avec les fonctions de la liste"
  (loop for fun in list do
       (multiple-value-bind (x y value)  (parcour-grid-with-function game fun)
	 (when x
	   (return-from substrategy (values x y value)))))
  )

(defun verification-square-valid (game x y)
  "Retourne si on peut jouer dans le square x y"
  (and (not (protected ( aref (squares-array (game-squares game)) x y)))
       (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
)


(defun parcour-grid-with-function (game fun)
  " permet de parcourir la grille et d'appliquer une fonction à chaque square valide"
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (when (verification-square-valid game x y)
	      (multiple-value-bind (x y value) (funcall fun game x y)
		(when x
		  (return-from parcour-grid-with-function (values x y value)))))))
  )


(defun one-possibility-square (game x y)
  "Retourne x y value, d'un square n'ayant qu'une possibilité"
  (when (= 1 (length (get-possibility game x y)))
    (return-from one-possibility-square (values x y (car (get-possibility game x y)))))
  )


(defun one-possibility-in-subsquare (game x y)
  "Retourne x y value, s'il y a une possibilité unique dans un sous-square.
Par exemple si une value n'apparait que dans les possibilités d'un seul square du sous squares"
  (let ((list (verifier-subsquare game x y )))
    (when (= (length list) 1)
      (return-from one-possibility-in-subsquare (values x y (car list)))))
  )


(defun verifier-subsquare(game coorX coorY)
  "Vérifie unicité d'un nombre dans un sous squares"
  (let* ((list (get-possibility game coorX coorY))
	 ;; définition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ coorX *sqrt-size*)) *sqrt-size*))
	(finX (- (+ departX *sqrt-size*) 1)) 
	(departY (* (truncate (/ coorY *sqrt-size*)) *sqrt-size*))
	(finY (- (+ departY *sqrt-size*) 1)))
    (loop for x from departX to finX do
      (loop for y from departY to finY do
	;; différent de la coordonné de départ
	(unless (and (eq x coorX)
		     (eq y coorY))
	  (setf list (remove-sublist (get-possibility game x y) list)))))
    (unless (endp list)
      list))
  )


(defun one-possibility-in-line (game x y)
  "Retourne x y value, s'il y a une possibilité unique dans la line/column.
Par exemple si une value n'apparait que dans les possibilités d'un seul square de la line/column"
  (let ((list (remove-duplicates (append (verifier-line game x y 'line)
					 (verifier-line game x y 'column)))))
    (when (= (length list) 1) 
      (return-from one-possibility-in-line (values x y (car list)))))
  )


(defun verifier-line ( game coorX coorY &optional (sens 'line))
  "Vérifie l'unicité d'un nombre dans une line/column. 
L'optional sens permet de choisir entre line et colonne (line / column)"
  (assert (or (eq sens 'line) (eq sens 'column)))
  (let ((list (get-possibility game coorX coorY))
	(indiceStatic (if (eq sens 'line)
			  coorX
			  coorY)))    
    (loop for indiceMovible from 0 to (1- *size*) do    
       ;; pour parcourir les colonnes c'est le x qui est statique 
       ;; pour parcourir les lignes c'est le y qui est statique
	 (let ((coor (line-column sens indiceMovible indiceStatic)))
	   (unless (and (eq (car coor) coorX)
			(eq (cadr coor)  coorY))
	     (setf list (remove-sublist (get-possibility game (car coor) (cadr coor)) list)))))
    (unless (endp list)
      list))
  )


(defun depth-study-possibility (game x y)
  "Renvoie un x y value, permettant de résoudre la grille (étude en profondeur)"
  (loop for v in (get-possibility game x y) do
       (if (depht-hypothesis game x y v)
	   (return-from depth-study-possibility (values x y v))))
  )


(defun depht-hypothesis(game x y value)
  "Vérifie si à la coordonnée (x,y), la value permet de résoudre la grille"
  (let ((oldSquares (copy-squares (game-squares game)))
	(bool NIL))
    (change-digit game x y value)
    (update-possibility-all-square (game-squares game))
    ;; Vérifie qu'avec x y value on resoud le sudoku
    (when (eq (play-verification-game-over game) 'win)
      (setf bool T))
    
    (setf (game-squares game) oldSquares)
    bool)
  )


(defun play-verification-game-over (game)
  "Joue jusqu'à perdre ou gagner pour vérifier si x y value de l'étude en profondeur va résoudre la grille"
  (loop while T do
       (multiple-value-bind (x y value) (substrategy game '(one-possibility-square
							    one-possibility-in-line
							    one-possibility-in-subsquare))
	 (unless x
	   (return-from play-verification-game-over (game-over game)))
	 (change-digit game x y value)
	 (update-after-change-digit game x y)))
  )


(defun play-strategy (game)
  "Permet de lancer l'IA"
  (loop while (and (not (eq (strategy game) NIL))
		   (not (game-over game))) do
       (main-standalone)
       )
  (game-over game)
  )


;;===========================================
;;==           Implémentation              ==
;;==            FONCTIONS IA               ==
;;===========================================



(defmethod init-standalone (tab)
  (setf *game* (make-instance 'game
			      :game-squares (make-squares)
			      :initial-grid tab))
  ;(setf (initial-grid *game*) tab)
  (init-game *game*)
  (update-possibility-all-square (game-squares *game*))
  )


(defmethod main-standalone ()
  (multiple-value-bind (x y value) (strategy *game*)
    (when x
      (change-digit *game* x y value)
      (update-after-change-digit *game* x y)
      (values x y value)))
  )
