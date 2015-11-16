
;;============================================
;;==             Interface                  ==
;;==  contenant toutes les méthodes utiles  ==
;;============================================

(in-package :sudoku)
		
;; =====================
;; ==     Données     ==
;; =====================

(defun create-list-possibility (size list)
  (if (= size 0)
      list
      (create-list-possibility (1- size) (cons size list))))

(defvar *sqrt-size* 3 "Side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "The current instance of a game")
(defvar *digits* (create-list-possibility *size* '()))


;; =========================
;; ==     Coordonnées     ==
;; =========================

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "Class for coordinates in the squares grid"))

(defgeneric make-coor (x y)
  (:documentation "Instance of a coor [x,y]"))


;; ===================
;; ==     Carre     ==
;; ===================

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


;; ====================
;; ==     Grille     ==
;; ====================

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


;; ==================
;; ==     Game     ==
;; ==================

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :reader initial-grid :initarg :initial-grid))
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


;; =======================
;; ==     Affichage     ==
;; =======================

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
