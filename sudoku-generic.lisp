
;;===========================================
;;==             Interface                 ==
;;==  contenant toutes les méthodes utile  ==
;;===========================================

(in-package :sudoku)
		
;; =================
;; =    Données    =
;; =================

(defvar *sqrt-size* 3 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "the current instance of a game")
(defvar *digits* '(1 2 3 4 5 6 7 8 9))
;; ========================
;; ==     Coordonnée     ==
;; ========================

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defgeneric x-coor (coor)
  (:documentation "x slot of coor"))

(defgeneric y-coor (coor)
  (:documentation "y slot of coor"))

(defgeneric make-coor (x y)
  (:documentation "instance of a coor [x,y]"))

;; ===================
;; ==     Carre     ==
;; ===================

(defclass square ()
  ((coor :initarg :coor :accessor coor) 
   (possible-digits :initform (copy-list *digits*)
		    :initarg :possible-digits :accessor possible-digits)
   (digit :initarg :digit :accessor digit :initform 0)
   (protected :initform nil :initarg :protected :accessor protected))
  (:documentation "one square of the squares
         coor -> coordonnée du carré dans la grille
         possible-digits -> valeurs (nombre) possibles du carré
         digit -> valeur (nombre) affiché du carré
         protected -> vaut true si le nombre était au début => non-modifiable"))

(defgeneric make-square (coor &optional digit)
  (:documentation "creates a square containing coor and digit"))

(defgeneric coor-square (squares coor)
  (:documentation "the square at COOR in SQUARES"))

(defgeneric possible-digits (square)
  (:documentation 
   "list of possible digits remaining in SQUARE"))

(defgeneric digit (square)
  (:documentation 
   "assigned digit of square (0 if unassigned)"))

(defgeneric protected (square) 
  (:documentation 
   "T if square was originally filled in the grid"))

(defgeneric assigned-p (square)
  (:documentation "T if the digit slot a striclty positive and inferior to 10"))

(defgeneric copy-square(square)
  (:documentation "copie un square et retourne la copie"))


;; ==================
;; ==     Grid     ==
;; ==================

(defclass squares ()
  ((squares-array :initarg :squares-array :reader squares-array :type array
		  :initform (make-squares-array *size*))
   (to-fill :initform *nb-squares* :accessor to-fill))
  (:documentation "class containing a two dimensional array of instances of the square class \
                   to-fill -> nombre de cases vides (= à remplir)"))

(defgeneric make-squares()
  (:documentation "return array two dimensional (size,  size) "))

(defgeneric to-fill (squares)
  (:documentation "return number of free case"))
		  
(defgeneric make-squares-array (size)
  (:documentation "return array two dimensional (size,  size) "))

(defgeneric squares-array (size)
  (:documentation "reader squares-array "))

(defgeneric grid-to-square (squares)
  (:documentation "attribue the right coordinates to square-array"))

(defgeneric change-digit (squares x y value)
(:documentation "change digit du carré [x,y]
                  et met a jour les possibilité de la ligne, colonne et du sous-carré")) 

(defgeneric copy-squares(squares)
  (:documentation "copi un squares et retourne la copie"))


(defgeneric update-possibility (squares x y list)
  (:documentation "met à jour les possible digit du carré [x,y] par rapport à list
                   Il supprime tout les éléments de list présent dans la list des possibilité"))

(defgeneric remove-sublist (list1 list2)
  (:documentation "supprime les éléments tout les élément de list2 présent dans list1 et renvoie list2"))
 
(defgeneric update-possibility-line (squares indiceStatic &optional sens comportement )
  (:documentation "cette fonction a plusieur fonctionnalité configurer grace au deux paramètre optionnel:
                     sens -> 'line ou 'column  ermet de parcourir soit les ligne ou les colonnes
                     comportement -> 'list : renvoie les digit de la ligne ou colonne
                                  -> 'update-possibility: met à jour les possible digit de tout les carrés d'une ligne ou colonne
                                  -> 'update-and-list: met a jour les possible digit et renvoie la list des digit de la ligne ou colonne "

))

(defgeneric update-possibility-subsquares (squares x y &optional comportement)
  (:documentation "cette fonction permet de gérer les sous-carré configurable avec comportement
                     comportement -> 'list : renvoie les digit du sous carré
                                  -> 'update-possibility: met à jour les possible digit de tout les carrés du sous-carré
                                  -> 'update-and-list: met a jour les possible digit et renvoie la list des digit du sous-carré "
))

(defgeneric update-possibility-all-square (squares)
(:documentation "met a jour les possible-digits de tout les carrés d'une grille")
)


;; ==================
;; ==     Game     ==
;; ==================

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :reader initial-grid :initarg :initial-grid))
  (:documentation "class for game instances"))

(defgeneric game-squares (game)
  (:documentation "the squares of GAME"))

(defgeneric initial-grid (game)
  (:documentation "the initial grid of GAME"))

(defgeneric game-over (game)
  (:documentation "if the game is over (either won or lost"))

(defgeneric init-game (game)
  (:documentation "initializes GAME with its initial-grid"))

(defgeneric game-with-new-grid (&optional strategy)
  (:documentation "instance of game with a grid and STRATEGY"))

(defgeneric game-do (game square)
  (:documentation 
   "plays coor/digit of square in the coor-square in squares of GAME"))

