 ==========================================
 ==    Guide d'installation du Sudoku    ==
 ==========================================

Auteur :
 . Yordan Kirov
 . Jimmy Gouraud 
 . Sébastien Pouteau


Pré-requis :
 . Avoir installer le logiciel Emacs
 . Avoir installer SBCL
 . Dans votre répertoire courant, modifiez ou créez le fichier .sbclrc, en ajoutant dedans :
       (require "asdf")
       (push #P"/home/jimgouraud/sudoku/" asdf:*central-registry*)
          /!\ Remplacez "/home/jimgouraud/sudoku" par votre répertoire contenant le sudoku


-----------------------
- Lancement du sudoku -
-----------------------

Mode console:

A la racine du projet, effectuez:
   - ./make.sh compile  -> ceci compilera le projet
   - ./make.sh run      -> le sudoku se lancera directement dans le terminal,
                          il demandera un chemin de grille.

Mode interface Web:

A la racine du projet, effectuez:
   - ./make.sh compile  -> ceci compilera le projet
   - ./make.sh web      -> il lancera le sudoku, et donnera une adresse, 
                          il suffira de rentrer l'adresse dans un navigateur.

Pour obtenir de l'aide concernant les différentes options de make.sh, effectuez :
   - ./make.sh help


---------------------------------------
- Création fichier strategy du sudoku -
---------------------------------------

effectuez:
   ./make.sh strategy name-strategy name-strategy-output
        avec : 
	   - name-strategy = strategy-smart.lisp / strategy-random.lisp
           - name-strategy-output = toto.lisp par exemple. 
               toto.lisp contiendra tous les fichiers concaténés en un seul
