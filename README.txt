 ==========================================
 ==    Guide d'installation du Sudoku    ==
 ==========================================

Auteur :
 . Yordan Kirov
 . Jimmy Gouraud 
 . Sébastien Pouteau


Pré-requis :
 . Avoir installer le logiciel Emacs
 . Savoir ouvrir un fichier avec Emacs
 . Avoir installer SBCL
 . Dans votre répertoire courant, modifiez ou créez le fichier .sbclrc, en ajoutant dedans :
       (require "asdf")
       (push #P"/home/jimgouraud/sudoku/" asdf:*central-registry*)
          -> remplacez "/home/jimgouraud/sudoku" par votre répertoire contenant le sudoku


-----------------------
- Lancement du sudoku -
-----------------------

Mode console:

A la racine du projet effectuer:
   - ./make.sh compile  : ceci compilera le projet
   - ./make.sh run      : le sudoku ce lancera directement dans le terminal,
                          il demandera un chemin de grille.

mode Web:

A la racine du projet effectuer:
   - ./make.sh compile  : ceci compilera le projet
   - ./make.sh web      : il lancera le sudoku, et donnera une adresse, 
                          il suffira de la rentrer dans un navigateur.
           
----------------------------------------
- Création fichier Startegy  du sudoku -
----------------------------------------

effectuer:
   ./make.sh strategy name-strategy  name-file-strategy-out
        (name-strategy = strategy-smart.lisp / strategy-random.lisp)
        (name-file-strategy-out = toto.lisp par exemple, 
                                  toto.lisp contiendra tout les fichiers concaténé en un seul)
