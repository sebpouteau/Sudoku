 =========================================
 ==    Guide d'utilisation du Sudoku    ==
 =========================================

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

Dans Slime :
 . Ouvrez emacs
 . Faites  M-x slime  ou  Alt-x slime 
 . Dans le *slime-repl sbcl* qui apparait, tapez :
       (asdf:load-system "sudoku")
     - Faites Ctr-c Alt-p SUDOKU
     - En enfin, lancer le jeu avec :
       (main)


Dans le Terminal :
  == Version 1 ==
 . Ouvrez emacs
 . Faites  M-x slime  ou  Alt-x slime 
 . Dans le *slime-repl sbcl* qui apparait, tapez :
       (compile-file "start.lisp")  -> cela génère le fichier start.fasl
 . Retournez sur le terminal, et tapez :
       sbcl --noinform --load start.fasl
 . Aprés avoir joué, tapez dans le SBCL du terminal :
       (SB-EXT:EXIT)

  == Version 2 ==
 . Tapez sbcl
 . Dans le SBCL du terminal, tapez :
       (asdf:load-system "sudoku")
       (sb-ext::save-lisp-and-die "sudoku.core" :toplevel #'sudoku::main)


