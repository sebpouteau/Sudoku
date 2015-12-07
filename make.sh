#!/bin/bash

FONCT=$1
COMPILE="compile"
RUN="run"
CLEAN="clean"
STRATEGY="strategy"
WEB="web"
HELP="help"
if [ -z $FONCT ]
then
    echo -e "Missibg argument compile, run, make strategy, web, help"

elif [ $FONCT == $COMPILE ]
then
    echo -e "(asdf:load-system \"sudoku\")\n(sb-ext::save-lisp-and-die \"sudoku.core\" :toplevel #'sudoku::main)" |sbcl --noinform --noprint
   mv sudoku.core bin/

elif [ $FONCT == $RUN ]
then
    sbcl --noinform --core bin/sudoku.core
elif [ $FONCT == $CLEAN ]
then
    rm -rf sudoku/*~ sudoku/*.fasl
    rm -rf sudoku-prof/*~ sudoku-prof/*.fasl
    rm -rf strategy/*~ strategy/*.fasl
    rm -rf *~ *.fasl
    rm -rf bin/*
elif [ $FONCT == $STRATEGY ]
then
    FILE_OUT=$3
    STRAT=$2
    rm -rf $FILE_OUT
    cat sudoku/sudoku-static-method.lisp > tmp
    cat sudoku/sudoku-generic.lisp >> tmp
    cat sudoku/sudoku-method.lisp >> tmp
    cat sudoku/sudoku-affichage.lisp >> tmp
    cat strategy/$STRAT  >> tmp
    cat sudoku/sudoku-strategy.lisp >> tmp
    sed '/package/d' tmp > $FILE_OUT
    rm -rf tmp
elif [ $FONCT == $WEB ]
then
    sudo /etc/cfengine3/scripts/packages_update.d/sbcl.update
    echo -e "(asdf:load-system \"sudoku\")\n(asdf:load-system \"mini-gui\")\n(gui-sudoku::sudoku)" |sbcl --noinform
elif [ $FONCT == $HELP ]
then
    echo -e " ===========================
             HELP
 ===========================\n"
    echo -e "Arguements possibles: \n
  - compile
       Compile le programme et créer l'executable placer dans bin/ \n
  - run
       Lance le sudoku dans le terminal\n
  - web
       Lance le sudoku sur une page web\n
  - stretegy name-startegy-input  name-staretgy-output
       Créer un fichier strategy\n
  - clean 
       Nettoye l'archive\n"
fi




