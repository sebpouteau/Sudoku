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
    if [ ! -d "./bin" ];then
	mkdir bin
    fi
    mv sudoku.core bin/

elif [ $FONCT == $RUN ]
then
    if [ ! -d "./bin" ];then
	 echo -e "Vous devez d'abord compiler avant de run. Faire : 
     ./make.sh compile"
    else
	sbcl --noinform --core bin/sudoku.core
    fi

elif [ $FONCT == $CLEAN ]
then
    rm -rf sudoku/*~ sudoku/*.fasl
    rm -rf sudoku-prof/*~ sudoku-prof/*.fasl
    rm -rf strategy/*~ strategy/*.fasl
    rm -rf *~ *.fasl
    rm -rf bin

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
    echo -e "
   ====================
   ====    HELP    ====
   ====================

Arguments possibles: 

  - compile
       Compile le programme et créer l'éxécutable placé dans bin/ 

  - run
       Lance le sudoku dans le terminal

  - web
       Lance le sudoku sur une page web

  - strategy name-strategy name-strategy-output
        avec : 
	   - name-strategy = strategy-smart.lisp / strategy-random.lisp
           - name-strategy-output = toto.lisp par exemple. 
                toto.lisp contiendra tous les fichiers concaténés en un seul

  - clean 
       Nettoie l'archive
"
fi




