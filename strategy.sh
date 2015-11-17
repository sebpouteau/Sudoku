#!/bin/bash
FILE_OUT=$2
STRAT=$1
cat sudoku-static-method.lisp >> $FILE_OUT
cat sudoku-generic.lisp >> $FILE_OUT
cat sudoku-method.lisp >> $FILE_OUT
cat sudoku-affichage.lisp >> $FILE_OUT
cat $STRAT  >> FILE_OUT
cat sudoku-strategy.lisp >> $FILE_OUT
