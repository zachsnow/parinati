#!/bin/sh
# compile.sh:
#   This script compiles Parinati.
echo Compiling...
ocamake *.ml* str.cmxa -opt -o parinati
# echo Installing...
# cp parinati ~/bin
echo Done.
