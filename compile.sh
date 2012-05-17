#!/bin/sh
# compile.sh:
#   This script compiles Parinati.
mkdir bin
echo Compiling...
ocamake *.ml* str.cmxa -opt -o bin/parinati
echo Done.
