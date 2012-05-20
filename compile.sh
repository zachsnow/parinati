#!/bin/sh
echo Compiling parinati...
mkdir -p bin
ocamake src/*.ml* str.cmxa -opt -o bin/parinati
echo Done.
