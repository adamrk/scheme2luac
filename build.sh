#!/bin/bash

stack build
stack exec -- ghc LibCompiler.hs -shared -dynamic -fPIC -o libcompiler.so -lHSrts-ghc8.0.2
stack exec -- ghc libcompilerhelper.c -no-hs-main -optl -L. -lcompiler -o lualibhelper.so -shared -fPIC -dynamic