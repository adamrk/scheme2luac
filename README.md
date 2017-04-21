Scheme2Luac
==============
[![made at Recurse Center](https://cdn.rawgit.com/heatherbooker/made_at_rc/master/made_at_RC.svg)](https://www.recurse.com)

# Overview
A Scheme compiler for the Lua VM, still in progress. It is designed for Lua 5.1 (download [here](https://www.lua.org/download.html)). Build using Haskell's package manager stack (available [here](https://www.haskellstack.org)):
```
$ git clone https://github.com/adamrk/scheme2luac.git
$ cd scheme2luac
$ stack build
```

To compile a Scheme script and run in Lua, first check you have Lua 5.1:
```
$ lua -v
```
This should return 'Lua 5.1.* Copyright (C) 1994-2012 Lua.org, PUR-Rio' or similar.

Then create scheme script (`test.scm` in this example) and run:
```
$ stack exec -- scheme2luac-exe test.scm out.luac
$ lua out.luac
```

# Implemented Components of R5RS Standard
This project aims to implement the [R5RS](http://www.schemers.org/Documents/Standards/R5RS/) scheme standard.
These expressions have been implemented:
* `let`
* `define`
* `lambda`
* `if`
* `quote`
* `eval`

as well as several primitive functions (see the list of primitives in [CodeGenerator.hs](src/CodeGenerator.hs)).
There is also support for macros using `define-syntax`.

# Differences from R5RS Standard
* The full heirarchy of numerical types is not supported. All numbers are converted to Lua Numbers which are doubles.
* `quasiquote` is not supported.
* `quoted` symbols do not yet print properly.
* equivalence predicates are not supported.
* input/output ports are not yet supported
* many derived expressions are not supported, but can easily be implemented since we have support for macros. 

# Using 'eval'
If your scheme script uses the `eval` function then it will need to have access to the compiler at runtime. This requires a special compilation process (these commands are in the file [build.sh](build.sh)):
```
$ stack build
$ stack exec -- ghc LibCompiler.hs -shared -dynamic -fPIC -o libcompiler.so -lHSrts-ghc8.0.2
$ stack exec -- ghc libcompilerhelper.c -no-hs-main -optl -L. -lcompiler -o lualibhelper.so -shared -fPIC -dynamic
```
Note that you will need to change the option `-lHSrts-ghc8.0.2` to match with your version of GHC. Check which version of ghc you're running with `stack exec -- ghc --version`. You will also need the lua header files `lua.h` and `luaconf.h` which are available [here](https://www.lua.org/source/5.1/). Modify the `-L.` flag to the path to these header files.

This will create two shared libraries: `libcompiler.so` and `lualibhelper.so`. At runtime Lua must be able to access the `lualibhelper.so` library and the path to `libcompiler.so` must be in the `LD_LIBRARY_PATH` environment variable. Otherwise you will see an error like this:
```
lua: error loading module 'lualibhelper' from file './lualibhelper.so':
    libcompiler.so: cannot open shared object file: No such file or directory
```