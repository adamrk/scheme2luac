# Scheme2Lua

A Scheme compiler for the Lua VM, still in progress. It is designed for Lua 5.1 (download [here](https://www.lua.org/download.html)). Build using Haskell's package manager stack (available [here](https://www.haskellstack.org)):
```
git clone https://github.com/adamrk/scheme2lua.git
cd scheme2lua
stack build
```

To compile a Scheme script and run in Lua, first check you have Lua 5.1:
```
lua -v
```
>  Lua 5.1.* Copyright (C) 1994-2012 Lua.org, PUR-Rio

Then run:
```
touch test.scm
stack exec -- scheme2lua-exe test.scm out.luac
lua out.luac
```