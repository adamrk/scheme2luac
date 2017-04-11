{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module LibCompiler where

import Data.Maybe
import Foreign.C.Types
import qualified Data.ByteString as BS
import Text.Trifecta (parseByteString, Result(..))
import Scripting.Lua
import Scripting.Lua.Raw

import Parser2
import CodeGenerator

foreign export ccall
  compile :: LuaState -> IO CInt

filename :: BS.ByteString
filename = "/tmp/scheme2luac.luac"

compile :: LuaState -> IO CInt
compile l = do
--  putStrLn "compiling" -- for debugging
  str :: BS.ByteString <- fromJust `fmap` peek l 1
--  print str -- for debugging
  let tree = parseByteString parProgram mempty str
  let luafunc = fmap evalWrapper tree
  case luafunc of
    Success x -> writeLuaFunc "/tmp/scheme2luac.luac" (Just x)
    Failure _ -> return ()
  push l filename
  return 1