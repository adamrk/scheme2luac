{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Assembler where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (Builder, 
                                word8, 
                                word32LE,  
                                doubleLE,
                                stringUtf8,
                                lazyByteString,
                                toLazyByteString,
                                )
import Data.Word (Word8, Word32)
import Data.Text.Lazy (Text, pack)
import qualified Data.Map as M

data LuaConst = LuaNil | LuaBool Bool | LuaNumber Double | LuaString String
  deriving (Eq, Show)

data LuaFunc = LuaFunc { source       :: String,
                         startline    :: Word32, 
                         endline      :: Word32,
                         upvals       :: Word8,
                         params       :: Word8,
                         vararg       :: Word8,
                         maxstack     :: Word8,
                         instructions :: [LuaInstruction],
                         constants    :: [LuaConst],
                         functions    :: [LuaFunc]
                       } deriving (Eq, Show)

data LuaOp = OpMove |
             OpLoadK |
             OpLoadBool |
             OpLoadNil |
             OpGetUpVal |
             OpGetGlobal |
             OpGetTable |
             OpSetGlobal |
             OpSetUpVal |
             OpSetTable |
             OpNewTable |
             OpSelf |
             OpAdd |
             OpSub |
             OpMul |
             OpDiv |
             OpMod |
             OpPow |
             OpUnM |
             OpNot |
             OpLen |
             OpConcat |
             OpJmp |
             OpEq |
             OpLT |
             OpLE |
             OpTest |
             OpTestSet |
             OpCall |
             OpTailCall |
             OpReturn |
             OpForLoop |
             OpForPrep |
             OpTForLoop |
             OpSetList |
             OpClose |
             OpClosure |
             OpVarArg deriving (Eq, Show, Ord, Enum, Bounded)

formats :: M.Map LuaOp LuaInstFormats
formats = M.fromList[(OpMove, ABC),
                     (OpLoadNil, ABC),
                     (OpLoadK, ABx),
                     (OpLoadBool, ABC),
                     (OpGetGlobal, ABx),
                     (OpSetGlobal, ABx),
                     (OpGetUpVal, ABC),
                     (OpSetUpVal, ABC),
                     (OpGetTable, ABC),
                     (OpSetTable, ABC),
                     (OpAdd, ABC),
                     (OpSub, ABC),
                     (OpMul, ABC),
                     (OpDiv, ABC),
                     (OpMod, ABC),
                     (OpPow, ABC),
                     (OpUnM, ABC),
                     (OpNot, ABC),
                     (OpLen, ABC),
                     (OpConcat, ABC),
                     (OpJmp, AsBx),
                     (OpCall, ABC),
                     (OpReturn, ABC),
                     (OpTailCall, ABC),
                     (OpVarArg, ABC),
                     (OpSelf, ABC),
                     (OpEq, ABC),
                     (OpLT, ABC),
                     (OpLE, ABC),
                     (OpTest, ABC),
                     (OpTestSet, ABC),
                     (OpForPrep, AsBx),
                     (OpForLoop, AsBx),
                     (OpTForLoop, ABC),
                     (OpNewTable, ABC),
                     (OpSetList, ABC),
                     (OpClosure, ABx),
                     (OpClose, ABC)
                     ]

data LuaInstFormats = ABC | ABx | AsBx deriving (Eq, Show, Ord)

data LuaInstruction = IABC { op :: LuaOp, iA :: Int, iB :: Int, iC :: Int } |
                      IABx { op :: LuaOp, iA :: Int, iBx :: Int } |
                      IAsBx { op :: LuaOp, iA :: Int, isBx :: Int }
                        deriving (Eq, Show)

opFormat :: LuaInstruction -> LuaInstFormats
opFormat (IABC _ _ _ _) = ABC
opFormat (IABx _ _ _) = ABx
opFormat (IAsBx _ _ _) = AsBx

validOpFormat :: LuaInstruction -> Maybe Int
validOpFormat ins = if M.lookup opCode formats == Just (opFormat ins) 
                                  then Just (fromEnum opCode)
                                  else Nothing
                    where opCode = op ins

validA :: Int -> Maybe Int
validA n = if 0 <= n && n < (2^8) then Just n else Nothing

validB :: Int -> Maybe Int
validB n = if 0 <= n && n < (2^9) then Just n else Nothing

validC :: Int -> Maybe Int
validC = validB

validBx :: Int -> Maybe Int
validBx n = if 0 <= n && n < (2^18) then Just n else Nothing

validsBx :: Int -> Maybe Int
validsBx n = if (-131071) <= n && n < (2^18 - 131071) then Just n else Nothing
 -- The sBx entry represents negatives with a -131071 bias

inst2int :: LuaInstruction -> Maybe Word32
inst2int ins@(IABC op a b c) = fmap fromIntegral $ sum <$> sequence 
                                [validOpFormat ins, 
                                fmap ((2^6)*) $ validA a,  
                                fmap ((2^14)*) $ validC c, 
                                fmap ((2^23)*) $ validB b]

inst2int ins@(IABx op a b) = fmap fromIntegral $ sum <$> sequence
                              [validOpFormat ins,
                               fmap ((2^6)*) $ validA a,
                               fmap ((2^14)*) $ validBx b]                                   

inst2int ins@(IAsBx op a b) = fmap fromIntegral $ sum <$> sequence
                           [validOpFormat ins,
                            fmap ((2^6*)) $ validA a,
                            fmap (((2^14)*) . (+131071)) $ validsBx b]

rk :: Int -> Int
rk n = if n >= 256 then 0 else n

maxReg :: LuaInstruction -> Int
maxReg (IABx _ a _) = a
maxReg (IAsBx OpJmp _ _) = 0
maxReg (IAsBx _ a _) = a + 3

maxReg (IABC OpMove a b _) = max a b
maxReg (IABC OpLoadNil a b c) = max a b
maxReg (IABC OpUnM a b c) = max a b
maxReg (IABC OpNot a b c) = max a b
maxReg (IABC OpLen a b c) = max a b
maxReg (IABC OpTestSet a b c) = max a b

maxReg (IABC OpLoadBool a _ _) = a
maxReg (IABC OpGetUpVal a _ _) = a
maxReg (IABC OpSetUpVal a _ _) = a
maxReg (IABC OpTest a _ _) = a
maxReg (IABC OpNewTable a _ _) = a
maxReg (IABC OpClose a _ _) = a

maxReg (IABC OpGetTable a b c) = maximum [a, b, rk c]

maxReg (IABC OpSetTable a b c) = maximum [a, rk b, rk c]
maxReg (IABC OpAdd a b c) = maximum [a, rk b, rk c]
maxReg (IABC OpSub a b c) = maximum [a, rk b, rk c]
maxReg (IABC OpMul a b c) = maximum [a, rk b, rk c]
maxReg (IABC OpDiv a b c) = maximum [a, rk b, rk c]
maxReg (IABC OpMod a b c) = maximum [a, rk b, rk c]
maxReg (IABC OpPow a b c) = maximum [a, rk b, rk c]

maxReg (IABC OpConcat a b c) = maximum [a, b, c]

maxReg (IABC OpCall a b c) = max (a + c - 2) (a + b - 1)

maxReg (IABC OpReturn a b _) = a + b - 2

maxReg (IABC OpTailCall a b _) = a + b - 1 -- could actually return more
maxReg (IABC OpVarArg a b _) = a + b - 1

maxReg (IABC OpEq _ b c) = max (rk b) (rk c)
maxReg (IABC OpLT _ b c) = max (rk b) (rk c)
maxReg (IABC OpLE _ b c) = max (rk b) (rk c)

maxReg (IABC OpTForLoop a _ c) = a + c + 2

maxReg (IABC OpSetList a b _) = a + b

class ToByteString a where
  toBS :: a -> Maybe Builder

instance ToByteString Char where
  toBS c = Just $ stringUtf8 $ [c]

instance ToByteString LuaInstruction where
  toBS = (fmap word32LE) . inst2int

instance ToByteString LuaConst where
  toBS LuaNil = Just $ word8 0
  toBS (LuaBool b) = Just $ word8 1 `mappend` word32LE (if b then 1 else 0) 
    -- how is bool is encoded as 0 and 1 in WHAT FORMAT ???
  toBS (LuaNumber n) = Just $ word8 3 `mappend` doubleLE n
  toBS (LuaString str) = Just $ word8 4 `mappend` word32LE sz `mappend` strbytes
    where sz = fromIntegral $ length str + 1
          strbytes = stringUtf8 $ str ++ "\0"

instance (ToByteString a) => ToByteString [a] where
  toBS xs = mappend <$> Just (word32LE (fromIntegral $ length xs)) <*> 
            (fmap mconcat) (traverse toBS $ xs)

instance ToByteString LuaFunc where
  toBS func = (fmap mconcat) . sequence $
                                  (toBS $ source func) 
                    :  map Just [ word32LE (startline func), 
                                  word32LE (endline func),
                                  word8 (upvals func),
                                  word8 (params func),
                                  word8 (vararg func),
                                  word8 (maxstack func)]
                    ++          [ toBS $ instructions func,
                                  toBS $ constants func,
                                  toBS $ functions func]
                    ++ map Just [ word32LE 0,
                                  word32LE 0,
                                  word32LE 0]       


-- luac header for my setup
luaHeader :: [Word8]
luaHeader = [0x1b, 0x4c, 0x75, 0x61] ++ -- Header Signature
            [0x51] ++ -- Version Lua 5.1
            [0x00] ++ -- Format version official
            [0x01] ++ -- little endian
            [0x04] ++ -- size of int (bytes)
            [0x04] ++ -- size of size_t (bytes)
            [0x04] ++ -- size of instructions (bytes)
            [0x08] ++ -- size of lua_Number (bytes)
            [0x00]    -- integral flag for floating point

luaFunc :: LuaFunc -- Example function to test
luaFunc = LuaFunc {source = "foo", startline=0, endline=0, upvals=0, params=0, vararg=2,
                   maxstack=10, 
                   instructions=[ IABC  OpLoadNil 0 1 0
                                , IABC  OpEq 1 0 1
                                , IAsBx OpJmp 0 1
                                , IAsBx OpJmp 0 3
                                , IABx  OpGetGlobal 0 0
                                , IABx  OpLoadK 1 1
                                , IABC  OpCall 0 2 1
                                , IABC  OpReturn 0 1 0
                                ], 
                   
                   constants=   [ LuaString "print"
                                , LuaNumber 5.0
                                , LuaNumber 6.0],
                   
                   functions=   []}

smallFunc = LuaFunc{ source="@test.lua\0", startline=0, endline=0, upvals=1,
                                  params=0, vararg=0, maxstack=3,
            instructions = [
                           --   IABC  OpGetUpVal 0 0 0
                           -- , IABC  OpSetTable 0 256 257
                           -- , IABx  OpGetGlobal 1 2
                           -- , IABC  OpGetTable 2 0 256
                           -- , IABC  OpCall 1 2 1
                            IABC  OpReturn 0 1 0
                           ],
            constants =    [ 
                           --   LuaNumber 0
                           -- , LuaString "bar"
                           -- , LuaString "print"
                           ],
            functions =    []}

luaFunc' :: LuaFunc -- Example function to test
luaFunc' = LuaFunc { source="@qux\0", startline=0, endline=0, upvals=0, params=0, vararg=2,
                   maxstack=3, 
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0
                                , IABC  OpSetTable 0 256 257
                                , IABC  OpSetTable 0 260 257 
                                , IABx  OpClosure 1 0
                                , IABC  OpMove 0 0 0
                                -- , IABx  OpSetGlobal 1 2
                                -- , IABx  OpGetGlobal 1 3
                                -- , IABx  OpGetGlobal 2 2
                                , IABC  OpCall 1 1 1
                                , IABx  OpGetGlobal 1 3 
                                , IABC  OpGetTable 2 0 256
                                , IABC  OpCall 1 2 1
                                , IABx  OpGetGlobal 1 3
                                , IABC  OpGetTable 2 0 260
                                , IABC  OpCall 1 2 1
                                , IABx  OpGetGlobal 1 3
                                , IABC  OpMove 2 0 0
                                , IABC  OpCall 1 2 1
                                , IABC  OpReturn 0 1 0
                                ], 
                   
                   constants=   [ LuaNumber 0
                                , LuaString "foo"
                                , LuaString "func"
                                , LuaString "print"
                                , LuaNumber 1
                                , LuaString "bar"],
                   
                   functions=   [smallFunc]}

finalBuilder :: LuaFunc -> Maybe Builder
finalBuilder f = (fmap mconcat) . sequence $
                 [Just $ foldMap word8 luaHeader, -- header
                  toBS f -- main function 
                  --Just $ foldMap word32LE [0,0,0] -- 3 optional lists set to 0
                  ] 

-- Write bytestring to file for testing
writeBuilder :: String -> Builder -> IO ()
writeBuilder file = BL.writeFile file . toLazyByteString

testOutput :: IO ()
testOutput = case finalBuilder luaFunc' of 
  Just bs -> writeBuilder "temp" bs
  Nothing -> print "error completing builder"