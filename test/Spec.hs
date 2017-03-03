import Assembler
import CodeGenerator
import Parser
import Test.Hspec
import Test.QuickCheck
import System.Process
import System.FilePath
import System.Exit
import qualified Data.Map as M
import Data.List (nub)


endVal :: String -> String
endVal s = let ls = lines s
               n  = length ls
           in  drop 8 $ ls !! (n - 5)


defineScripts :: [String]
defineScripts = map ("test/testfiles/" ++) 
  [ "define1.scm"
  , "define2.scm"
  , "define3.scm" 
  ]

arithScripts :: [String]
arithScripts = map ("test/testfiles/" ++) 
  [ "arith1.scm" 
  , "arith2.scm"
  , "arith3.scm"
  , "arith4.scm"
  , "arith5.scm" 
  ]

boolScripts :: [String]
boolScripts = map ("test/testfiles/" ++)  
  [ "bool1.scm" 
  , "bool2.scm"
  , "bool3.scm"
  , "bool4.scm"
  , "bool5.scm"
  ]

lambdaScripts :: [String]
lambdaScripts = map ("test/testfiles/" ++)
  [ "lambda1.scm"
  , "lambda2.scm"
  , "lambda3.scm"
  , "lambda4.scm"
  , "lambda5.scm" 
  ]

recursiveScripts :: [String]
recursiveScripts = map ("test/testfiles/" ++)
  [ "recursive1.scm" ]

fileExCompare :: String -> SpecWith ()
fileExCompare s = 
        let outfile = replaceExtension s "luac"
        in
          do 
            a <- runIO $ readCreateProcess (shell $ "scheme < " ++ s) ""
            b <- runIO $ do
              f <- compileFromFile s
              case f >>= finalBuilder of
                Just bs -> writeBuilder outfile bs
                Nothing -> print "assembly error"
              (head . lines) <$> 
                readCreateProcess (shell $ "lua " ++ outfile) ""
            it s $ endVal a `shouldBe` b

bytecodeParses :: String -> LuaFunc -> SpecWith ()
bytecodeParses s luafunc = do
  (exitCode, stdOut, stdErr) <- runIO $
    case finalBuilder luafunc of
      Just bs -> writeBuilder outfile bs >> do 
        readCreateProcessWithExitCode (shell $ "luac -l -l " ++ outfile) "" 
      Nothing -> return (ExitFailure 101, "AssemblyError", "AssemblyError")
  it (s ++ " assembled") $ exitCode `shouldNotBe` (ExitFailure 101)
  it (s ++ " valid bytecode") $ exitCode `shouldBe` ExitSuccess
  where outfile = "test/testfiles/temp.luac" 

dummyLuaFunc :: Value -> LuaFunc
dummyLuaFunc v = let atable = M.fromList $ zip (nub $ getAtoms v) [1..]
                 in  genEval atable v

main :: IO ()
main = hspec $ do
  describe "define tests" 
        $ mapM_ fileExCompare defineScripts
  describe "arithmetic tests"
        $ mapM_ fileExCompare arithScripts
  describe "boolean tests"
        $ mapM_ fileExCompare boolScripts
  describe "lambda tests"
        $ mapM_ fileExCompare lambdaScripts
  describe "recursive tests"
        $ mapM_ fileExCompare recursiveScripts
  describe "bytecodeParses" 
        $ mapM_ (uncurry bytecodeParses) primitives
