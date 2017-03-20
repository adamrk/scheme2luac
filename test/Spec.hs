import Assembler
import CodeGenerator
import Test.Hspec
import Test.QuickCheck
import System.Process
import System.FilePath
import System.Exit
import System.Directory
import qualified Data.Map as M
import Data.List (nub, sort, groupBy)
import Data.Char (isAlpha)


endVal :: String -> String
endVal s = let ls = lines s
               n  = length ls
           in  drop 8 $ ls !! (n - 5)

fileExCompare :: String -> SpecWith ()
fileExCompare s = 
        let outfile = replaceExtension s "luac"
        in
          do 
            (_,a,_) <- runIO $ readCreateProcessWithExitCode (shell $ "scheme < " ++ s) ""
            (exitcode, b, _) <- runIO $ do
              f <- compileFromFile s
              case f >>= finalBuilder of
                Just bs -> writeBuilder outfile bs
                Nothing -> print "assembly error"
              readCreateProcessWithExitCode (shell $ "lua " ++ outfile) ""  
            it s $ do
              exitcode `shouldBe` ExitSuccess
              endVal a `shouldBe` (head . lines $ b)

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

main :: IO ()
main = hspec $ do
  testFiles <- runIO $ do
      files <- listDirectory "test/testfiles"
      return $ groupBy (\x y -> (takeWhile isAlpha x == takeWhile isAlpha y)) 
             . sort 
             . filter ((== ".scm") . takeExtension) 
             $ files
  mapM_ run testFiles
  describe "bytecodeParses" 
        $ mapM_ (uncurry bytecodeParses) primitives
  where
    run xs = let name = takeWhile isAlpha (head xs)
             in  describe (name ++ " tests") $
                    mapM_ (fileExCompare . ("test/testfiles/" ++)) xs