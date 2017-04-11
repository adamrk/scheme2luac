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

fileExCompare :: [String] -> String -> SpecWith ()
fileExCompare res s =
  let outfile = replaceExtension s "luac"
      resultfile = replaceExtension s "result"
  in
    do
      (exitcode, b, _) <- runIO $ do
        f <- compileFromFile s
        case maybeToEither f >>= finalBuilder of
          Right bs -> writeBuilder outfile bs
          Left s -> print $ "assembly error: " ++ s
        readCreateProcessWithExitCode (shell $ "lua " ++ outfile) ""
      a <- if resultfile `elem` res
        then runIO $ readFile resultfile
        else runIO $ do
          (_,r,_) <- readCreateProcessWithExitCode (shell $ "scheme < " ++ s) ""
          return (endVal r)
      it s $ do
        exitcode `shouldBe` ExitSuccess
        a `shouldBe` (head . lines $ b)

bytecodeParses :: String -> LuaFunc -> SpecWith ()
bytecodeParses s luafunc = do
  (exitCode, stdOut, stdErr) <- runIO $
    case finalBuilder luafunc of
      Right bs -> writeBuilder outfile bs >> do 
        readCreateProcessWithExitCode (shell $ "luac -l -l " ++ outfile) "" 
      Left s -> return (ExitFailure 101, "AssemblyError: " ++ s, "AssemblyError: " ++ s)
  it (s ++ " assembled") $ exitCode `shouldNotBe` (ExitFailure 101)
  it (s ++ " valid bytecode") $ exitCode `shouldBe` ExitSuccess
  where outfile = "test/testfiles/temp.luac" 

main :: IO ()
main = hspec $ do
  (testFiles, resultFiles) <- runIO $ do
      files <- listDirectory "test/testfiles"
      let test = groupBy (\x y -> (takeWhile isAlpha x == takeWhile isAlpha y)) 
               . sort 
               . filter ((== ".scm") . takeExtension) 
               $ files
          results = filter ((== ".result") . takeExtension) files
      return $ (test, results)
  mapM_ (run (map ("test/testfiles/" ++) resultFiles)) testFiles 
  describe "bytecodeParses" 
        $ mapM_ (uncurry bytecodeParses) primitives
  where
    run res xs = let name = takeWhile isAlpha (head xs)
                 in  describe (name ++ " tests") $
                        mapM_ (fileExCompare res . ("test/testfiles/" ++)) xs