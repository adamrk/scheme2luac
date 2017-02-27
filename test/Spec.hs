import Assembler
import CodeGenerator
import Test.Hspec
import System.Process
import System.FilePath

endVal :: String -> String
endVal s = let ls = lines s
               n  = length ls
           in  drop 8 $ ls !! (n - 5)


defineScripts :: [String]
defineScripts = map ("test/testfiles/" ++) 
  [ "define1.scm" ]

arithScripts :: [String]
arithScripts = map ("test/testfiles/" ++) 
  [ "arith1.scm" ]

lambdaScripts :: [String]
lambdaScripts = map ("test/testfiles/" ++)
  [ "lambda1.scm" ]

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

main :: IO ()
main = hspec $ do
  describe "define tests" 
        $ mapM_ fileExCompare defineScripts
  describe "arithmetic tests"
        $ mapM_ fileExCompare arithScripts
  describe "lambda tests"
        $ mapM_ fileExCompare lambdaScripts