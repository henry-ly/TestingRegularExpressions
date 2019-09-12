import System.Process 
import Test.QuickCheck.Test
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)


grep :: String -> String -> IO ExitCode
grep s r = system $ "echo " ++ s ++ " | grep \"" ++ r ++"\""


unixgrep :: String -> String -> IO Bool
unixgrep s r = do 
            a <- grep s r
          
            case a of
                ExitSuccess ->  return True
                _           ->  return False

equal :: Bool
equal = (unsafePerformIO (unixgrep "a" "a"))

main :: IO()
main = putStrLn "Trying out system calls"

