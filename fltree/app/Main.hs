import GeneralizedTree
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  evalArgs args

evalArgs :: [String] -> IO ()
evalArgs args =
  case length args of
    0 -> putStrLn "- for standard input and -f <filename> for a file."
    1 | args == ["-"] -> do s <- getContents
                            putStrLn . grow . clone $ length s `seq` s
    2 | head args == "-f" -> readFile (last args) >>= putStrLn . grow . clone
    _ -> errorWithoutStackTrace "too many arguments"
