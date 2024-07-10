import GeneralizedTree
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  evalArgs args

evalArgs :: [String] -> IO ()
evalArgs args =
  case length args of
    0 ->
      putStrLn "- for standard input and -f <filename> for a file."
    1 | args == ["-"] ->
      let s = getContents
      in  fmap length s `seq` s >>= putStrLn . grow . clone
    2 | head args == "-f" ->
      readFile (last args) >>= putStrLn . grow . clone
    _ ->
      errorWithoutStackTrace "too many arguments"
