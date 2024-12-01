import GeneralizedTree
  ( grow
  , clone
  , Wood (..)
  )
import System.Environment (getArgs)
import Data.List (intercalate)


-- | The input method: read from stdin or from a specified file.
-- Null stands for unspecified method input.  This is intended to be implemented
-- as a placeholder when no input methods were specified.
data InputMethod = StdIn | File String | Null


-- | The configuration of our application will dictate how it will behave.
-- The configuration are:
--     • the input method;
--     • the type of drawing characters (wood type).
data AppConfig = AppConfig
       { inputMethod :: InputMethod
       , woodType    :: Wood
       }


-- | This is the default configuration for our application.  This is overridden
-- by the user-given options.
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { inputMethod = Null
  , woodType = Unicode
  }


-- | Evaluate the command-line arguments we have received once.  A single call
-- does not exhaustively evaluate all the arguments or options.
evalArgs :: AppConfig -> [String] -> IO ()
evalArgs config (a1:a2:as) =
  case a1:a2:[] of
    ["-w", wood] -> let config' = config
                          { woodType = case wood of
                                         "ascii"   -> ASCII
                                         "unicode" -> Unicode
                                         "round"   -> Round
                                         -         -> Unicode}
                    in  evalArgs config' as

    ["-f", file] -> let config' = config
                          { inputMethod = File file }
                    in  evalArgs config' as

    _            -> errorWithoutStackTrace helpText

evalArgs config ("-":as) =
  let config' = config { inputMethod = StdIn }
  in  evalArgs config' as

evalArgs config [] = runApp config

evalArgs _      _  = errorWithoutStackTrace helpText


-- | The text message to display when either options are incorrect or if no
-- specific action was specified.  The title of the our application is supposed
-- to be this.
--  ____  __   ____  ____  ____  ____ 
-- (  __)(  ) (_  _)(  _ \(  __)(  __)
--  ) _) / (_/\ )(   )   / ) _)  ) _) 
-- (__)  \____/(__) (__\_)(____)(____)
--
-- However, the backslashes disfigure it.  There‘s nothing we can do.
helpText :: String
helpText = intercalate "\n"
  [ " ____  __   ____  ____  ____  ____ "
  , "(  __)(  ) (_  _)(  _ \\(  __)(  __)"
  , " ) _) / (_/\\ )(   )   / ) _)  ) _) "
  , "(__)  \\____/(__) (__\\_)(____)(____)"
  , ""
  , "These are the possible options:"
  , "  • -f <file>"
  , "      Read input from <file>."
  , "      <file> must exist."
  , "  • -w <wood type>"
  , "      Specify the drawing characters."
  , "      Possible values for <wood type> are:"
  , "        · ’ascii’ for ASCII characters"
  , "        · ‘unicode’ for UTF-8 characters"
  , "        · ‘round’ for rounded UTF-8 characters"
  , ""
  , "To read from stdin, provide a simple hyphen at the end."
  , "For more information, refer the documentation."
  ]


-- | Run our application with the specified configuration.
runApp :: AppConfig -> IO ()
runApp (AppConfig im w) =
  case im of
    StdIn ->  do s <- getContents
                 let n = length s
                 n `seq` (putStrLn . grow w . clone) s
                 {- This is required to make evaluation strict. -}
    File f -> do s <- readFile f
                 (putStrLn . grow w . clone) s
    Null   -> putStrLn helpText
    

-- | The main function (obligatory comment).
main :: IO ()
main = do
  args <- getArgs
  evalArgs defaultConfig args
