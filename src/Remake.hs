
import Signal

import Control.Monad (forever,when,guard)
import Data.Maybe (mapMaybe)
import System.Directory (getDirectoryContents,doesDirectoryExist)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.INotify (INotify,withINotify,EventVariety(..),Event(..),addWatch)
import System.Process (createProcess,shell,waitForProcess)
import Text.Regex.PCRE (Regex,makeRegex,matchTest)


data Config = Config
  { cfgCmd     :: String
  , cfgPattern :: Regex
  , cfgPath    :: FilePath
  }

cfgMatch :: Config -> FilePath -> Bool
cfgMatch  = matchTest . cfgPattern

remakeEvents :: [EventVariety]
remakeEvents  =
  [ Modify
  , CloseWrite
  , MoveIn
  , MoveOut
  , Create
  , Delete
  ]

getConfig :: IO Config
getConfig  = do
  args <- getArgs
  case args of

    [path,cmd,pat] -> return Config
      { cfgCmd     = cmd
      , cfgPattern = makeRegex pat
      , cfgPath    = path
      }

    _ -> printUsage >> exitFailure

printUsage :: IO ()
printUsage  = do
  prog <- getProgName
  putStrLn ("Usage: " ++ prog ++ " path cmd pattern")


main :: IO ()
main  = do
  cfg <- getConfig
  sig <- newSignal
  withINotify $ \ i -> do
    addWatches i sig cfg (cfgPath cfg)
    forever $ do
      block sig
      remake cfg

-- | Add watches on non-dot directories.
addWatches :: INotify -> Signal -> Config -> FilePath -> IO ()
addWatches i sig cfg = loop
  where
  loop path = do
    isDir <- doesDirectoryExist path
    when isDir $ do
      putStrLn ("Watching: " ++ path)
      _wd <- addWatch i remakeEvents path (handleEvent i sig cfg)
      mapM_ loop =<< dirs path

dirs :: FilePath -> IO [FilePath]
dirs path = mapMaybe addPrefix `fmap` getDirectoryContents path
  where
  addPrefix d = do
    guard (not (null d) && head d /= '.')
    return (path </> d)

remake :: Config -> IO ()
remake cfg = do
  putStrLn (" > " ++ cfgCmd cfg)
  (_,_,_,ph) <- createProcess (shell (cfgCmd cfg))
  _          <- waitForProcess ph
  putStrLn " > done"

handleEvent :: INotify -> Signal -> Config -> Event -> IO ()
handleEvent _i sig cfg evt = do
  case evt of

    Modified False (Just path) | cfgMatch cfg path -> signal sig

    _ -> return ()
