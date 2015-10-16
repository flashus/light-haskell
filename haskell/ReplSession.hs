module ReplSession (
  ReplSession,
  evalInSession,
  startSession,
  endSession
) where

import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import Control.Monad (liftM)
import Language.Haskell.Exts
import Data.Maybe(fromJust)

findStr :: String -> String -> Maybe Int
findStr sub s
          | length sub > length s      = Nothing
          | take (length sub) s == sub = Just 0
          | otherwise                  = fmap (+1) $ findStr sub $ drop 1 s

rep::String->String->String->String
rep s find repl
  | findStr find s == Nothing = s
  | otherwise = rep newS find repl
  where
    index = fromJust $ findStr find s
    newS = take index s ++ repl ++ drop (index+ (length find)) s

removeList=[("<interactive>",">"),("Prelude Data.List Data.Maybe| ",""),("Prelude Main| ",""),("Prelude Data.List Data.List.Split| ",""),("Prelude Data.Maybe| ",""),("Prelude| ",""),("Prelude Data.List.Split| ",""),("Prelude Data.List| ","")]

clean::String->String
clean s
  | ln == [] = ""
  | head ln == "" = unlines $ tail ln
  | head ln == " " = unlines $ tail ln
  | otherwise = unlines ln
  where
    newS = foldl (\str (find,repl)-> rep str find repl) s removeList
    ln = lines newS


data ReplSession = ReplSession {
  replIn :: Handle,
  replOut :: Handle,
  replError :: Handle,
  replProcess :: ProcessHandle
}

evalInSession :: String -> ReplSession -> IO (Either String String)
evalInSession cmd session@(ReplSession input out err _) = do
  sendCommand (":{\n" ++ prepareCode cmd ++ "\n") session
  clearHandle out 10
  clearHandle err 0
  sendCommand ":}\n" session
  readEvalOutput session

prepareCode :: String -> String
prepareCode code = case parseCode code of
  [] -> code
  decls -> if isFunDecl $ head decls
           then prependLet decls
           else code

isFunDecl :: Decl -> Bool
isFunDecl (TypeSig {}) = True
isFunDecl (FunBind {}) = True
isFunDecl (PatBind {}) = True
isFunDecl _ = False

prependLet :: [Decl] -> String
prependLet = prettyPrint . letStmt

parseCode :: String -> [Decl]
parseCode code = case parseFileContents code of
  (ParseOk (Module _ _ _ _ _ _ decls)) -> decls
  (ParseFailed {}) -> []

readEvalOutput :: ReplSession -> IO (Either String String)
readEvalOutput (ReplSession _ out err _) = do
  output <- readUntil out ("--EvalFinished\n" `isSuffixOf`)
  let onlyOutput = take (length output - length "--EvalFinished\n") output
  hasErrorOutput <- hReady err
  if hasErrorOutput
    then readAll err >>= \errorOutput -> return . Left $ clean errorOutput
    else return . Right $ clean onlyOutput

readUntil :: Handle -> (String -> Bool) -> IO String
readUntil handle predicate = readUntil' handle "" predicate

readUntil' :: Handle -> String -> (String -> Bool) -> IO String
readUntil' handle output predicate = do
  char <- hGetChar handle
  let newOutput = output ++ [char]
  if predicate $ newOutput
    then return newOutput
    else readUntil' handle newOutput predicate

readAll :: Handle -> IO String
readAll handle = untilM' (liftM not $ hReady handle) (hGetChar handle)

startSession :: FilePath -> IO ReplSession
startSession path = do
  cabalProject <- isCabalProject path
  let (cmd, args) = if cabalProject then ("cabal", ["repl"]) else ("ghci", [])
  (input, out, err, process) <- runInteractiveProcess cmd args (Just path) Nothing
  let session = ReplSession input out err process
  prepareSession session
  return session

isCabalProject :: FilePath -> IO Bool
isCabalProject dir = do
  files <- getDirectoryContents dir
  return $ any (".cabal" `isSuffixOf`) files

prepareSession :: ReplSession -> IO ()
prepareSession session@(ReplSession _ out _ _) = do
  sendCommand ":set prompt \"--EvalFinished\\n\"\n" session
  clearHandle out 1000

sendCommand :: String -> ReplSession -> IO ()
sendCommand cmd (ReplSession input _ _ _) = do
  hPutStrLn input cmd
  hFlush input

clearHandle :: Handle -> Int -> IO ()
clearHandle handle wait =
  untilM (liftM not $ hWaitForInput handle wait) $ do
    hGetChar handle

untilM :: (Monad m) => m Bool -> m a -> m ()
untilM predicate action = untilM' predicate action >> return ()

untilM' :: (Monad m) => m Bool -> m a -> m [a]
untilM' predicate action = do
  isFinished <- predicate
  if isFinished
    then return []
    else do
      res <- action
      others <- untilM' predicate action
      return $ res : others

endSession :: ReplSession -> IO ()
endSession session = do
  sendCommand ":quit\n" session
  waitForProcess $ replProcess session
  return ()
