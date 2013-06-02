import System.IO
import System.Directory
import System.Environment
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "delete" = tododelete
dispatch command = doesntExist command

main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add (filename:todos) = mapM_ (\todo -> appendFile filename $ todo ++ "\n") todos

view :: [String] -> IO ()
view [filename] = do
  contents <- readFile filename
  mapM_ putStrLn $ lines contents

tododelete :: [String] -> IO ()
tododelete [filename, strNumber] = do
  contents <- readFile filename
  let tasks = lines contents
  let number = read strNumber
  let newTasks = unlines $ delete (tasks !! number) tasks
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTasks
  hClose tempHandle
  removeFile filename
  renameFile tempName filename

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ command ++ "は存在しません"
