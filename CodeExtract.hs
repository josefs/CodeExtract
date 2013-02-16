{-# LANGUAGE PatternGuards #-}
module Main where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Templates
import System.Environment
import System.FilePath
import qualified Data.Set as S
import qualified Data.Map as M
import Data.IORef
import System.IO
import Data.Default

main = do args <- getArgs
          case args of
            [file] -> do f <- readFile file
                         let p = readMarkdown def f
                         process p
            _ -> error usage

usage = "usage: extract <file>"

process :: Pandoc -> IO ()
process p = do
  s <- newIORef S.empty
  m <- newIORef M.empty
  bottomUpM (extractCode s m) p
  t <- readIORef m
  M.foldrWithKey writeTemplate (return ()) t

writeTemplate templateF vars k
  = do template <- readFile templateF
       writeFile dest (renderTemplate vars template)
       k
  where
    (dest,_) = splitExtension templateF

extractCode :: IORef (S.Set String) -> IORef (M.Map String [(String,String)]) ->
               Block -> IO Block
extractCode r m b@(CodeBlock (id,cls,attrs) code)
  | Just fileName <- lookup "file" attrs =
    do s <- readIORef r
       if S.member fileName s
         then appendFile fileName (code ++ "\n")
         else do writeFile fileName (code ++ "\n")
                 writeIORef r (S.insert fileName s)
       return b
  | Just template <- lookup "template" attrs,
    Just var      <- lookup "var" attrs =
      do t <- readIORef m
         writeIORef m (M.insertWith (++) template [(var,code ++ "\n")] t)
         return b
extractCode r m b = return b
