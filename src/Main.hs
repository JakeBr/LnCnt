module Main where

import System.Environment(getArgs)
import Data.Char(isSpace)

main :: IO ()
main = fmap evalArgs getArgs >>= out

evalArgs :: [String] -> Maybe String
evalArgs [path] = Just path
evalArgs _      = Nothing

out :: Maybe String -> IO ()
out Nothing = putStrLn usage
out (Just path) = do
  x <- readFile path
  putStrLn $
    show (ls x) ++ " lines total\n" ++
    show (cd x) ++ " lines of code\n" ++
    show (ct x) ++ " lines consisting only of comments\n" ++
    show (ws x) ++ " lines consisting only of whitespace\n"
  where
    ls   = length . lines
    ct   = length . filter isCommentLine . lines
    ws   = length . filter isWhitespaceLine . lines
    cd x = ls x - ct x - ws x

    isCommentLine []      = False
    isCommentLine (_:[])  = False
    isCommentLine (x:y:_) = x == '-' && y == '-'

    isWhitespaceLine = all isSpace

usage :: String
usage =
  "Usage:\nlnCnt <filepath>"
