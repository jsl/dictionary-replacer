{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

import Text.Parsec
import qualified Data.Map as M
import Data.Either (partitionEithers)

data Word = ReplaceableWord String | NonReplaceableWord String
            deriving (Ord, Eq, Show)

wordToken = many1 alphaNum

replaceableTokenMarker = char '$'

replaceableWord = do
  replaceableTokenMarker
  s <- wordToken
  replaceableTokenMarker
  return $ ReplaceableWord s

nonReplaceableWord = do
  s <- wordToken
  return $ NonReplaceableWord s

templateText = (replaceableWord <|> nonReplaceableWord) `sepBy` spaces

evaluateTemplate :: M.Map Word String -> [Word] -> [String]
evaluateTemplate dict words =
    map (\w -> case w of
                 ReplaceableWord wd    -> (M.!) dict (ReplaceableWord wd)
                 NonReplaceableWord wd -> wd ) words

dictionary :: M.Map Word String
dictionary = M.fromList [ (ReplaceableWord "foo", "bar")
                        , (ReplaceableWord "baz", "buzz") ]

main = do
  toTranslate <- getLine

  case parse templateText "" toTranslate of
    Left err -> putStrLn $ show err
    Right res ->
        putStrLn $ unlines $ map show $ evaluateTemplate dictionary res
