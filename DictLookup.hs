{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

import Text.Parsec
import qualified Data.Map as M
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)

data Word = ReplaceableWord String | NonReplaceableWord String
            deriving (Ord, Eq, Show)

-- Lexer

wordToken = many1 alphaNum

replaceableWord = do
  char '$'
  s <- wordToken
  char '$'
  return $ ReplaceableWord s

nonReplaceableWord = do
  s <- wordToken
  return $ NonReplaceableWord s

templateText = (replaceableWord <|> nonReplaceableWord) `sepBy` spaces

lookupWord :: M.Map Word String -> Word -> Either String String
lookupWord _ (NonReplaceableWord w) = Right w
lookupWord dict (ReplaceableWord w) =
    case M.lookup (ReplaceableWord w) dict of
      Nothing -> Left w
      Just wd -> Right wd

-- Parser

evaluateTemplate :: M.Map Word String -> [Word] -> Either [String] [String]
evaluateTemplate dict words =
    if null (fst searched) then
        Right (snd searched)
    else
        Left (fst searched)

    where searched = partitionEithers $ map (lookupWord dict) words

-- Input dictionary

dictionary :: M.Map Word String
dictionary = M.fromList [ (ReplaceableWord "foo", "bar")
                        , (ReplaceableWord "baz", "buzz") ]

-- Driver function

main = do
  toTranslate <- getLine

  case parse templateText "" toTranslate of
    Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
    Right res ->
        case evaluateTemplate dictionary res of
          Left errs -> hPutStrLn stderr $ "Token(s) not found: " ++ show errs
          Right r -> putStrLn $ "Parse success: " ++ (intercalate " " r)
