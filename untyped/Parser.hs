module Parser where

import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)

data Term =
    TmVar Info Int Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving (Show)

data Info = Info { row :: Int, col :: Int } deriving (Show)

type BoundContext = [String]
type LCParser = Parsec String BoundContext Term

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

findVar :: String -> BoundContext -> LCParser
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseVar :: LCParser
parseVar = do
  v <- parseVarName
  list <- getState
  findVar v list

parseAbs :: LCParser
parseAbs = do
  char '\\' <|> char 'λ'
  v <- parseVarName
  modifyState (v :)
  char '.'
  term <- parseTerm
  modifyState tail
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v term

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

parseNonApp :: LCParser
parseNonApp = parens parseTerm -- (M)
           <|> parseAbs        -- λx.M
           <|> parseVar        -- x

parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
  space
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p s = runParser p [] "untyped λ-calculus" s

parse :: String -> Either ParseError Term
parse s = parseWith parseTerm s