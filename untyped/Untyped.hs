import Parser (Term(..), Info, parse)

data Binding = NameBind deriving (Show)

type Context = [(String, Binding)]

ctxLength :: Context -> Int
ctxLength = length

indexToName :: Context -> Int -> String
indexToName ctx n = fst $ ctx !! n

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | x `elem` (map fst ctx) = pickFreshName ctx $ x ++ "'"
  | otherwise = ((x, NameBind) : ctx , x)

printTm :: Context -> Term -> String
printTm ctx t = case t of
  TmAbs _ x t1 -> let
      (ctx', x') = pickFreshName ctx x
    in "(\\" ++ x' ++ "." ++ (printTm ctx' t1) ++ ")"
  TmApp _ t1 t2 ->
    "(" ++ (printTm ctx t1) ++ " " ++ printTm ctx t2 ++ ")"
  TmVar _ x n ->
    if ctxLength ctx == n then
      indexToName ctx x
    else
      "[bad index]"

termShift d t = walk 0 t
  where
    walk c t = case t of
      TmVar fi x n -> if x >= c then
          TmVar fi (x + d) (n + d)
        else
          TmVar fi x (n + d)
      TmAbs fi x t1  -> TmAbs fi x $ walk (c + 1) t1
      TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where
    walk c t = case t of
      TmVar fi x n -> if x == j + c then
          termShift c s
        else
          TmVar fi x n
      TmAbs fi x t1  -> TmAbs fi x $ walk (c + 1) t1
      TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)

j +-> s = termSubst j s

termSubstTop s t = termShift (-1) $ (0 +-> termShift 1 s) t

isVal :: Context -> Term -> Bool
isVal _ t = case t of
  TmAbs _ _ _ -> True
  _ -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  TmApp _ (TmAbs _ x t12) v2 | isVal ctx v2 -> Just $ termSubstTop v2 t12
  TmApp fi v1 t2 | isVal ctx v1 -> do
    t2' <- eval1 ctx t2
    return $ TmApp fi v1 t2'
  TmApp fi t1 t2 -> do
    t1' <-  eval1 ctx t1
    return $ TmApp fi t1' t2
  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Nothing -> t
  Just t' -> eval ctx t'

pretty s = case parse s of
  Left err -> print err
  Right t -> print $ printTm [] t

simplify :: String -> IO ()
simplify s = case parse s of
  Left err -> print err
  Right t  -> print $ printTm [] $ eval [] t