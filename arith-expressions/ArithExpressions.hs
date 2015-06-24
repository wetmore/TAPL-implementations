data Term =
    TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  | TmZero Info
  | TmSucc Info Term
  | TmPred Info Term
  | TmIsZero Info Term
  deriving (Show)

type Info = Maybe Int

isNumericVal :: Term -> Bool
isNumericVal t = case t of
  TmZero _    -> True
  TmSucc _ t1 -> isNumericVal t1
  _           -> False

isVal :: Term -> Bool
isVal t = case t of
  TmTrue _  -> True
  TmFalse _ -> True
  _         -> isNumericVal t

eval1 :: Term -> Maybe Term
eval1 t = case t of
  TmIf _ (TmTrue _) t1 _  -> Just t1
  TmIf _ (TmFalse _) _ t1 -> Just t1
  TmIf fi t1 t2 t3 -> do
    t1' <- eval1 t1
    return $ TmIf fi t1' t2 t3
  TmSucc fi t1 -> do
    t1' <- eval1 t1
    return $ TmSucc fi t1'
  TmPred _ (TmZero _) -> Just $ TmZero Nothing 
  TmPred _ (TmSucc _ nv) | isNumericVal nv -> Just nv
  TmPred fi t1 -> do
    t1' <- eval1 t1
    return $ TmPred fi t1'
  TmIsZero _ (TmZero _) -> Just $ TmTrue Nothing 
  TmIsZero _ (TmSucc _ nv) | isNumericVal nv -> Just $ TmFalse Nothing 
  TmIsZero fi t1 -> do
    t1' <- eval1 t1
    return $ TmIsZero fi t1'
  _ -> Nothing

eval :: Term -> Term
eval t = case eval1 t of
  Nothing -> t
  Just t' -> eval t'