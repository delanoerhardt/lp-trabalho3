module Root.Questoes.Q5.Interpreter where

import Root.Questoes.Q5.AbsLI

type RContext = [(String, Valor)]

type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp -> case eval context exp of
    Left err -> Left err
    Right val -> Right (update context (getStr id) (val))
  SBlock [] -> Right context
  SBlock (s : stms) -> case execute context s of
    Left err -> Left err
    Right newContext -> execute newContext (SBlock stms)
  SWhile exp stm -> case eval context exp of
    Left err -> Left err
    Right condVal ->
      if condVal /= ValorInt 0
        then case execute context stm of
          Left err -> Left err
          Right newContext -> execute newContext (SWhile exp stm)
        else Right context
  SdoWhile stm exp -> case execute context stm of
    Left err -> Left err
    Right newContext -> execute newContext (SWhile exp stm)
  STry try catch finally -> case foldl executeIfRight (Right context) try of
    Left context1 -> case execute context1 (SBlock catch) of
      Left err -> Left err
      Right context2 -> execute context2 (SBlock finally)
    Right context1 -> execute context1 (SBlock finally)
    where
      executeIfRight (Right context) stmt = case execute context stmt of
        Left _ -> Left context
        Right a -> Right a
      executeIfRight (Left context) _ = Left context

eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
  EAdd exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (ValorInt (i v1 + i v2))
  ESub exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (ValorInt (i v1 - i v2))
  EMul exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (ValorInt (i v1 * i v2))
  EDiv exp0 exp -> case eval context exp of
    Left err -> Left err
    Right (ValorInt 0) -> Left "divisao por 0"
    Right n -> case eval context exp0 of
      Left err -> Left err
      Right n1 -> Right (ValorInt (i n1 `div` i n))
  ECon exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (ValorStr (s v1 ++ s v2))
  EInt n -> Right (ValorInt n)
  EVar id -> Right (lookup2 context (getStr id))
  EStr str -> Right (ValorStr str)
  EOr exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (ValorBool (b v1 || b v2))
  EAnd exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (ValorBool (b v1 && b v2))
  ENot exp -> case eval context exp of
    Left err -> Left err
    Right v1 -> Right (ValorBool (not (b v1)))
  ETrue -> Right (ValorBool True)
  EFalse -> Right (ValorBool False)

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

s :: Valor -> String
s (ValorStr str) = str
s _ = error "invalid Valor"

i :: Valor -> Integer
i (ValorInt vint) = vint
i _ = error "invalid Valor"

b :: Valor -> Bool
b (ValorBool vbool) = vbool
b _ = error "invalid Valor"

getStr :: Ident -> String
getStr (Ident s) = s

-- Essa função estava dando conflito com importação do Prelude, então troquei o nome para evitar problemas
lookup2 :: RContext -> String -> Valor
lookup2 ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup2 cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv