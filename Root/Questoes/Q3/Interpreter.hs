module Root.Questoes.Q3.Interpreter where

import Root.Questoes.Q3.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]

type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp ->
    case eval context exp of
      Left err -> Left err
      Right val -> Right (update context (getStr id) (val))
  SBlock [] -> Right context
  SBlock (s : stms) ->
    case execute context s of
      Left err -> Left err
      Right newContext -> execute newContext (SBlock stms)
  SWhile exp stm ->
    case eval context exp of
      Left err -> Left err
      Right condVal ->
        if condVal /= 0
          then case execute context stm of
            Left err -> Left err
            Right newContext -> execute newContext (SWhile exp stm)
          else Right context
          
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
  EAdd exp0 exp ->
    case eval context exp0 of
      Left err -> Left err
      Right v1 ->
        case eval context exp of
          Left err -> Left err
          Right v2 -> Right (v1 + v2)
  ESub exp0 exp ->
    case eval context exp0 of
      Left err -> Left err
      Right v1 ->
        case eval context exp of
          Left err -> Left err
          Right v2 -> Right (v1 - v2)
  EMul exp0 exp ->
    case eval context exp0 of
      Left err -> Left err
      Right v1 ->
        case eval context exp of
          Left err -> Left err
          Right v2 -> Right (v1 * v2)
  EDiv exp0 exp ->
    case eval context exp of
      Left err -> Left err
      Right 0 -> Left "divisao por 0"
      Right n ->
        case eval context exp0 of
          Left err -> Left err
          Right n1 -> Right (n1 `div` n)
  EInt n -> Right n
  EVar id -> Right (lookup context (getStr id))

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s
lookup [] _ = error "Variable not found"

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv
