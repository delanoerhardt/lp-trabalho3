module Root.Questoes.Q4.Interpreter where

import Root.Questoes.Q4.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]

type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar (conforme abaixo),
   mas a sua definicao (corpo) pode continuar a mesma dos exercícios anteriores
-}
executeP :: RContext -> Program -> Either ErrorMessage RContext
-- executeP context (Prog stm) = execute context stm
executeP context (Prog stm) = execute context stm

{- Dica: o tipo de execute deve mudar para
 execute :: RContext -> Stm -> Either ErrorMessage RContext
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos
 serao afetados
-}
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
      if condVal /= 0
        then case execute context stm of
          Left err -> Left err
          Right newContext -> execute newContext (SWhile exp stm)
        else Right context
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

-- Dica: insira aqui o tratamento do STry.

{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
  EAdd exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (v1 + v2)
  ESub exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (v1 - v2)
  EMul exp0 exp -> case eval context exp0 of
    Left err -> Left err
    Right v1 -> case eval context exp of
      Left err -> Left err
      Right v2 -> Right (v1 * v2)
  EDiv exp0 exp -> case eval context exp of
    Left err -> Left err
    Right 0 -> Left "divisao por 0"
    Right n -> case eval context exp0 of
      Left err -> Left err
      Right n1 -> Right (n1 `div` n)
  EInt n -> Right n
  EVar id -> Right (lookup context (getStr id))

{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of
                    Right ve1 -> case eval context e2 of
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: "
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg
                    Left msg -> Left msg
    EInt n  ->  Right n
-}

-- Dica: voce nao precisa mudar o codigo a partir daqui

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
