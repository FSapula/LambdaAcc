module Lambdas (
  Var,
  LExp (..),
  freeVar,
  churchEncoding,
  substitute,
  lambdaPLUS,
  betaReduce,
  LConst,
  apply,
  etaConversion,
  LProgram (..),
) where

import qualified Data.HashMap.Strict as HM

type Var = String
data LExp = LAbs Var LExp | App LExp LExp | LVar Var deriving (Eq)
type LConst = (String, LExp)
data LProgram = LProgram
  { consts :: [LConst]
  , exps :: [LExp]
  }

instance Show LExp where
  show (LAbs v (LAbs z expr)) = "λ" ++ v ++ "." ++ "λ" ++ z ++ "." ++ show expr
  show (LAbs v expr) = "λ" ++ v ++ ".(" ++ show expr ++ ")"
  show (App exp1 exp2) = "(" ++ show exp1 ++ " " ++ show exp2 ++ ")"
  show (LVar var) = var

freeVar :: LExp -> [Var]
freeVar (LVar x) = [x]
freeVar (LAbs x m) = filter (/= x) $ freeVar m
freeVar (App n m) = freeVar n ++ freeVar m

churchEncoding :: Int -> LExp
churchEncoding n = LAbs "f" $ LAbs "x" (encodeBody n)
 where
  encodeBody 0 = LVar "x"
  encodeBody p = App (LVar "f") (encodeBody (p - 1))

lambdaPLUS :: LExp
lambdaPLUS = LAbs "m" $ LAbs "n" $ LAbs "f" $ LAbs "x" $ App (LVar "m") $ App (LVar "f") $ App (App (LVar "n") (LVar "f")) (LVar "x")

substitute :: LExp -> Var -> LExp -> LExp
substitute (LVar y) x n
  | x == y = n
  | otherwise = (LVar y)
substitute (App m1 m2) x n = App (substitute m1 x n) (substitute m2 x n)
substitute (LAbs y m) x n
  | x == y = LAbs x n
  | x /= y && (y `notElem` freeVar n) = LAbs y (substitute m x n)

betaReduce :: LExp -> LExp -> LExp
betaReduce (LAbs x m) n = substitute m x n

apply :: LExp -> LExp
apply (App (LAbs x m) n) = betaReduce (LAbs x m) n
apply (App m n) = apply (App (apply m) n)

etaConversion :: LExp -> LExp
etaConversion (App (LAbs x e) (LVar y)) | y `notElem` freeVar e = e
etaConversion a = a
