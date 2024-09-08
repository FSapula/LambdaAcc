{-# LANGUAGE DataKinds #-}

module Lib (
  someFunc,
) where

import Lambdas
import Parser
import Text.ParserCombinators.Parsec

someFunc :: IO ()
someFunc = do
  let true = LAbs "x" (LAbs "y" (LVar "x"))
  let false = LAbs "x" (LAbs "y" (LVar "y"))
  let and = LAbs "p" (LAbs "q" (App (App (LVar "p") (LVar "q")) (LVar "p")))
  let tandf = App (App and true) false
  let id = LAbs "x" (LVar "x")
  let constl = LAbs "z" (LAbs "y" (LVar "z"))
  let idofconstl = App id constl
  let iszero = LAbs "n" $ App (App (LVar "n") (LAbs "x" false)) true
  print and
  print true
  print false
  print $ App (App and true) false
  print $ apply $ App (App and true) false
  print $ apply $ apply $ App (App and true) false
  print $ apply $ etaConversion $ App true false
  print $ apply $ App (App true false) false
  print $ App id constl
  print $ apply $ App id constl
  print $ App constl id
  print $ apply $ App constl id
  print $ apply $ App (App id constl) (LAbs "a" (LAbs "b" (LVar "a")))
  testFunc
