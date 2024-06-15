module Formula where

import Data.Maybe
import Data.List

type Id = String

data Formula = Var Id
             | Not Formula
             | And Formula Formula
             | Or  Formula Formula
             | Impl Formula Formula
             | BiCond Formula Formula
             deriving (Eq)

instance Show Formula where
  show (Var v) = v
  show (Not f) = '!' : show f
  show (And f f') = "(" ++ show f ++ " & " ++ show f' ++ ")"
  show (Or f f') = "(" ++ show f ++ " | " ++ show f' ++ ")"
  show (Impl f f') = "(" ++ show f ++ " -> " ++ show f' ++ ")"
  show (BiCond f f') = "(" ++ show f ++ " <-> " ++ show f' ++ ")"

-- all of the variables in a formula
vars :: Formula -> [Id]
vars
  = sort . nub . vars'
    where
      vars' (Var x) = [x]
      vars' (Not f) = vars' f
      vars' (And f f') = vars' f ++ vars' f'
      vars' (Or f f') = vars' f ++ vars' f'
      vars' (Impl f f') = vars' f ++ vars' f'
      vars' (BiCond f f') = vars' f ++ vars' f'

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key list = fromJust (lookup key list)

eval :: [(Id, Bool)] -> Formula -> Bool
eval idMap
  = eval'
    where
      eval' :: Formula -> Bool 
      eval' (Var x) = lookUp x idMap
      eval' (Not f) = (not . eval') f
      eval' (And f f') = eval' f && eval' f'
      eval' (Or f f') = eval' f || eval' f'
      eval' (Impl f f') = eval' (Not f) || eval' f'
      eval' (BiCond f f') = (eval' f && eval' f') || (eval' (Not f) && eval' (Not f'))


