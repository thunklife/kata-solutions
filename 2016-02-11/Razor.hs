module Razor where

data Razor
  = Lit Int
  | Add Razor Razor


interpret :: Razor -> Int
interpret (Lit n)   = n
interpret (Add l r) = interpret l + interpret r

prettyPrint :: Razor -> String
prettyPrint (Lit n)   = show n
prettyPrint (Add l r) = "(" ++ prettyPrint l ++ "+" ++ prettyPrint r ++ ")"
