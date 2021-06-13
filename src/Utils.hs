module Utils where

import Model

mkString :: [String] -> String
mkString xs = foldr (\l r -> l ++ ", " ++ r) "" xs
