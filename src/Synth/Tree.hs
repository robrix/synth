{-# LANGUAGE DeriveTraversable #-}
module Synth.Tree where

data B a = B0 | B1 a | BB (B a) (B a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
