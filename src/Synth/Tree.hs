{-# LANGUAGE DeriveTraversable #-}
module Synth.Tree where

data B a = B0 | B1 a | BB (B a) (B a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

fromList :: [a] -> B a
fromList []  = B0
fromList [x] = B1 x
fromList xs  = BB (fromList ys) (fromList zs)
  where (ys, zs) = splitAt (length xs `div` 2) xs
