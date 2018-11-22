{-# LANGUAGE DataKinds, DeriveTraversable, GADTs, StandaloneDeriving #-}
module Synth.Vector where

import Synth.Shape

data V s a where
  V0 :: V s a
  V1 :: a -> V 'S1 a
  VB :: V s1 a -> V s2 a -> V ('SB s1 s2) a

deriving instance Eq   a => Eq   (V s a)
deriving instance Ord  a => Ord  (V s a)
deriving instance Show a => Show (V s a)
deriving instance Foldable (V s)
deriving instance Functor (V s)
deriving instance Traversable (V s)
