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

instance Semigroup a => Semigroup (V s a) where
  V0       <> a2       = a2
  a1       <> V0       = a1
  V1 a1    <> V1 a2    = V1 (a1 <> a2)
  VB a1 b1 <> VB a2 b2 = VB (a1 <> a2) (b1 <> b2)
