{-# LANGUAGE DataKinds, DeriveTraversable, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, StandaloneDeriving #-}
module Synth.Vector where

import Synth.Shape
import Synth.Tree

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

instance Semigroup a => Monoid (V s a) where
  mempty = V0
  mappend = (<>)

instance Applicative (V 'S0) where
  pure _ = V0

  _ <*> _ = V0

instance Applicative (V 'S1) where
  pure = V1

  V0   <*> _ = V0
  V1 f <*> a = fmap f a

instance (Applicative (V s1), Applicative (V s2)) => Applicative (V ('SB s1 s2)) where
  pure a = VB (pure a) (pure a)

  V0       <*> _        = V0
  _        <*> V0       = V0
  VB f1 f2 <*> VB a1 a2 = VB (f1 <*> a1) (f2 <*> a2)


data SomeV a where
  SomeV :: V s a -> SomeV a

deriving instance Show a => Show (SomeV a)
deriving instance Foldable SomeV
deriving instance Functor SomeV
deriving instance Traversable SomeV

fromB :: B a -> SomeV a
fromB B0         = SomeV V0
fromB (B1 x)     = SomeV (V1 x)
fromB (BB x1 x2) = case (fromB x1, fromB x2) of
  (SomeV v1, SomeV v2) -> SomeV (VB v1 v2)

withVector :: (forall s . V s a -> b) -> SomeV a -> b
withVector f (SomeV v) = f v
