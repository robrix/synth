{-# LANGUAGE DataKinds, DeriveTraversable, FlexibleContexts, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Synth.Matrix where

import Synth.Shape
import Synth.Vector

data M sx sy a where
  M0 :: M sx sy a
  M1 :: a -> M 'S1 'S1 a
  MR :: M sx1 'S1 a -> M sx2 'S1 a -> M ('SB sx1 sx2) 'S1 a
  MC :: M 'S1 sy1 a
     -> M 'S1 sy2 a
     -> M 'S1 ('SB sy1 sy2) a
  MQ :: M sx1 sy1 a -> M sx2 sy1 a
     -> M sx1 sy2 a -> M sx2 sy2 a
     -> M ('SB sx1 sx2) ('SB sy1 sy2) a

deriving instance Eq   a => Eq   (M sx sy a)

instance Ord a => Ord (M sx sy a) where
  compare M0               M0               = EQ
  compare M0               _                = LT
  compare _                M0               = GT
  compare (M1 a1)          (M1 a2)          = compare a1 a2
  compare (MR a1 b1)       (MR a2 b2)       = compare a1 a2 <> compare b1 b2
  compare (MC a1 b1)       (MC a2 b2)       = compare a1 a2 <> compare b1 b2
  compare (MQ a1 b1 c1 d1) (MQ a2 b2 c2 d2) = compare a1 a2 <> compare b1 b2 <> compare c1 c2 <> compare d1 d2

deriving instance Show a => Show (M sx sy a)
deriving instance Foldable (M sx sy)
deriving instance Functor (M sx sy)
deriving instance Traversable (M sx sy)

instance Semigroup a => Semigroup (M x y a) where
  M0             <> a2             = a2
  a1             <> M0             = a1
  M1 a1          <> M1 a2          = M1 (a1 <> a2)
  MR a1 b1       <> MR a2 b2       = MR (a1 <> a2) (b1 <> b2)
  MC a1 b1       <> MC a2 b2       = MC (a1 <> a2) (b1 <> b2)
  MQ a1 b1 c1 d1 <> MQ a2 b2 c2 d2 = MQ (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Semigroup a => Monoid (M x y a) where
  mempty = M0
  mappend = (<>)

instance Applicative (M 'S0 'S0) where
  pure _ = M0

  _ <*> _ = M0

instance Applicative (M 'S1 'S1) where
  pure = M1

  M0   <*> _ = M0
  M1 f <*> a = fmap f a

instance (Applicative (M sx1 'S1), Applicative (M sx2 'S1)) => Applicative (M ('SB sx1 sx2) 'S1) where
  pure a = MR (pure a) (pure a)

  M0       <*> _        = M0
  _        <*> M0       = M0
  MR f1 f2 <*> MR a1 a2 = MR (f1 <*> a1) (f2 <*> a2)

instance (Applicative (M 'S1 sy1), Applicative (M 'S1 sy2)) => Applicative (M 'S1 ('SB sy1 sy2)) where
  pure a = MC (pure a) (pure a)

  M0       <*> _        = M0
  _        <*> M0       = M0
  MC f1 f2 <*> MC a1 a2 = MC (f1 <*> a1) (f2 <*> a2)

instance (Applicative (M sx1 sy1), Applicative (M sx2 sy1), Applicative (M sx1 sy2), Applicative (M sx2 sy2)) => Applicative (M ('SB sx1 sx2) ('SB sy1 sy2)) where
  pure a = MQ (pure a) (pure a) (pure a) (pure a)

  M0             <*> _              = M0
  _              <*> M0             = M0
  MQ f1 f2 f3 f4 <*> MQ a1 a2 a3 a4 = MQ (f1 <*> a1) (f2 <*> a2) (f3 <*> a3) (f4 <*> a4)


data SomeM a where
  SomeM :: M sx sy a -> SomeM a

deriving instance Show a => Show (SomeM a)
deriving instance Foldable SomeM
deriving instance Functor SomeM
deriving instance Traversable SomeM

fromRow :: V s a -> M s 'S1 a
fromRow V0         = M0
fromRow (V1 x)     = M1 x
fromRow (VB x1 x2) = MR (fromRow x1) (fromRow x2)
