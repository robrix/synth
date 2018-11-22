{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Synth.Index where

import Synth.Shape

data I a where
  I1 :: I 'S1
  IL :: I i1 -> I ('SB i1 i2)
  IR :: I i2 -> I ('SB i1 i2)

deriving instance Eq (I a)

instance Ord (I s) where
  compare I1      I1      = EQ
  compare (IL i1) (IL i2) = compare i1 i2
  compare (IL _)  _       = LT
  compare _       (IL _)  = GT
  compare (IR i1) (IR i2) = compare i1 i2

deriving instance Show (I a)

instance Bounded (I 'S1) where
  minBound = I1
  maxBound = I1
