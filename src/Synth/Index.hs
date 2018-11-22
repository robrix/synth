{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Synth.Index where

import Synth.Shape

data I a where
  I1 :: I 'S1
  IL :: I s1 -> I ('SB s1 s2)
  IR :: I s2 -> I ('SB s1 s2)

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

instance (Bounded (I s1), Bounded (I s2)) => Bounded (I ('SB s1 s2)) where
  minBound = IL minBound
  maxBound = IR maxBound
