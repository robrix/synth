{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Synth.Index where

import Synth.Shape

data I s where
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

instance Enum (I 'S1) where
  toEnum 1 = I1
  toEnum i = error $ "toEnum: (" ++ show i ++ ") out of bounds (" ++ show I1 ++ ")"

  fromEnum _ = 1

instance (Enum (I s1), Enum (I s2), Shaped s1) => Enum (I ('SB s1 s2)) where
  toEnum i = s
    where m = shape (lproxy s)
          s | i > 0, i <= smax m = IL (toEnum i)
            | otherwise          = IR (toEnum (i - smax m))

  fromEnum   (IL i) =          fromEnum i
  fromEnum s@(IR i) = smax m + fromEnum i
    where m = shape (lproxy s)

instance Num (I 'S1) where
  fromInteger _ = I1

  _ + _ = I1
  _ - _ = I1
  _ * _ = I1

  abs    _ = I1
  signum _ = I1
  negate _ = I1
