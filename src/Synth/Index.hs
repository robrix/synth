{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Synth.Index where

import Data.Ix
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

instance Ix (I 'S1) where
  range (I1, I1) = [I1]

  index (I1, I1) I1 = 0

  inRange (I1, I1) I1 = True

instance (Enum (I s1), Enum (I s2), Shaped s1, Shaped s2) => Ix (I ('SB s1 s2)) where
  range (l, h) = [l..h]

  {-# INLINE index #-}
  index b@(l, _) i | inRange b i = fromEnum i - fromEnum l
                   | otherwise   = error $ "Ix{I ('" ++ show (shape i) ++ ")}.index: Index (" ++ show i ++ ") out of range " ++ show b

  inRange (l, h) i = l <= i && i <= h
