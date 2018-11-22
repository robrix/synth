{-# LANGUAGE DataKinds, KindSignatures #-}
module Synth.Shape where

data S = S0 | S1 | SB S S
  deriving (Eq, Ord, Show)

smax :: Num a => S -> a
smax S0         = 0
smax S1         = 1
smax (SB s1 s2) = smax s1 + smax s2


class Shaped (s :: S) where
  shape :: proxy s -> S

instance Shaped 'S0 where
  shape _ = S0

instance Shaped 'S1 where
  shape _ = S1

instance (Shaped s1, Shaped s2) => Shaped ('SB s1 s2) where
  shape p = SB (shape (l p)) (shape (r p))
    where l :: proxy ('SB s1 s2) -> proxy s1
          l _ = undefined
          r :: proxy ('SB s1 s2) -> proxy s2
          r _ = undefined
