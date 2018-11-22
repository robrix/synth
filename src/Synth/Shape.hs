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
