{-# LANGUAGE DataKinds, GADTs #-}
module Synth.Index where

import Synth.Shape

data I a where
  I1 :: I 'S1
  IL :: I i1 -> I ('SB i1 i2)
  IR :: I i2 -> I ('SB i1 i2)
