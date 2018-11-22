{-# LANGUAGE DataKinds, GADTs #-}
module Synth.Vector where

import Synth.Shape

data V s a where
  V0 :: V s a
  V1 :: a -> V 'S1 a
  VB :: V s1 a -> V s2 a -> V ('SB s1 s2) a
