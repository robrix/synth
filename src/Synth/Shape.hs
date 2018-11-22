module Synth.Shape where

data S = S0 | S1 | SB S S
  deriving (Eq, Ord, Show)
