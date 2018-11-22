module Synth.Tree where

data B a = B0 | B1 a | BB (B a) (B a)
