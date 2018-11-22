{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}
module Synth.Matrix where

import Synth.Shape

data M sx sy a where
  M0 :: M sx sy a
  M1 :: a -> M 'S1 'S1 a
  MR :: M sx1 'S1 a -> M sx2 'S1 a -> M ('SB sx1 sx2) 'S1 a
  MC :: M 'S1 sy1 a
     -> M 'S1 sy2 a
     -> M 'S1 ('SB sy1 sy2) a
  MQ :: M sx1 sy1 a -> M sx2 sy1 a
     -> M sx1 sy2 a -> M sx2 sy2 a
     -> M ('SB sx1 sx2) ('SB sy1 sy2) a

deriving instance Eq   a => Eq   (M sx sy a)
