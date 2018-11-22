{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Control.Effect.Time where

import Control.Effect.Carrier
import Data.Coerce

data Time (m :: * -> *) k
  = Time (Double -> k)
  deriving (Functor)

instance HFunctor Time where
  hmap _ = coerce
