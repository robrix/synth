{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Control.Effect.Time where

data Time (m :: * -> *) k
  = Time (Double -> k)
  deriving (Functor)
