{-# LANGUAGE DeriveFunctor, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Time where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.IO.Class
import Data.Coerce
import Data.Time.Clock.System

data Time (m :: * -> *) k
  = Time (Double -> k)
  deriving (Functor)

instance HFunctor Time where
  hmap _ = coerce

instance Effect Time where
  handle state handler = coerce . fmap (handler . (<$ state))


runTime :: (Carrier sig m, MonadIO m) => Eff (TimeC m) a -> m a
runTime = runTimeC . interpret

newtype TimeC m a = TimeC { runTimeC :: m a }

instance (Carrier sig m, MonadIO m) => Carrier (Time :+: sig) (TimeC m) where
  ret = TimeC . ret
  eff = TimeC . handleSum (eff . handleCoercible) (\ (Time k) -> liftIO getSystemTime >>= runTimeC . k . systemToFracTime)

systemToFracTime :: Fractional a => SystemTime -> a
systemToFracTime (MkSystemTime s ns) = (fromIntegral s) + (fromIntegral ns) * 1E-9
