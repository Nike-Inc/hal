module Control.Monad.Trans.Hal (
) where

import           Katip                          (Katip(..), KatipContext(..))

-- Take heed from: http://hackage.haskell.org/package/amazonka-1.6.1/docs/src/Network.AWS.html#AWS

newtype Hal r m a = Hal { unHal :: a }

instance Katip (Hal r) where
  getLogEnv = asks logEnv
  localLogEnv f = local (\s -> s { logEnv = f (logEnv s) })

instance KatipContext (Hal r) where
  getKatipContext = asks logContext
  localKatipContext f = local (\s -> s { logContext = f (logContext s)})
  getKatipNamespace = asks logNamespace
  localKatipNamespace f = local (\s -> s { logNamespace = f (logNamespace s)})
