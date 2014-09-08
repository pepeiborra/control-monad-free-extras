
module Control.Monad.Free.Extras where

import Control.DeepSeq
import Control.DeepSeq.Extras
import Control.Monad.Free
import Debug.Hoed.Observe
import Prelude.Extras

instance NFData1 f => NFData1 (Free f) where rnf1 = rnf

instance (NFData a, NFData1 f) => NFData (Free f a) where
  rnf (Pure a) = rnf a
  rnf (Impure fa) = rnf1 fa

instance Observable1 f => Observable1 (Free f) where
  observer1 (Pure t)   = Pure . observer t
  observer1 (Impure t) = Impure . lower1 . observer (Lift1 t)

instance (Observable a, Observable1 f) => Observable(Free f a) where
  observer  = observer1
  observers = observers1
