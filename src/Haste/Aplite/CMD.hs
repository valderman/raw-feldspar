-- | Arrays and references for Aplite.
module Haste.Aplite.CMD
  ( module Feldspar
  , initRef, getRef, setRef, modifyRef, unsafeFreezeRef
  , getArr, setArr
  ) where
import Feldspar hiding
  ( initRef, modifyRef, getRef, setRef, unsafeFreezeRef
  , getArr, setArr
  , (<), (>), (<=), (>=), (==), (/=)
  )
import qualified Feldspar.Frontend as F
import Language.JS.Expression (JSType)

-------------------
-- * References
-------------------

-- | Create an initialized named reference
initRef :: (Syntax (Data a), MonadComp m) => Data a -> m (Ref a)
initRef = F.initRef

-- | Get the contents of a reference.
getRef :: (Syntax (Data a), MonadComp m) => Ref a -> m (Data a)
getRef = F.getRef

-- | Set the contents of a reference.
setRef :: (Syntax (Data a), MonadComp m) => Ref a -> Data a -> m ()
setRef = F.setRef

-- | A version of 'modifyRef' that fixes the value type to @`Data` a@
modifyRef :: (SmallType a, MonadComp m) => Ref a -> (Data a -> Data a) -> m ()
modifyRef = F.modifyRef

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef :: (JSType a, Syntax (Data a), MonadComp m)
                => Ref a -> m (Data a)
unsafeFreezeRef = F.unsafeFreezeRef


---------------
-- * Arrays  
---------------

-- | Set an element of an array
setArr :: forall m a. (JSType a, Syntax (Data a), MonadComp m)
       => Data Index -> Data a -> Arr a -> m ()
setArr = F.setArr

-- | Get an element of an array
getArr :: (JSType a, Syntax (Data a), MonadComp m)
       => Data Index -> Arr a -> m (Data a)
getArr = F.getArr
