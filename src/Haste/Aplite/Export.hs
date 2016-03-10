{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}
-- | Exporting functions from Aplite to Haskell.
module Haste.Aplite.Export where
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as CMD
import Language.JS.CompExp
import Language.JS.Expression hiding (Fun)
import Language.JS.Syntax
import Language.JS.Monad
import Haste (toJSString)
import qualified Haste.JSString as S

import Feldspar
import Feldspar.Frontend
import qualified Feldspar.Representation as FeldRep
import Data.VirtualContainer
import Feldspar.Run.Compile
import Feldspar.Run.Representation

data Fun = Fun
  { cgStartId    :: Int
  , expFunParams :: [Param]
  , expFunBody   :: Run ()
  }

type family RetVal a where
  RetVal (CExp a) = IO a
  RetVal a        = IO a

class Export f where
  type ExportSig f
  mkFun :: Int -> [Param] -> f -> Fun

instance (SmallType a, Export b) => Export (Data a -> b) where
  type ExportSig (Data a -> b) = a -> ExportSig b
  mkFun n as f = mkFun (succ n) (param t (MkId n') : as) (f argexp)
    where n' = S.cons 'n' (toJSString n)
          t = jsType (undefined :: CExp a)
          argexp = varExp n'

instance (SmallType a, Export b) => Export (Arr a -> b) where
  type ExportSig (Arr e -> b) = IArr e -> ExportSig b
  mkFun n as f = mkFun (succ n) (param t (MkId name) : as) (f argexp)
    where t = Arr (jsType (undefined :: CExp a))
          name = S.cons 'a' (toJSString n)
          argexp = FeldRep.Arr $ Actual $ CMD.ArrComp name

instance Return a => Export (Run a) where
  type ExportSig (Run a) = RetVal a
  mkFun n as body = Fun
      { cgStartId    = n
      , expFunParams = reverse as
      , expFunBody   = (body >>= return_)
      }
