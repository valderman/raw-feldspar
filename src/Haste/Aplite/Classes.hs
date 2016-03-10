{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
-- | Type classes generalising Ord, Eq, etc. across standard Haskell and Aplite.
module Haste.Aplite.Classes where
import qualified Prelude as P
import qualified Feldspar as F
import qualified Feldspar.Representation as FR
import qualified Data.Bits as B
import Language.Syntactic.TypeRep (sugarSymTR)
import Feldspar (SmallType, Int32, Word32, Data)
import Prelude ( (.), ($), undefined, fromIntegral, Bool, Double, Ordering
               , Integral
               )

type family Result (a :: *) (r :: *) where
  Result (f a) r = f (LiftedResult f r)
  Result a r     = r

type family LiftedResult (f :: * -> *) (r :: *)

type instance LiftedResult Data Ordering = Int32
type instance LiftedResult Data Int32    = Int32
type instance LiftedResult Data Word32   = Word32
type instance LiftedResult Data Bool     = Bool
type instance LiftedResult Data Double   = Double

-- | Generalised version of 'P.Eq'.
class GEq a where
  (==) :: a -> a -> Result a Bool
  (/=) :: a -> a -> Result a Bool

infixl 4 ==
infixl 4 /=

instance {-# OVERLAPPABLE #-} (P.Eq a, Result a Bool ~ Bool) => GEq a where
  (==) = (P.==)
  (/=) = (P./=)

instance (SmallType a, P.Ord a) => GEq (Data a) where
  (==) = (F.==)
  (/=) = (F./=)

-- | Generalised version of 'P.Ord'.
--   TODO: min and max.
class GEq a => GOrd a where
  (<)     :: a -> a -> Result a Bool
  (<=)    :: a -> a -> Result a Bool
  (>)     :: a -> a -> Result a Bool
  (>=)    :: a -> a -> Result a Bool

infixl 4 <
infixl 4 <=
infixl 4 >
infixl 4 >=

instance {-# OVERLAPPABLE #-} (P.Ord a, Result a Bool ~ Bool) => GOrd a where
  (<)     = (P.<)
  (<=)    = (P.<=)
  (>)     = (P.>)
  (>=)    = (P.>=)

instance (SmallType a, P.Ord a, GEq (Data a)) => GOrd (Data a) where
  (<)     = (F.<)
  (<=)    = (F.<=)
  (>)     = (F.>)
  (>=)    = (F.>=)

class GBits a where
  (.&.)    :: a -> a -> a
  (.|.)    :: a -> a -> a
  xor      :: a -> a -> a
  shiftL   :: a -> Result a Word32 -> a
  shiftR   :: a -> Result a Word32 -> a
  bitSize  :: a -> Result a Word32
  isSigned :: a -> Result a Bool
  testBit  :: a -> Result a Word32 -> Result a Bool
  bit      :: Result a Word32 -> a

infixl 5 .|.
infixl 6 `xor`
infixl 7 .&.
infixl 8 `shiftL`
infixl 8 `shiftR`

instance {-# OVERLAPPABLE #-} ( Integral (Result a Word32)
                              , Result a Bool ~ Bool
                              , Result a Word32 ~ Word32
                              , B.Bits a
                              ) => GBits a where
  (.&.)     = (B..&.)
  (.|.)     = (B..|.)
  xor       = B.xor
  shiftL x  = B.shiftL x . fromIntegral
  shiftR x  = B.shiftR x . fromIntegral
  bitSize   = fromIntegral . B.bitSize
  isSigned  = B.isSigned
  testBit x = B.testBit x . fromIntegral
  bit       = B.bit . fromIntegral

instance (F.SmallType a, P.Ord a, P.Num a, B.Bits a) => GBits (F.Data a) where
  (.&.)       = sugarSymTR FR.BitAnd
  (.|.)       = sugarSymTR FR.BitOr
  xor         = sugarSymTR FR.BitXor
  shiftL      = sugarSymTR FR.BitShl
  shiftR      = sugarSymTR FR.BitShr
  bitSize _   = 32
  isSigned _  = if B.isSigned (undefined :: a) then F.true else F.false
  testBit x b = (x `shiftR` b) == 1
  bit b       = 1 `shiftL` b
