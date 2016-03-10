{-# LANGUAGE OverloadedStrings #-}
module Feldspar.Frontend where



import Prelude (Integral, Floating (..), RealFrac, error, (=<<), sequence_)
import qualified Prelude
import Prelude.EDSL

import Data.Int

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Syntactic.TypeRep

import Language.Embedded.Imperative (IxRange)
import qualified Language.Embedded.Imperative as Imp

import qualified Data.Inhabited as Inhabited
import Data.VirtualContainer
import Feldspar.Representation
import Language.JS.Syntax (VarId)
import Language.JS.Expression (JSType)
import Data.Bits
import qualified Language.Embedded.Imperative.CMD as Imp
import qualified Language.Embedded.Expression as Imp

--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

----------------------------------------
-- ** General constructs
----------------------------------------

-- | Explicit sharing
share :: (Syntax a, Syntax b)
    => a         -- ^ Value to share
    -> (a -> b)  -- ^ Body in which to share the value
    -> b
share = shareTag ""

-- | Explicit tagged sharing
shareTag :: (Syntax a, Syntax b)
    => String
         -- ^ A tag (that may be empty). May be used by a back end to generate a sensible variable name.
    -> a         -- ^ Value to share
    -> (a -> b)  -- ^ Body in which to share the value
    -> b
shareTag tag = sugarSymTR (Let tag)

-- | For loop
forLoop :: Syntax st => Data Length -> st -> (Data Index -> st -> st) -> st
forLoop = sugarSymTR ForLoop

-- | Conditional expression
cond :: Syntax a
    => Data Bool  -- ^ Condition
    -> a          -- ^ True branch
    -> a          -- ^ False branch
    -> a
cond = sugarSymTR Condition

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: Syntax a
    => Data Bool  -- ^ Condition
    -> a          -- ^ True branch
    -> a          -- ^ False branch
    -> a
(?) = cond

infixl 1 ?

switch :: (Syntax a, Syntax b, SmallType (Internal a)) =>
    b -> [(Internal a, b)] -> a -> b
switch def [] _ = def
switch def cs s = Prelude.foldr
    (\(c,a) b -> value c == desugar s ? a $ b)
    def
    cs



----------------------------------------
-- ** Literals
----------------------------------------

-- | Literal
value :: Syntax a => Internal a -> a
value = sugarSymTR . Literal

false :: Data Bool
false = value False

true :: Data Bool
true = value True

instance Syntactic.Syntactic ()
  where
    type Domain ()   = FeldDomain
    type Internal () = Int32
    desugar () = unData 0
    sugar   _  = ()

-- | Example value
--
-- 'example' can be used similarly to 'undefined' in normal Haskell, i.e. to
-- create an expression whose value is irrelevant.
--
-- Note that it is generally not possible to use 'undefined' in Feldspar
-- expressions, as this will crash the compiler.
example :: Syntax a => a
example = value Inhabited.example



----------------------------------------
-- ** Primitive functions
----------------------------------------

instance (Num a, SmallType a) => Num (Data a)
  where
    fromInteger = value . fromInteger
    (+)         = sugarSymTR Add
    (-)         = sugarSymTR Sub
    (*)         = sugarSymTR Mul
    negate      = sugarSymTR Neg
    abs    = error "abs not yet defined for Data"
    signum = error "signum not yet defined for Data"

instance (Fractional a, SmallType a) => Fractional (Data a)
  where
    (/) = sugarSymTR FDiv
    fromRational = value . fromRational
    recip = error "recip not defined for (Data a)"

instance (Floating a, SmallType a) => Floating (Data a)
  where
    pi   = sugarSymTR Pi
    (**) = sugarSymTR Pow
    sin  = sugarSymTR Sin
    cos  = sugarSymTR Cos

quot :: (Integral a, SmallType a) => Data a -> Data a -> Data a
quot = sugarSymTR Quot

rem :: (Integral a, SmallType a) => Data a -> Data a -> Data a
rem = sugarSymTR Rem

-- | Simultaneous @quot@ and @rem@
quotRem :: (Integral a, SmallType a) => Data a -> Data a -> (Data a, Data a)
quotRem a b = (q,r)
  where
    q = quot a b
    r = a - b * q

-- | Integral type casting
i2n :: (Num n, Integral i, SmallType i, SmallType n) => Data i -> Data n
i2n = sugarSymTR I2N

-- | Cast integer to 'Bool'
i2b :: (Integral a, SmallType a) => Data a -> Data Bool
i2b = sugarSymTR I2B

-- | Cast 'Bool' to integer
b2i :: (Integral a, SmallType a) => Data Bool -> Data a
b2i = sugarSymTR B2I

-- | Round a floating-point number to an integer
round :: (Integral i, SmallType i) => Data Double -> Data i
round = sugarSymTR Round

-- | Take the floor of a floating point number
floor :: Data Double -> Data Double
floor = sugarSymTR Floor

-- | Truncate a floating-point number to an integer
truncate :: (Integral i, SmallType i) => Data Double -> Data i
truncate = sugarSymTR Trunc

-- | Boolean negation
not :: Data Bool -> Data Bool
not = sugarSymTR Not

-- | Boolean conjunction
(&&) :: Data Bool -> Data Bool -> Data Bool
(&&) = sugarSymTR And

infixr 3 &&

-- | Boolean disjunction
(||) :: Data Bool -> Data Bool -> Data Bool
(||) = sugarSymTR Or

infixr 2 ||


-- | Equality
(==) :: SmallType a => Data a -> Data a -> Data Bool
(==) = sugarSymTR Eq

-- | Inequality
(/=) :: SmallType a => Data a -> Data a -> Data Bool
a /= b = not (a==b)

-- | Less than
(<) :: SmallType a => Data a -> Data a -> Data Bool
(<) = sugarSymTR Lt

-- | Greater than
(>) :: SmallType a => Data a -> Data a -> Data Bool
(>) = sugarSymTR Gt

-- | Less than or equal
(<=) :: SmallType a => Data a -> Data a -> Data Bool
(<=) = sugarSymTR Le

-- | Greater than or equal
(>=) :: SmallType a => Data a -> Data a -> Data Bool
(>=) = sugarSymTR Ge

infix 4 ==, /=, <, >, <=, >=

-- | Return the smallest of two values
min :: SmallType a => Data a -> Data a -> Data a
min a b = a<=b ? a $ b
  -- There's no standard definition of min/max in C:
  -- <http://stackoverflow.com/questions/3437404/min-and-max-in-c>
  --
  -- There is `fmin`/`fminf` for floating-point numbers, but these are
  -- implemented essentially as above (except that they handle `NaN`
  -- specifically:
  -- <https://sourceware.org/git/?p=glibc.git;a=blob;f=math/s_fmin.c;hb=HEAD>

-- | Return the greatest of two values
max :: SmallType a => Data a -> Data a -> Data a
max a b = a>=b ? a $ b



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Index into an array
arrIx :: Syntax a => IArr (Internal a) -> Data Index -> a
arrIx arr i = undefined -- resugar $ mapVirtual ix $ unIArr arr
--  where
--    ix :: SmallType b => Imp.IArr Index b -> Data b
--    ix arr = sugarSymTR (ArrIx arr) i


----------------------------------------
-- ** Syntactic conversion
----------------------------------------

desugar :: Syntax a => a -> Data (Internal a)
desugar = Data . Syntactic.desugar

sugar :: Syntax a => Data (Internal a) -> a
sugar = Syntactic.sugar . unData

resugar :: (Syntax a, Syntax b, Internal a ~ Internal b) => a -> b
resugar = Syntactic.resugar



--------------------------------------------------------------------------------
-- * Programs with computational effects
--------------------------------------------------------------------------------

-- | Monads that support computational effects: mutable data structures and
-- control flow
class Monad m => MonadComp m
  where
    -- | Lift a 'Comp' computation
    liftComp :: Comp a -> m a
    -- | Conditional statement
    iff :: Data Bool -> m () -> m () -> m ()
    -- | For loop
    for :: (Integral n, SmallType n) => IxRange (Data n) -> (Data n -> m ()) -> m ()
    -- | While loop
    while :: m (Data Bool) -> m () -> m ()

instance MonadComp Comp
  where
    liftComp        = id
    iff c t f       = Comp $ Imp.iff c (unComp t) (unComp f)
    for  range body = Comp $ Imp.for range (unComp . body)
    while cont body = Comp $ Imp.while (unComp cont) (unComp body)



----------------------------------------
-- ** References
----------------------------------------

-- | Create an uninitialized reference
newRef :: (Type a, MonadComp m) => m (Ref a)
newRef = newNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef :: (Type a, MonadComp m)
    => VarId  -- ^ Base name
    -> m (Ref a)
newNamedRef base = liftComp $ fmap Ref $
    mapVirtualA (const $ Comp $ Imp.newNamedRef base) virtRep

-- | Create an initialized named reference
initRef :: (Syntax a, MonadComp m) => a -> m (Ref (Internal a))
initRef = initNamedRef "r"

-- | Create an initialized named reference
initRefD :: (Syntax (Data a), MonadComp m) => Data a -> m (Ref (Internal (Data a)))
initRefD = initNamedRef "r"

-- | Create an initialized reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedRef :: (Syntax a, MonadComp m)
    => VarId  -- ^ Base name
    -> a       -- ^ Initial value
    -> m (Ref (Internal a))
initNamedRef base =
    liftComp . fmap Ref . mapVirtualA (Comp . Imp.initNamedRef base) . resugar

-- | Get the contents of a reference.
getRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> m a
getRef = liftComp . fmap resugar . mapVirtualA (Comp . Imp.getRef) . unRef

-- | Set the contents of a reference.
setRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> a -> m ()
setRef r
    = liftComp
    . sequence_
    . zipListVirtual (\r' a' -> Comp $ Imp.setRef r' a') (unRef r)
    . resugar

-- | Modify the contents of reference.
modifyRef :: (JSType (Internal a), Syntax a, MonadComp m) => Ref (Internal a) -> (a -> a) -> m ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | A version of 'modifyRef' that fixes the value type to @`Data` a@
modifyRefD :: (SmallType a, MonadComp m) => Ref a -> (Data a -> Data a) -> m ()
modifyRefD r f = setRef r . f =<< unsafeFreezeRef r

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef :: (JSType (Internal a), Syntax a, MonadComp m) => Ref (Internal a) -> m a
unsafeFreezeRef
    = liftComp
    . fmap resugar
    . mapVirtualA (Comp . Imp.unsafeFreezeRef)
    . unRef



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Create an uninitialized array
newArr :: (Type a, MonadComp m) => Data Length -> m (Arr a)
newArr = newNamedArr "a"

-- | Create an uninitialized named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedArr :: forall m a . (Type a, MonadComp m)
    => VarId  -- ^ Base name
    -> Data Length
    -> m (Arr a)
newNamedArr base l = liftComp $ fmap Arr $
    mapVirtualA (const (Comp $ Imp.newNamedArr base l)) rep
  where
    rep = virtRep :: VirtualRep SmallType a

-- | Create and initialize an array
initArr :: (SmallType a, MonadComp m)
    => [a]  -- ^ Initial contents
    -> m (Arr a)
initArr = initNamedArr "a"

-- | Create and initialize a named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedArr :: (SmallType a, MonadComp m)
    => VarId  -- ^ Base name
    -> [a]     -- ^ Initial contents
    -> m (Arr a)
initNamedArr base =
    liftComp . fmap (Arr . Actual) . Comp . Imp.initNamedArr base

-- | Get an element of an array
getArr :: (JSType (Internal a), Syntax a, MonadComp m) => Data Index -> Arr (Internal a) -> m a
getArr i = liftComp . fmap resugar . mapVirtualA (Comp . Imp.getArr i) . unArr

-- | Set an element of an array
setArr :: forall m a . (JSType (Internal a), Syntax a, MonadComp m) =>
    Data Index -> a -> Arr (Internal a) -> m ()
setArr i a
    = liftComp
    . sequence_
    . zipListVirtual (\a' arr' -> Comp $ Imp.setArr i a' arr') rep
    . unArr
  where
    rep = resugar a :: Virtual SmallType Data (Internal a)

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either
-- array.
copyArr :: (Type a, MonadComp m)
    => Arr a        -- ^ Destination
    -> Arr a        -- ^ Source
    -> Data Length  -- ^ Number of elements
    -> m ()
copyArr arr1 arr2 len = liftComp $ sequence_ $
    zipListVirtual (\a1 a2 -> Comp $ Imp.copyArr a1 a2 len)
      (unArr arr1)
      (unArr arr2)

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: (Type a, MonadComp m)
    => Arr a
    -> Data Length  -- ^ Length of array
    -> m (IArr a)
freezeArr arr n
    = undefined -- liftComp
--    $ fmap IArr
--    $ (Comp . flip Imp.freezeArr n) arr

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: (Type a, MonadComp m) => Arr a -> m (IArr a)
unsafeFreezeArr = undefined
--    = liftComp
--    . fmap IArr
--    . (Comp . Imp.unsafeFreezeArr)

-- | Create and initialize an immutable array
initIArr :: (SmallType a, MonadComp m) => [a] -> m (IArr a)
initIArr = undefined -- liftComp . fmap (IArr . Actual) . Comp . Imp.initIArr


----------------------------------------
-- ** Control-flow
----------------------------------------

-- | Conditional statement that returns an expression
ifE :: (SmallType a, Syntax (Data a), MonadComp m)
    => Data Bool  -- ^ Condition
    -> m (Data a)        -- ^ True branch
    -> m (Data a)        -- ^ False branch
    -> m (Data a)
ifE c t f = do
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    unsafeFreezeRef res

-- | Break out from a loop
break :: MonadComp m => m ()
break = liftComp $ Comp Imp.break

-- | Assertion
assert :: MonadComp m
    => Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> m ()
assert cond msg = liftComp $ Comp $ Imp.assert cond msg

class Return a where
  -- | Hard return (i.e. C-style) from a function.
  return_ :: MonadComp m => a -> m ()

instance SmallType a => Return (Data a) where
  return_ x = liftComp $ Comp $ Imp.return_ x

instance SmallType a => Return (Arr a) where
  return_ (Arr (Actual (Imp.ArrComp a))) = return_ (Imp.varExp a :: Data a)

instance Return () where
  return_ _ = return ()
