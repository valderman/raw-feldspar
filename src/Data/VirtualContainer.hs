{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TypeOperators, UndecidableInstances, RankNTypes, TemplateHaskell, ConstraintKinds, GADTs, ConstraintKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PatternSynonyms, Rank2Types, ScopedTypeVariables, TypeFamilies, TypeOperators, ViewPatterns #-}

-- | Virtual containers

module Data.VirtualContainer where



import Control.Monad.Identity
import Data.Proxy
-- import Language.Haskell.TH

import Language.Syntactic
-- import Language.Syntactic.Functional.Tuple

import Data.TypeRep
import Data.TypeRep.Representation
import Data.TypeRep.Types.Tuple
-- import Language.Syntactic.TypeRep.TupleConversion

-- import Data.VirtualContainer.TH
--------------------------------------------------------------------------------
-- * Virtual containers
--------------------------------------------------------------------------------

-- | A virtual structured container
--
-- It makes sense for the predicate @p@ to exclude tuple types. That way the
-- the type index @a@ uniquely determines the constructor used. This assumption
-- is used by e.g. 'viewActual' and 'zipVirtual' which are only total if it is
-- impossible for 'Actual' to return the same type as the other constructors.

data Virtual p con res
  = p res => Actual (con res)

-- | Get the content of a 'Virtual' leaf
--
-- This function is only total if tuples are excluded by the predicate @pred@.
viewActual :: pred a => Virtual pred con a -> con a
viewActual (Actual c) = c
  -- Note that this function is well-typed even without the contraint, but then
  -- the constraint is needed to guarantee totality (under the assumption that
  -- tuples are excluded by @pred@).

-- | Representation of the structure of a 'Virtual' container
type VirtualRep pred = Virtual pred Proxy

-- | Virtualizable types
class VirtualType_ pred a
  where
    virtRep :: VirtualRep pred a
  -- Not exported since we want a closed class

instance pred a => VirtualType_ pred a
  where
    virtRep = Actual Proxy

-- | Virtualizable types
class    VirtualType_ pred a => VirtualType pred a
instance VirtualType_ pred a => VirtualType pred a

-- | Create a 'Virtual' container from a value of a virtualizable type
toVirtual :: forall p a . VirtualType p a => a -> Virtual p Identity a
toVirtual = go (virtRep :: VirtualRep p a) . Identity
  where
    go :: VirtualRep p b -> Identity b -> Virtual p Identity b
    go (Actual _) i = Actual i


-- 'sugar' assumes that the argument is either a tuple type or a type satisfying
-- @pred@.
instance (TupleType :<: t, PWitness pred t t) =>
    Syntactic (Virtual pred (ASTFull (sym :&: TypeRep t)) a)
  where
    type Domain   (Virtual pred (ASTFull (sym :&: TypeRep t)) a) = (sym :&: TypeRep t)
    type Internal (Virtual pred (ASTFull (sym :&: TypeRep t)) a) = a

    desugar (Actual a) = unASTFull a

    sugar a = simpleMatch (go t) $ unTypeRep t
      where
        t = getDecor a
        go :: (DenResult sig ~ a)
           => TypeRep t a
           -> t sig
           -> Args (AST t) sig
           -> Virtual pred (ASTFull (sym :&: TypeRep t)) a
        go t _ _ | Right Dict <- pwit (Proxy :: Proxy pred) t = Actual $ sugar a

-- | Map over a 'Virtual' structure
mapVirtual :: forall pred c1 c2 b
    .  (forall a . pred a => c1 a -> c2 a)
    -> Virtual pred c1 b
    -> Virtual pred c2 b
mapVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a
    go (Actual a) = Actual (f a)

-- | Monadic map over a 'Virtual' structure
mapVirtualA :: forall m pred c1 c2 b . Applicative m
    => (forall a . pred a => c1 a -> m (c2 a))
    -> Virtual pred c1 b -> m (Virtual pred c2 b)
mapVirtualA f = go
  where
    go :: Virtual pred c1 a -> m (Virtual pred c2 a)
    go (Actual a) = Actual <$> (f a)

-- | Map over a 'Virtual' structure
mapVirtualA_ :: forall m pred cont b . Applicative m =>
    (forall a . pred a => cont a -> m ()) -> Virtual pred cont b -> m ()
mapVirtualA_ f = go
  where
    go :: Virtual pred cont a -> m ()
    go (Actual a) = f a

-- | Fold a 'Virtual' structure to a list
listVirtual :: forall pred cont b c .
    (forall y . pred y => cont y -> c) -> Virtual pred cont b -> [c]
listVirtual f = go
  where
    go :: Virtual pred cont a -> [c]
    go (Actual a) = [f a]

-- | Zip two 'Virtual' structures
--
-- It is assumed that the predicate @pred@ excludes tuples so that the two
-- arguments are guaranteed to have the same structure.
zipVirtual :: forall pred c1 c2 c3 b
    . (forall a . pred a => c1 a -> c2 a -> c3 a)
    -> Virtual pred c1 b
    -> Virtual pred c2 b
    -> Virtual pred c3 b
zipVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a -> Virtual pred c3 a
    go (Actual a) (Actual b) = Actual (f a b)


-- | Zip two 'Virtual' structures to a list
--
-- It is assumed that the predicate @pred@ excludes tuples so that the two
-- arguments are guaranteed to have the same structure.
zipListVirtual :: forall pred c1 c2 b r
    . (forall a . pred a => c1 a -> c2 a -> r)
    -> Virtual pred c1 b
    -> Virtual pred c2 b
    -> [r]
zipListVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a -> [r]
    go (Actual a) (Actual b) = [f a b]

-- | Compare two 'Virtual' structures using a function that compares the
-- 'Actual' elements. If the structures don't match, 'False' is returned.
compareVirtual :: forall pred c1 c2 c d
    . (forall a b . (pred a, pred b) => c1 a -> c2 b -> Bool)
    -> Virtual pred c1 c
    -> Virtual pred c2 d
    -> Bool
compareVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 b -> Bool
    go (Actual a) (Actual b) = f a b

-- | Lift a function operating on containers @con@ to a function operating on
-- virtual containers. It is assumed that @pred@ excludes tuples, so that the
-- virtual containers just contain a single 'Actual' node.
liftVirt :: (pred a, pred b) =>
    (con a -> con b) -> Virtual pred con a -> Virtual pred con b
liftVirt f (Actual a) = Actual (f a)

-- | Lift a function operating on containers @con@ to a function operating on
-- virtual containers. It is assumed that @pred@ excludes tuples, so that the
-- virtual containers just contain a single 'Actual' node.
liftVirt2 :: (pred a, pred b, pred c)
    => (con a -> con b -> con c)
    -> Virtual pred con a -> Virtual pred con b -> Virtual pred con c
liftVirt2 f (Actual a) (Actual b) = Actual (f a b)
