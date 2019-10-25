{- |
    ==A small singleton core module

    A trimmed down core version of the singletons library made for my use case or more
    specifically Hreq but still general enough for similar use cases.

    ===Motivation

    The singleton library has pretty long compile times which can be hard to justify when you are using only a small portion of it.
    It is also not backward compatible with previous GHC releases.
    So if you want to support earlier GHC releases in a library that depends on singletons,
    you are pretty much left to the mercy of CPP hacks.

    ===Attribution
    Some of the code in this module was directly borrowed from the Singletons library
-}
{-# LANGUAGE TypeInType #-}
module Data.Singletons where

import Data.Kind
import GHC.TypeLits
import Data.Typeable

type family Sing :: k -> Type

class SingI a where
  sing :: Sing a

-- | TODO: Maybe add a SingKind class and SingKind instances

-- | Type-lits
-------------------

data SNat (n :: Nat) =  KnownNat n => SNat
type instance Sing = SNat

instance KnownNat a => SingI (a :: Nat) where
  sing = SNat

data SSymbol (n :: Symbol) = KnownSymbol n => SSym
type instance Sing = SSymbol

instance KnownSymbol a => SingI (a :: Symbol) where
  sing = SSym

-- | Given a singleton for @Nat@, call something requiring a
-- @KnownNat@ instance.
withKnownNat :: Sing n -> (KnownNat n => r) -> r
withKnownNat SNat f = f

-- | Given a singleton for @Symbol@, call something requiring
-- a @KnownSymbol@ instance.
withKnownSymbol :: Sing n -> (KnownSymbol n => r) -> r
withKnownSymbol SSym f = f

-- | Lists
----------------------

data SList :: [k] -> Type where
  SNil :: SList '[]
  SCons :: forall k (h :: k) (t :: [k]). Sing h -> SList t -> SList (h ': t)

type instance Sing = SList

instance SingI ('[] :: [k]) where
  sing = SNil

instance (SingI x, SingI xs) => SingI ( (x ': xs)  :: [k]) where
  sing = SCons sing sing

-- | Tuples
-------------------

data STuple2 (a :: (k1, k2)) where
  STuple2 :: Sing x -> Sing y -> STuple2 '(x, y)

instance (SingI x, SingI y) => SingI ( '(x, y) :: (k1, k2)) where
  sing = STuple2 sing sing

type instance Sing = STuple2

-- | TypeRep
----------------------------------

data STypeRep :: Type -> Type where
  STypeRep :: forall (t :: Type). Typeable t => STypeRep t

type instance Sing = STypeRep

instance Typeable a => SingI (a :: Type) where
  sing = STypeRep
