{-# OPTIONS_GHC -Wall #-}
-- All GHC warnings are enabled.

{-# OPTIONS_GHC -Werror #-}
-- This module may not emit any warnings.

{-# LANGUAGE KindSignatures #-}
-- We use the kind signatures extension to annotate type parameters with their kind using (::). This is just like a type annotation, but what follows the colons is a kind rather than a type.

{-# LANGUAGE NoImplicitPrelude #-}
-- This module has extremely little external dependency, not even from Prelude.

{-# LANGUAGE FunctionalDependencies #-}
-- The 'OpticCompose' class has a functional dependency.

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}

module Prosidy.OpticsConcepts (

    -- * Optic
    Optic (trivial),

    -- * Try
    Try (..), recover, no, ok, overTry,

    -- * Separation
    Separation (..), part, reassemble, afterReassemble, beforeReassemble,

    -- * Try separation
    TrySeparation,

    -- * Operations
    Forward (forward), ForwardTry (forwardTry), Backward (backward), Over (over),

    -- * Optic types
    Iso (Iso), Lens (Lens), Prism (Prism), AffineTraversal (AffineTraversal), ApplicativeTraversal (ApplicativeTraversal), MonadicTraversal (MonadicTraversal),

    -- * Composition
    OpticCompose (..),

    -- * Simple
    Simple

  ) where

import Data.Function (id)

-- Functors
import Data.Functor (fmap)
import Control.Applicative (Applicative (pure))
import Control.Monad (Monad)

-- | Function composition: @ab ▶ bc@ converts from @a@ to @b@, then from @b@ to @c@.
(▶) :: (a -> b) -> (b -> c) -> (a -> c)
(ab ▶ bc) a = bc (ab (a))

(◀) :: (a -> b) -> (c -> a) -> c -> b
(ab ◀ bc) c = ab (bc (c))

data Try a b = No a | Ok b

recover :: Try a a -> a
recover (No a) = a
recover (Ok a) = a

overNo :: (a -> a') -> Try a b -> Try a' b
overNo f (No a) = No (f a)
overNo _ (Ok b) = Ok b

overOk :: (b -> b') -> Try a b -> Try a b'
overOk _ (No a) = No a
overOk f (Ok b) = Ok (f b)

overTry :: (a -> a') -> (b -> b') -> Try a b -> Try a' b'
overTry f _ (No a) = No (f a)
overTry _ f (Ok b) = Ok (f b)

ok :: Prism (Try z b) (Try z b') b b'
ok = Prism abTry Ok
  where
    abTry (Ok b) = Ok b
    abTry (No z) = No (No z)

no :: Prism (Try b z) (Try b' z) b b'
no = Prism abTry No
  where
    abTry (No b) = Ok b
    abTry (Ok z) = No (Ok z)

-- | The result of applying a lens.
--
-- Represents the separation of /a/ into two components:
--
-- - 1. /b/
-- - 2. everything else

data Separation a' b b' =
    Separation
        b          -- ^ The part of the original object targeted by the lens.
        (b' -> a') -- ^ The remainder of the original object, represented as a function that constructs a new version of the original object with the lens-targeted part replaced by a new value.

part :: Separation a b b' -> b
part (Separation b _) = b

reassemble :: Separation a b b -> a
reassemble (Separation b ab') = ab' b

-- | Modifies a separation to perform an additional transformation after reassembly.
afterReassemble :: (b' -> a') -> Separation b' c c' -> Separation a' c c'
afterReassemble f (Separation b bc') = Separation b (f ◀ bc')

-- | Modifies a separation to perform a transformation on the reassembly parameter.
beforeReassemble :: (a -> b') -> Separation a' b b' -> Separation a' b a
beforeReassemble f (Separation b bc') = Separation b (f ▶ bc')

type TrySeparation a' b b' = Try a' (Separation a' b b')

-- |
-- >  ╭──────────╮
-- >  │  a    b  │
-- >  │          │
-- >  │  a'   b' │
-- >  ╰──────────╯

class Optic (o :: * -> * -> * -> * -> *)
  where
    trivial :: Simple o a a

-- |
-- >                    ╭──────────╮   ╭──────────╮   ╭──────────╮
-- >                    │  a    b  │   │  b    c  │   │  a    c  │
-- >  opticCompose  ::  │          │ → │          │ → │          │
-- >                    │  a'   b' │   │  b'   c' │   │  a'   c' │
-- >                    ╰──────────╯   ╰──────────╯   ╰──────────╯
-- >                         x              y              z

class (Optic x, Optic y, Optic z) => OpticCompose x y z | x y -> z
  where
    opticCompose :: x a a' b b' -> y b b' c c' -> z a a' c c'

data Iso a a' b b' = Iso (a -> b) (b' -> a')

instance Optic Iso
  where
    trivial = Iso id id

data Lens a a' b b' = Lens (a -> Separation a' b b')

instance Optic Lens
  where
    trivial = Lens (\a -> Separation a (\_b -> a))

data Prism a a' b b' = Prism (a -> Try a' b) (b' -> a')

instance Optic Prism
  where
    trivial = Prism Ok id

data AffineTraversal a a' b b' = AffineTraversal (a -> TrySeparation a' b b')

instance Optic AffineTraversal
  where
    trivial = AffineTraversal (\a -> Ok (Separation a (\_b -> a)))

type Simple o a b = o a a b b

data ApplicativeTraversal a a' b b' = ApplicativeTraversal (a -> forall f. (Applicative f) => (b -> f b') -> f a')

instance Optic ApplicativeTraversal
  where
    trivial = ApplicativeTraversal (\a f -> f a)

data MonadicTraversal a a' b b' = MonadicTraversal (a -> forall f. (Monad f) => (b -> f b') -> f a')

instance Optic MonadicTraversal
  where
    trivial = MonadicTraversal (\a f -> f a)

instance OpticCompose Iso Iso Iso
  where
    opticCompose (Iso ab ab') (Iso bc bc') = Iso (ab ▶ bc) (ab' ◀ bc')

instance OpticCompose Iso Lens Lens
  where
    opticCompose (Iso ab ab') (Lens bcSep) =
        Lens (ab ▶ bcSep ▶ afterReassemble ab')

instance OpticCompose Iso Prism Prism
  where
    opticCompose (Iso ab ab') (Prism bcTry bc') =
        Prism (ab ▶ bcTry ▶ overNo ab') (ab' ◀ bc')

instance OpticCompose Iso AffineTraversal AffineTraversal
  where
    opticCompose (Iso ab ab') (AffineTraversal bcTrySep) =
        AffineTraversal (ab ▶ bcTrySep ▶ overTry ab' (afterReassemble ab'))

instance OpticCompose Iso ApplicativeTraversal ApplicativeTraversal
  where
    opticCompose (Iso ab ab') (ApplicativeTraversal bcTraversal) =
        ApplicativeTraversal (ab ▶ bcTraversal ▶ (\cAction -> cAction ▶ fmap ab'))

instance OpticCompose Iso MonadicTraversal MonadicTraversal
  where
    opticCompose (Iso ab ab') (MonadicTraversal bcTraversal) =
        MonadicTraversal (ab ▶ bcTraversal ▶ (\cAction -> cAction ▶ fmap ab'))

instance OpticCompose Lens ApplicativeTraversal ApplicativeTraversal
  where
    opticCompose (Lens abSep) (ApplicativeTraversal bcTraversal) =
        ApplicativeTraversal (abSep ▶ (\(Separation b ab') -> (bcTraversal b ▶ fmap ab')))

instance OpticCompose Lens MonadicTraversal MonadicTraversal
  where
    opticCompose (Lens abSep) (MonadicTraversal bcTraversal) =
        MonadicTraversal (abSep ▶ (\(Separation b ab') -> (bcTraversal b ▶ fmap ab')))

instance OpticCompose Prism ApplicativeTraversal ApplicativeTraversal
  where
    opticCompose (Prism abTry ab') (ApplicativeTraversal bcTraversal) =
        ApplicativeTraversal (abTry ▶ overTry (\a _cAction -> pure a) (\b -> bcTraversal b ▶ fmap ab') ▶ recover)

instance OpticCompose Prism MonadicTraversal MonadicTraversal
  where
    opticCompose (Prism abTry ab') (MonadicTraversal bcTraversal) =
        MonadicTraversal (abTry ▶ overTry (\a _cAction -> pure a) (\b -> bcTraversal b ▶ fmap ab') ▶ recover)

instance OpticCompose AffineTraversal ApplicativeTraversal ApplicativeTraversal
  where
    opticCompose (AffineTraversal abTrySep) (ApplicativeTraversal bcTraversal) =
        ApplicativeTraversal (abTrySep ▶ overTry (\a _cAction -> pure a) (\(Separation b ab') -> bcTraversal b ▶ fmap ab') ▶ recover)

instance OpticCompose AffineTraversal MonadicTraversal MonadicTraversal
  where
    opticCompose (AffineTraversal abTrySep) (MonadicTraversal bcTraversal) =
        MonadicTraversal (abTrySep ▶ overTry (\a _cAction -> pure a) (\(Separation b ab') -> bcTraversal b ▶ fmap ab') ▶ recover)

instance OpticCompose ApplicativeTraversal ApplicativeTraversal ApplicativeTraversal
  where
    opticCompose (ApplicativeTraversal abTraversal) (ApplicativeTraversal bcTraversal) =
        ApplicativeTraversal (\a action -> abTraversal a (\b -> bcTraversal b action))

instance OpticCompose ApplicativeTraversal MonadicTraversal MonadicTraversal
  where
    opticCompose (ApplicativeTraversal abTraversal) (MonadicTraversal bcTraversal) =
        MonadicTraversal (\a action -> abTraversal a (\b -> bcTraversal b action))

instance OpticCompose MonadicTraversal ApplicativeTraversal MonadicTraversal
  where
    opticCompose (MonadicTraversal abTraversal) (ApplicativeTraversal bcTraversal) =
        MonadicTraversal (\a action -> abTraversal a (\b -> bcTraversal b action))

instance OpticCompose MonadicTraversal MonadicTraversal MonadicTraversal
  where
    opticCompose (MonadicTraversal abTraversal) (MonadicTraversal bcTraversal) =
        MonadicTraversal (\a action -> abTraversal a (\b -> bcTraversal b action))

-- |
-- >               ╭──────────╮   ╭──────────╮
-- >               │  a    b  │   │  a  → b  │
-- >  forward  ::  │          │ → │          │
-- >               │  a'   b' │   │          │
-- >               ╰──────────╯   ╰──────────╯

class Optic o => Forward o
  where
    forward :: o a a' b b' -> a -> b

instance Forward Iso
  where
    forward (Iso ab _ab') = ab

instance Forward Lens
  where
    forward (Lens abSep) = abSep ▶ part

-- |
-- >                  ╭──────────╮   ╭──────────╮
-- >                  │  a    b  │   │  a  → b  │
-- >  forwardTry  ::  │          │ → │  ↓       │
-- >                  │  a'   b' │   │  a'      │
-- >                  ╰──────────╯   ╰──────────╯

class Optic o => ForwardTry o
  where
    forwardTry :: o a a' b b' -> a -> Try a' b

instance ForwardTry Prism
  where
    forwardTry (Prism abTry _ab') = abTry

instance ForwardTry AffineTraversal
  where
    forwardTry (AffineTraversal abTrySep) = abTrySep ▶ overOk part

-- |
-- >                ╭──────────╮   ╭──────────╮
-- >                │  a    b  │   │          │
-- >  backward  ::  │          │ → │          │
-- >                │  a'   b' │   │  a' ← b' │
-- >                ╰──────────╯   ╰──────────╯

class Optic o => Backward o
  where
    backward :: o a a' b b' -> b' -> a'

instance Backward Iso
  where
    backward (Iso _ab ab') = ab'

instance Backward Prism
  where
    backward (Prism _abTry ab') = ab'

-- |
-- >            ╭──────────╮   ╭──────────╮   ╭──────────╮
-- >            │  a    b  │   │       b  │   │  a       │
-- >  over  ::  │          │ → │       ↓  │ → │  ↓       │
-- >            │  a'   b' │   │       b' │   │  a'      │
-- >            ╰──────────╯   ╰──────────╯   ╰──────────╯

class Optic o => Over o
  where
    over :: o a a' b b' -> (b -> b') -> (a -> a')

instance Over Iso
  where
    over (Iso ab ab') f = ab ▶ f ▶ ab'

instance Over Lens
  where
    over (Lens abSep) f = abSep ▶ beforeReassemble f ▶ reassemble

instance Over Prism
  where
    over (Prism abTry ab') f = abTry ▶ overOk (f ▶ ab') ▶ recover

instance Over AffineTraversal
  where
    over (AffineTraversal abTrySep) f = abTrySep ▶ overOk (beforeReassemble f ▶ reassemble) ▶ recover
