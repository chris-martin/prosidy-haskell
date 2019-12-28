{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

-- todo {-# OPTIONS_GHC -Werror #-}
    {- This module may not emit any warnings. -}

{-# LANGUAGE GADTs #-}
    {- The 'Optic' type is a GADT; its data constructors have
       different type parameters than the 'Optic' type itself.
       This requires enabling the GADTs language extension. -}

{-# LANGUAGE DataKinds #-}
    {- The data kinds extension turns types into kinds and
       data constructors into type constructors. We use this to
       establish the kinds 'Presence' and 'Proportion' which
       parameterize the 'Optic' type. -}

{-# LANGUAGE KindSignatures #-}
    {- We use the kind signatures extension to annotate type
       parameters with their kind using (::). This is just like
       a type annotation, but what follows the colons is a kind
       rather than a type. -}

{-# LANGUAGE NoImplicitPrelude #-}
    {- This module has extremely little external dependency,
       not even from Prelude. -}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE LambdaCase #-}

module Prosidy.OpticsConcepts
  ( {- * Optic definition -}  Optic ( Iso, Lens, Prism, AffineTraversal,
                                      Getter ),
                              Presence ( .. ), Proportion ( .. ),
    {- * Try -}               Try ( .. ), recover, overNo, overOk, overTry,
    {- * Separation -}        Separation ( .. ), part, reassemble,
                              afterReassemble, beforeReassemble,
    {- * Operations -}        forward, backward, tryForward, over,
    {- * Isomorphism -}       Iso, Iso',
    {- * Lens -}              Lens, Lens',
    {- * Prism -}             Prism, Prism',
    {- * Affine traversal -}  AffineTraversal, AffineTraversal',
    {- * Getter -}            Getter, Getter',
    {- * Composition -}       opticCompose,
                              PresenceComposition,
                              ProportionComposition, ProportionMaybeComposition
  ) where

import Data.Maybe ( Maybe ( Just, Nothing ) )

-- | Function composition: @ab ▶ bc@ converts from @a@ to @b@, then from @b@ to @c@.
(▶) :: (a -> b) -> (b -> c) -> (a -> c)
(ab ▶ bc) a = bc (ab (a))

(◀) :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
(ab ◀ bc) c = ab (bc (c))

data Try a b = No a | Ok b

recover :: Try a a -> a
recover = \case No a -> a; Ok a -> a

overNo :: (a -> a') -> Try a b -> Try a' b
overNo f = \case No a -> No (f a); Ok b -> Ok b

overOk :: (b -> b') -> Try a b -> Try a b'
overOk f = \case No a -> No a; Ok b -> Ok (f b)

overTry :: (a -> a') -> (b -> b') -> Try a b -> Try a' b'
overTry f g = \case No a -> No (f a); Ok b -> Ok (g b)

-- | The result of applying a lens.
--
-- Represents the separation of /a/ into two components:
--
-- - 1. /b/
-- - 2. everything else

data Separation a' b b' =
    Separation
        b          -- ^ The part of the original object targeted by the lens.
        (b' -> a') -- ^ The remainder of the original object, represented as
                   --   a function that constructs a new version of the original
                   --   object with the lens-targeted part replaced by a new value.

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

data Presence = AlwaysPresent | MayBeMissing

data Proportion = EntireThing | PartOfWhole

-- |
-- >  ╭──────────╮  ╭──────────╮     ╭───────────────╮     ╭──────────╮
-- >  │  a  → b  │  │  b  → c  │     │  a  → b  → c  │     │  a  → c  │
-- >  │       ↓  │  │       ↓  │  =  │            ↓  │  =  │       ↓  │
-- >  │  a' ← b' │  │  b' ← c' │     │  a' ← b' ← c' │     │  a' ← c' │
-- >  ╰──────────╯  ╰──────────╯     ╰───────────────╯     ╰──────────╯
data Optic (presence :: Presence) (proportion :: Maybe Proportion) a a' b b'
  where
    Iso :: (a -> b) -> (b' -> a')                         -> Optic 'AlwaysPresent ('Just 'EntireThing)  a a' b b'
    Lens :: (a -> Separation a' b b')                     -> Optic 'AlwaysPresent ('Just 'PartOfWhole)  a a' b b'
    Prism :: (a -> Try a' b) -> (b' -> a')                -> Optic 'MayBeMissing  ('Just 'EntireThing)  a a' b b'
    AffineTraversal :: (a -> Try a' (Separation a' b b')) -> Optic 'MayBeMissing  ('Just 'PartOfWhole)  a a' b b'
    Getter :: (a -> b)                                    -> Optic 'AlwaysPresent 'Nothing              a a' b b'

-- |
-- >                ╭──────────╮  ╭──────────╮     ╭───────────────╮     ╭──────────╮
-- >                │  a  → b  │  │  b  → c  │     │  a  → b  → c  │     │  a  → c  │
-- >  opticCompose  │       ↓  │  │       ↓  │  =  │            ↓  │  =  │       ↓  │
-- >                │  a' ← b' │  │  b' ← c' │     │  a' ← b' ← c' │     │  a' ← c' │
-- >                ╰──────────╯  ╰──────────╯     ╰───────────────╯     ╰──────────╯

opticCompose :: Optic abPresence abProportion a a' b b'
             -> Optic bcPresence bcProportion b b' c c'
             -> Optic (PresenceComposition abPresence bcPresence)
                      (ProportionMaybeComposition abProportion bcProportion)
                      a a' c c'

opticCompose (Iso ab ab') (Iso bc bc') = Iso (ab ▶ bc) (ab' ◀ bc')
opticCompose (Iso ab ab') (Lens bcSep) = Lens (ab ▶ bcSep ▶ afterReassemble ab')
opticCompose (Iso ab ab') (Prism bcTry bc') = Prism (ab ▶ bcTry ▶ overNo ab') (ab' ◀ bc')
opticCompose (Iso ab ab') (AffineTraversal bcTrySep) = AffineTraversal (ab ▶ bcTrySep ▶ overTry ab' (afterReassemble ab'))
opticCompose (Iso ab _ab') (Getter bc) = Getter (ab ▶ bc)

type family PresenceComposition a b
  where
    PresenceComposition 'AlwaysPresent 'AlwaysPresent = 'AlwaysPresent
    PresenceComposition 'MayBeMissing _ = 'MayBeMissing
    PresenceComposition _ 'MayBeMissing = 'MayBeMissing

type family ProportionComposition a b
  where
    ProportionComposition 'PartOfWhole _ = 'PartOfWhole
    ProportionComposition _ 'PartOfWhole = 'PartOfWhole
    ProportionComposition 'EntireThing 'EntireThing = 'EntireThing

type family ProportionMaybeComposition a b
  where
    ProportionMaybeComposition ('Just x) ('Just y) = ('Just (ProportionComposition x y))
    ProportionMaybeComposition _ _ = 'Nothing

forward :: Getter proportion a a' b b' -> a -> b
forward (Iso ab _ab') = ab
forward (Lens abSep) = abSep ▶ part
forward (Getter ab) = ab

backward :: Optic presence ('Just 'EntireThing) a a' b b' -> b' -> a'
backward (Iso _ab ab') = ab'
backward (Prism _abTry ab') = ab'

tryForward :: Optic presence proportion a a' b b' -> a -> Try a' b
tryForward (Iso ab _ab') = ab ▶ Ok
tryForward (Lens abSep) = abSep ▶ part ▶ Ok
tryForward (Prism abTry _ab') = abTry
tryForward (AffineTraversal abTrySep) = abTrySep ▶ overOk part
tryForward (Getter ab) = ab ▶ Ok

over :: Optic presence ('Just proportion) a a' b b' -> (b -> b') -> (a -> a')
over (Iso ab ab') f = ab ▶ f ▶ ab'
over (Lens abSep) f = abSep ▶ beforeReassemble f ▶ reassemble
over (Prism abTry ab') f = abTry ▶ overOk (f ▶ ab') ▶ recover
over (AffineTraversal abTrySep) f = abTrySep ▶ overOk (beforeReassemble f ▶ reassemble) ▶ recover

type Iso a a' b b' = Optic 'AlwaysPresent ('Just 'EntireThing) a a' b b'
type Iso' a b = Iso a a b b
type Lens a a' b b' = Optic 'AlwaysPresent ('Just 'PartOfWhole) a a' b b'
type Lens' a b = Lens a a b b
type Prism a a' b b' = Optic 'MayBeMissing ('Just 'EntireThing) a a' b b'
type Prism' a b = Prism a a b b
type AffineTraversal a a' b b' = Optic 'MayBeMissing ('Just 'PartOfWhole) a a' b b'
type AffineTraversal' a b = AffineTraversal a a b b
type Getter proportion a a' b b' = Optic 'AlwaysPresent proportion a a' b b'
type Getter' proportion a b = Getter proportion a a b b
