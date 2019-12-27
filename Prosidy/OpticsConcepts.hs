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

module Prosidy.OpticsConcepts
  ( {- * Optic definition -}  Optic ( Iso, Lens, Prism, AffineTraversal ),
                              Presence ( .. ), Proportion ( .. ),
                              Try ( .. ), Separation ( .. ),
    {- * Operations -}        forward, backward, tryForward, over,
    {- * Isomorphism -}       Iso, Iso',
    {- * Lens -}              Lens, Lens',
    {- * Prism -}             Prism, Prism',
    {- * Affine traversal -}  AffineTraversal, AffineTraversal',
    {- * Composition -}       opticCompose,
                              PresenceComposition, ProportionComposition
  ) where

data Try t a = No t | Ok a

-- | The result of applying a lens.
--
-- Represents the separation of s/t into two components:
--
-- - 1. a/b
-- - 2. everything else

data Separation t a b =
    Separation
        a          -- ^ The part of the original object targeted by the lens.
        (b -> t)   -- ^ The remainder of the original object, represented as
                   --   a function that constructs a new version of the original
                   --   object with the lens-targeted part replaced by a new value.

data Presence = AlwaysPresent | MayBeMissing

data Proportion = EntireThing | PartOfWhole

data Optic (targetPresence :: Presence) (targetProportion :: Proportion) s t a b
  where
    Iso :: (s -> a) -> (b -> t)                        -> Optic 'AlwaysPresent 'EntireThing s t a b
    Lens :: (s -> Separation t a b)                    -> Optic 'AlwaysPresent 'PartOfWhole s t a b
    Prism :: (s -> Try t a) -> (b -> t)                -> Optic 'MayBeMissing  'EntireThing s t a b
    AffineTraversal :: (s -> Try t (Separation t a b)) -> Optic 'MayBeMissing  'PartOfWhole s t a b

-- |
-- >                ╭───────╮  ╭───────╮     ╭───────────╮     ╭───────╮
-- >                │ s → u │  │ u → a │     │ s → u → a │     │ s → a │
-- >  opticCompose  │     ↓ │  │     ↓ │  =  │         ↓ │  =  │     ↓ │
-- >                │ t ← v │  │ t ← b │     │ t ← v ← b │     │ t ← b │
-- >                ╰───────╯  ╰───────╯     ╰───────────╯     ╰───────╯

opticCompose :: forall fore1 back1 fore2 back2 s t u v a b.
       Optic fore1 back1 s t u v
    -> Optic fore2 back2 u v a b
    -> Optic (PresenceComposition fore1 fore2)
             (ProportionComposition back2 back2) s t a b

opticCompose (Iso convert1 convertBack1) (Iso convert2 convertBack2) = Iso convert3 convertBack3
  where
    convert3 :: s -> a
    convert3 s = convert2 (convert1 s :: u)

    convertBack3 :: b -> t
    convertBack3 b = convertBack1 (convertBack2 b :: v)

opticCompose (Iso convert convertBack) (Lens separate) = Lens separate'
  where
    separate' :: s -> Separation t a b
    separate' s = Separation part reassemble'
      where
        Separation part reassemble = separate (convert s :: u)
        reassemble' b = convertBack (reassemble b)

opticCompose (Iso convert convertBack) (Prism narrow widen) =
    Prism
        (\s -> case narrow (convert s :: u) of
            No (v :: v) -> No (convertBack v :: t)
            Ok (a :: a) -> Ok a
        )
        (\b -> convertBack (widen b))

type family PresenceComposition a b
  where
    PresenceComposition 'AlwaysPresent 'AlwaysPresent = 'AlwaysPresent
    PresenceComposition 'MayBeMissing _ = 'MayBeMissing
    PresenceComposition _ 'MayBeMissing = 'MayBeMissing

type family ProportionComposition a b
  where
    ProportionComposition 'EntireThing 'EntireThing = 'EntireThing
    ProportionComposition 'PartOfWhole _ = 'PartOfWhole
    ProportionComposition _ 'PartOfWhole = 'PartOfWhole

forward :: Optic 'AlwaysPresent targetProportion s t a b -> s -> a
forward (Iso convert _convertBack) x = convert x
forward (Lens separate) x = part
  where
    Separation part _ = separate x

backward :: Optic targetPresence 'EntireThing s t a b -> b -> t
backward (Iso _convert convertBack) x = convertBack x
backward (Prism _narrow widen) x = widen x

tryForward :: Optic targetPresence targetProportion s t a b -> s -> Try t a
tryForward o (s :: s) = case o of
    Iso convert _convertBack -> Ok (convert s)
    Lens separate -> Ok part
      where
        Separation part _ = separate s
    Prism narrow _widen -> narrow s
    AffineTraversal separate ->
        case (separate s) of
            No t -> No t
            Ok (Separation part _) -> Ok part

over :: Optic targetPresence targetProportion s t a b -> (a -> b) -> (s -> t)
over o (f :: a -> b) (s :: s) = case o of
    Iso convert convertBack -> convertBack (f (convert s))
    Lens separate -> reassemble (f part)
      where
        Separation part reassemble = separate s
    Prism narrow widen ->
        case (narrow s) of
            No t -> t
            Ok a -> widen (f a)
    AffineTraversal separate ->
        case (separate s) of
            No t -> t
            Ok (Separation part reassemble) -> reassemble (f part)

type Iso s t a b = Optic 'AlwaysPresent 'EntireThing s t a b
type Iso' s a = Iso s s a a
type Lens s t a b = Optic 'AlwaysPresent 'PartOfWhole s t a b
type Lens' s a = Lens s s a a
type Prism s t a b = Optic 'MayBeMissing 'EntireThing s t a b
type Prism' s a = Prism s s a a
type AffineTraversal s t a b = Optic 'MayBeMissing 'PartOfWhole s t a b
type AffineTraversal' s a = AffineTraversal s s a a
