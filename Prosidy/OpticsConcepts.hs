{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

{-# OPTIONS_GHC -Werror #-}
    {- This module may not emit any warnings. -}

{-# LANGUAGE GADTs #-}
    {- The 'Optic' type is a GADT; its data constructors have
       different type parameters than the 'Optic' type itself.
       This requires enabling the GADTs language extension. -}

{-# LANGUAGE DataKinds #-}
    {- The data kinds extension turns types into kinds and
       data constructors into type constructors. We use this to
       establish the kinds 'OpticForward' and 'OpticBackward' which
       parameterize the 'Optic' type. -}

{-# LANGUAGE KindSignatures #-}
    {- We use the kind signatures extension to annotate type
       parameters with their kind using (::). This is just like
       a type annotation, but what follows the colons is a kind
       rather than a type. -}

{-# LANGUAGE NoImplicitPrelude #-}
    {- This module depends on as little library code as possible,
       even from the base package. Disabling the implicit import
       of the Prelude module makes this more clear. -}

{-# LANGUAGE ScopedTypeVariables #-}

module Prosidy.OpticsConcepts
  ( {- * Optic definition -}  Optic ( Iso, Lens, Prism, AffineTraversal ),
                              OpticForward ( .. ), OpticBackward ( .. ),
    {- * Operations -}        view, review, preview, over,
    {- * Isomorphism -}       Iso, Iso',
    {- * Lens -}              Lens, Lens',
    {- * Prism -}             Prism, Prism',
    {- * Affine traversal -}  AffineTraversal, AffineTraversal'
  ) where

-- Either
import Data.Either (Either)
import qualified Data.Either as Either

-- Maybe
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

data OpticForward = ForwardTotal | ForwardPartial

data OpticBackward = BackwardTotal | BackwardReassemble

data Optic (forward :: OpticForward) (backward :: OpticBackward) s t a b
  where
    Iso ::
        (s -> a)           -- ^ Total forward function
        ->
        (b -> t)           -- ^ Total backward function
        ->
        Optic 'ForwardTotal 'BackwardTotal s t a b

    Lens ::
        (s -> a)           -- ^ Total forward function
        ->
        (s -> (b -> t))    -- ^ Backward reassembly
        ->
        Optic 'ForwardTotal 'BackwardReassemble s t a b

    Prism ::
        (s -> Either t a)  -- ^ Partial forward function
        ->
        (b -> t)           -- ^ Total backward function
        ->
        Optic 'ForwardPartial 'BackwardTotal s t a b

    AffineTraversal ::
        (s -> Either t a)  -- ^ Partial forward function
        ->
        (s -> (b -> t))    -- ^ Backward reassembly
        ->
        Optic 'ForwardPartial 'BackwardReassemble s t a b

view :: Optic 'ForwardTotal backward s t a b -> s -> a
view (Iso convert _convertBack) x = convert x
view (Lens getPart _reassemble) x = getPart x

review :: Optic forward 'BackwardTotal s t a b -> b -> t
review (Iso _convert convertBack) x = convertBack x
review (Prism _narrow widen) x = widen x

preview :: Optic forward backward s t a b -> s -> Maybe a
preview o (s :: s) = case o of
    Iso convert _convertBack -> Maybe.Just (convert s)
    Lens getPart _reassemble -> Maybe.Just (getPart s)
    Prism narrow _widen ->
        case (narrow s) of
            Either.Left _t -> Maybe.Nothing
            Either.Right a -> Maybe.Just a
    AffineTraversal findPart _reassemble ->
        case (findPart s) of
            Either.Left _t -> Maybe.Nothing
            Either.Right a -> Maybe.Just a

over :: Optic forward backward s t a b -> (a -> b) -> (s -> t)
over o (f :: a -> b) (s :: s) = case o of
    Iso convert convertBack -> convertBack (f (convert s))
    Lens getPart reassemble -> reassemble s (f (getPart s))
    Prism narrow widen ->
        case (narrow s) of
            Either.Left t -> t
            Either.Right a -> widen (f a)
    AffineTraversal findPart reassemble ->
        case (findPart s) of
            Either.Left t -> t
            Either.Right a -> reassemble s (f a)

type Iso s t a b = Optic 'ForwardTotal 'BackwardTotal s t a b
type Iso' s a = Iso s s a a
type Lens s t a b = Optic 'ForwardTotal 'BackwardReassemble s t a b
type Lens' s a = Lens s s a a
type Prism s t a b = Optic 'ForwardPartial 'BackwardTotal s t a b
type Prism' s a = Prism s s a a
type AffineTraversal s t a b = Optic 'ForwardPartial 'BackwardReassemble s t a b
type AffineTraversal' s a = AffineTraversal s s a a
