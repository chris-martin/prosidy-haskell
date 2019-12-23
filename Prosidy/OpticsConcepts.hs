{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

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

module Prosidy.OpticsConcepts
  ( {- * Optic -}       Optic ( Iso, Lens, Prism ),
                        OpticForward ( .. ), OpticBackward ( .. ),
                        view, review, preview, over,
    {- * Isomorphism -} Iso, Iso',
    {- * Lens -}        Lens, Lens',
    {- * Prism -}       Prism, Prism'
  ) where

import Prelude (id)

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

view :: Optic 'ForwardTotal backward s t a b -> s -> a
view (Iso f _) x = f x
view (Lens f _) x = f x

review :: Optic forward 'BackwardTotal s t a b -> b -> t
review (Iso _ f) x = f x
review (Prism _ f) x = f x

preview :: Optic forward backward s t a b -> s -> Maybe a
preview (Iso f _) x = Maybe.Just (f x)
preview (Lens f _) x = Maybe.Just (f x)
preview (Prism f _) x = Either.either (\_ -> Maybe.Nothing) Maybe.Just (f x)

over :: Optic forward backward s t a b -> (a -> b) -> (s -> t)
over (Iso f g) h x = g (h (f x))
over (Lens f g) h x = g x (h (f x))
over (Prism f g) h x = (Either.either id (\a -> g (h a)) (f x))

type Iso s t a b = Optic 'ForwardTotal 'BackwardTotal s t a b
type Iso' s a = Iso s s a a
type Lens s t a b = Optic 'ForwardTotal 'BackwardReassemble s t a b
type Lens' s a = Lens s s a a
type Prism s t a b = Optic 'ForwardPartial 'BackwardTotal s t a b
type Prism' s a = Prism s s a a
