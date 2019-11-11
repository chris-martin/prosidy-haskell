{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, NoImplicitPrelude,
             PatternSynonyms #-}

-- | A variant of the "Prosidy.AbstractSyntax" module with the
-- general data structure type parameters of the
-- 'Prosidy.AbstractSyntax.Prosidy' type filled by types from
-- the @base@ package.

module Prosidy.AbstractSyntax.Base
  (
    -- * Prosidy content
    Prosidy,
    pattern Prosidy.AbstractSyntax.Document,
    pattern Prosidy.AbstractSyntax.List,
    pattern Prosidy.AbstractSyntax.Paragraph,
    pattern Prosidy.AbstractSyntax.TagParagraph,
    pattern Prosidy.AbstractSyntax.TagBlock,
    pattern Prosidy.AbstractSyntax.TagLiteral,
    pattern Prosidy.AbstractSyntax.TagInline,
    pattern Prosidy.AbstractSyntax.String,
    pattern Prosidy.AbstractSyntax.SoftBreak,
    pattern Prosidy.AbstractSyntax.Attrs,

    BaseDeps, -- todo: figure out where this goes

    -- * Content context: size and level
    Context
        ( Context ),
    Size
        ( One, Many ),
    Level
        ( Root, Block, Inline, Meta ),

    -- * Association list
    AssociationList
        ( AssociationList )

  ) where

import Prosidy.AbstractSyntax hiding (Prosidy)
import qualified Prosidy.AbstractSyntax

import Data.Char (Char)

type BaseDeps =
  'Deps
    ([] Char)             -- string
    []                    -- list
    (AssociationList [])  -- map

-- | 'Prosidy' is the type of Prosidy content.
type Prosidy (context :: Context) =
    Prosidy.AbstractSyntax.Prosidy BaseDeps context

newtype AssociationList list a b =
    AssociationList (list (a, b))
