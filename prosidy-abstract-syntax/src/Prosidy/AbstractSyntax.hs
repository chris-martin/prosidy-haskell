{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, RankNTypes, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Pro (size :: Size) (context :: Context) (string :: *) (list :: * -> *) =

    (size ~ 'One, context ~ 'RootCtx) =>
      Document
        -- ^ A Prosidy document consists of a head and a body. The first line
        -- containing only three dashes (@---@) separates the head from the body.
        (Attrs string)
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
        (Blocks string list)
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line breaks.

    | (size ~ 'Many) => ProList (list (Pro 'One context string list))
        -- ^ Lists are important in the structure Prosidy (or of any markup language)
        -- because prose is largely linear; a document body is a list of paragraphs,
        -- and paragraphs are lists of words.

    | (size ~ 'One, context ~ 'BlockCtx) => Paragraph (Inlines string list)
        -- ^ A block of text not wrapped in any special notation is a paragraph.
        -- A paragraph is comprised of a non-empty list of inline content.

    | (size ~ 'One, context ~ 'BlockCtx) =>
    TagParagraph (TagName string) (Attrs string) (Inlines string list)
        -- ^ A block of the following form:
        --
        -- @
        -- #-tagname[attrs]{inlines}
        -- @

    | (size ~ 'One, context ~ 'BlockCtx) =>
    TagBlocks (TagName string) (Attrs string) (Blocks string list)
        -- ^ A block of the following form:
        --
        -- @
        -- #-tagname[attrs]:end
        -- blocks
        -- #:end
        -- @

    | (size ~ 'One, context ~ 'BlockCtx) =>
    TagLiteral (TagName string) (Attrs string) string
        -- ^ A block of the following form:
        --
        -- @
        -- #+tagname[attrs]:end
        -- text
        -- #:end
        -- @

    | (size ~ 'One, context ~ 'InlineCtx) =>
    TagInline (TagName string) (Attrs string) (Inlines string list)
        -- ^ A tag within a paragraph, of the following form:
        --
        -- @
        -- #tagname[attrs]{inlines}
        -- @

    | (size ~ 'One, context ~ 'InlineCtx) => String string -- ^ Plain text

    | (size ~ 'One, context ~ 'InlineCtx) => SoftBreak
        -- ^ A line break within a paragraph. When a Prosidy document is rendered
        -- into another format, typically soft breaks are either replaced with
        -- a space character (or, in a CJK writing system, are simply removed).

    | (size ~ 'One, context ~ 'TagNameCtx) => TagName string

data Size = One | Many

data Context = RootCtx | BlockCtx | InlineCtx | TagNameCtx

-- | A Prosidy document consists of a head and a body. The first line containing only three dashes (@---@) separates the head from the body.
type Document = Pro 'One 'RootCtx

-- | A block is either a 'Paragraph' (text not wrapped in any special notation) or a tag ('TagParagraph', 'TagBlocks', or 'TagLiteral') beginning with (@#+@) or (@#-@).
type Block = Pro 'One 'BlockCtx

type Blocks = Pro 'Many 'BlockCtx

type Inline = Pro 'One 'InlineCtx

type Inlines = Pro 'Many 'InlineCtx

type TagName string = forall list. Pro 'One 'TagNameCtx string list

data Attrs string = Attrs (Set string) (Map string string)

-- | A map from type @key@ to type @value@, sorted by key. See "Data.Map".
type Map key value = Map.Map key value

-- | A sorted collection of distinct @element@s. See "Data.Set".
type Set element = Set.Set element
