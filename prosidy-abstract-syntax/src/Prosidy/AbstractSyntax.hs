{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

import qualified Data.Map as Map
import qualified Data.Sequence as List
import qualified Data.Set as Set

data Pro (size :: Size) (context :: Context) string where

    -- | A Prosidy document consists of a head and a body. The first line containing only three dashes (@---@) separates the head from the body.
    Document :: Head string -> Body string -> Document string

    -- | Lists are important in the structure Prosidy (or of any markup language) because prose is largely linear; a document body is a list of paragraphs, and paragraphs are lists of words.
    ProList :: List (Pro 'One context string) -> Pro size context string

    -- | A block of text not wrapped in any special notation is a paragraph. A paragraph is comprised of a non-empty list of inline content.
    Paragraph :: Inlines string -> Block string

    -- | A block of the following form:
    --
    -- @
    -- #-tagname[attrs]{inlines}
    -- @
    TagParagraph :: TagName string -> Attrs string -> Inlines string -> Block string

    -- | A block of the following form:
    --
    -- @
    -- #-tagname[attrs]:end
    -- blocks
    -- #:end
    -- @
    TagBlocks :: TagName string -> Attrs string -> Blocks string -> Block string

    -- | A block of the following form:
    --
    -- @
    -- #+tagname[attrs]:end
    -- text
    -- #:end
    -- @
    TagLiteral :: TagName string -> Attrs string -> string -> Block string

    -- | A tag within a paragraph, of the following form:
    --
    -- @
    -- #tagname[attrs]{inlines}
    -- @
    TagInline :: TagName string -> Attrs string -> Inlines string -> Inline string

    -- | Plain text
    String :: string -> Inline string

    -- | A line break within a paragraph. When a Prosidy document is rendered into another format, typically soft breaks are either replaced with a space character (or, in a CJK writing system, are simply removed).
    SoftBreak :: Inline string

    TagName :: string -> TagName string

data Size = One | Many

data Context = RootCtx | BlockCtx | InlineCtx | TagNameCtx

-- | A Prosidy document consists of a head and a body. The first line containing only three dashes (@---@) separates the head from the body.
type Document = Pro 'One 'RootCtx

-- | The beginning of a Prosidy document is the head. Each non-empty line of the head is an attribute.
type Head = Attrs

-- | A Prosidy document body consists of a list of blocks. Blocks are (typically) separated by two consecutive line breaks.
type Body = Blocks

-- | A block is either a 'Paragraph' (text not wrapped in any special notation) or a tag ('TagParagraph', 'TagBlocks', or 'TagLiteral') beginning with (@#+@) or (@#-@).
type Block = Pro 'One 'BlockCtx

type Blocks = Pro 'Many 'BlockCtx

type Inline = Pro 'One 'InlineCtx

type Inlines = Pro 'Many 'InlineCtx

type TagName = Pro 'One 'TagNameCtx

data Attrs string = Attrs (Set string) (Map string string)

-- | A finite sequence of elements, with the parameter @size@ constraining how many elements are in the sequence, and the parameter @element@ indicating the type of the elements.
data List (element :: *) where

    -- | A finger tree. See "Data.Sequence".
    List0 :: List.Seq element -> List element

-- | A map from type @key@ to type @value@, sorted by key. See "Data.Map".
type Map key value = Map.Map key value

-- | A sorted collection of distinct @element@s. See "Data.Set".
type Set element = Set.Set element
