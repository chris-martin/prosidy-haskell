{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude #-}

module Prosidy.AbstractSyntax
  (
    -- * Prosidy content
    Prosidy
        ( Document, List, Paragraph, TagParagraph, TagBlock,
          TagLiteral, TagInline, String, SoftBreak ),

    -- * Attributes: flags and fields
    Attrs
        ( Attrs ),

    -- * Content context: size and level
    Context
        ( Context ),
    Size
        ( One, Many ),
    Level
        ( Root, Block, Inline )

    -- * Other types: string, list, map
    -- $otherTypes

  ) where

import Data.Kind (Type)

-- | 'Prosidy' is the type of Prosidy content.
data Prosidy
  (string :: Type) (list :: Type -> Type) (map :: Type -> Type -> Type)
  (context :: Context)
    where

    Document ::
        Attrs string map
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Prosidy string list map ('Context 'Many 'Block)
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line
          -- breaks.
      ->
        Prosidy string list map ('Context 'One 'Root)
          -- ^ A Prosidy document consists of a head and a body. The
          -- first line containing only three dashes (@---@) separates
          -- the head from the body.

    List ::
        list (Prosidy string list map ('Context 'One level))
          -- ^ e.g. a list of blocks or a list of inlines
      ->
        Prosidy string list map ('Context 'Many level)
          -- ^ Lists are important in the structure Prosidy (or of
          -- any markup language) because prose is largely linear;
          -- a document body is a list of paragraphs, and paragraphs
          -- are lists of words.

    Paragraph ::
        Prosidy string list map ('Context 'Many 'Inline)
          -- ^ A list of inline elements (plain text, inline tags,
          -- soft breaks).
      ->
        Prosidy string list map ('Context 'One 'Block)
          -- ^ A block of text not wrapped in any special notation
          -- is a paragraph.

    TagParagraph ::
        string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Prosidy string list map ('Context 'Many 'Inline)
          -- ^ Tag body: a list of inlines
      ->
        Prosidy string list map ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]{inlines}
          -- @

    TagBlock ::
        string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Prosidy string list map ('Context 'Many 'Block)
          -- ^ Tag body: a list of blocks
      ->
        Prosidy string list map ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]:end
          -- blocks
          -- #:end
          -- @

    TagLiteral ::
        string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        string
          -- ^ Tag body: a literal string
      ->
        Prosidy string list map ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #=tagname[attrs]:end
          -- text
          -- #:end
          -- @

    TagInline ::
        string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Prosidy string list map ('Context 'Many 'Inline)
          -- ^ Tag body: a list of inline elements
      ->
        Prosidy string list map ('Context 'One 'Inline)
          -- ^ A tag within a paragraph, of the following form:
          --
          -- @
          -- #tagname[attrs]{inlines}
          -- @

    String ::
        string
          -- ^ Plain text
      ->
        Prosidy string list map ('Context 'One 'Inline)
          -- ^ A plain text inline element

    SoftBreak ::
        Prosidy string list map ('Context 'One 'Inline)
          -- ^ A line break within a paragraph. When a Prosidy document is
          -- rendered into another format, typically soft breaks are either
          -- replaced with a space character (or, in a CJK writing system,
          -- are simply removed).

-- | The context of a 'Prosidy' indicates what kind of place it is allowed to
-- appear within the syntax tree.
--
-- The DataKinds GHC extension lifts 'Context' to a kind.
data Context = Context Size Level

-- | The level of a 'Prosidy' indicates what kind of elements it may be
-- nested within.
--
-- The DataKinds GHC extension lifts 'Level' to a kind and its constructors
-- 'Root', 'Block' and 'Inline' to types of the 'Level' kind.
data Level
  where

    -- | The top level containins everything else.
    -- Only a 'Document' appears at the 'Root' level.
    Root :: Level

    -- | The document body is at the block level. A block is either a 'Paragraph'
    -- (text not wrapped in any special notation) or a tag ('TagParagraph',
    -- 'TagBlock', or 'TagLiteral') beginning with (@#-@) or (@#=@).
    --
    -- Blocks have a recursive structure; the body of a 'TagBlock' element is
    -- block-level, permitting a tree of blocks.
    Block :: Level

    -- | The body of a 'Paragraph' or 'TagParagraph' block is at the inline level.
    -- The types of inlines are 'String', 'SoftBreak', and 'TagInline'.
    --
    -- Inlines have a recursive structure; the body of a 'TagInline' element
    -- has an inline level, permitting a tree of inlines.
    Inline :: Level

-- | When a Prosidy element that has a body (other Prosidy content within the
-- element), that body consists of a list of elements. The size of a 'Prosidy'
-- indicates whether the 'Prosidy' represents a list of elements or a single
-- element within such a list.
--
-- The DataKinds GHC extension lifts 'Size' to a kind and its constructors
-- 'One' and 'Many' to types of the 'Size' kind.
data Size
  where

    -- | Indicates that a 'Prosidy' represents a single inline or block element.
    One :: Size

    -- | Only the 'List' constructor has an size of 'Many'.
    Many :: Size

data Attrs
    (string :: Type) (map :: Type -> Type -> Type)
  where
    Attrs ::
        map string ()
          -- ^ Flags
      ->
        map string string
          -- ^ Fields
      ->
        Attrs string map

{- $otherTypes

=== String

The representation of text within a 'Prosidy' value is controlled by the @string@ type parameter.

Some reasonable options for this parameter:

  - 'Data.String.String' from the @base@ package
  - 'Data.Text.Text' from the @text@ package
  - 'Data.Text.Lazy.Text' from the @text@ package

=== List

The representation of sequences within a 'Prosidy' value is controlled by the @list@ type parameter.

Some reasonable options for this parameter:

  - The built-in @[]@ type
  - 'Data.Sequence.Seq' from the @containers@ package
  - 'Data.Vector.Vector' from the @vector@ package

=== Map

The representation of key-value mappings within a 'Prosidy' value is controlled by the @map@ type parameter.

Some reasonable options for this parameter:

  - 'Data.Map.Map' from the @containers@ package
  - 'Data.HashMap.HashMap' from the @unordered-containers@ package

-}
