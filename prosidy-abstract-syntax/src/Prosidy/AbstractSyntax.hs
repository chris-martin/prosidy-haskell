{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, ExplicitForAll #-}

module Prosidy.AbstractSyntax
  (
  -- * Prosidy content
    Pro (..)

  -- * Tags
  , Tag (..)

  -- * Attributes
  , Attrs (..)

  -- * Sizes
  , Size (..)

  -- * Contexts
  , Context (..)

  -- * Strings
  -- $string

  -- * Lists
  -- $list

  -- * Maps
  -- $map

  -- * Specialization with base types
  , BasePro, AssociationList (..)

  ) where

import qualified Data.Char

-- | 'Pro' is the type of Prosidy content.

data Pro
  (string :: *) (list :: * -> *) (map :: * -> * -> *)
  (size :: Size) (context :: Context)
    where

    Document ::
        Attrs string map
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Pro string list map 'Many 'Block
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line
          -- breaks.
      ->
        Pro string list map 'One 'Root
          -- ^ A Prosidy document consists of a head and a body. The
          -- first line containing only three dashes (@---@) separates
          -- the head from the body.

    List ::
        list (Pro string list map 'One context)
          -- ^ e.g. a list of blocks or a list of inlines
      ->
        Pro string list map 'Many context
          -- ^ Lists are important in the structure Prosidy (or of
          -- any markup language) because prose is largely linear;
          -- a document body is a list of paragraphs, and paragraphs
          -- are lists of words.

    Paragraph ::
        Pro string list map 'Many 'Inline
          -- ^ A list of inline elements (plain text, inline tags,
          -- soft breaks).
      ->
        Pro string list map 'One 'Block
          -- ^ A block of text not wrapped in any special notation
          -- is a paragraph.

    TagParagraph ::
        Tag string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Pro string list map 'Many 'Inline
          -- ^ Tag body (a list of inline elements)
      ->
        Pro string list map 'One 'Block
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]{inlines}
          -- @

    TagBlock ::
        Tag string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Pro string list map 'Many 'Block
          -- ^ Tag body (a list of block elements)
      ->
        Pro string list map 'One 'Block
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]:end
          -- blocks
          -- #:end
          -- @

    TagLiteral ::
        Tag string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        string
          -- ^ Tag body (plain text)
      ->
        Pro string list map 'One 'Block
          -- ^ A block of the following form:
          --
          -- @
          -- #=tagname[attrs]:end
          -- text
          -- #:end
          -- @

    TagInline ::
        Tag string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Pro string list map 'Many 'Inline
          -- ^ Tag body (a list of inline elements)
      ->
        Pro string list map 'One 'Inline
          -- ^ A tag within a paragraph, of the following form:
          --
          -- @
          -- #tagname[attrs]{inlines}
          -- @

    String ::
        string
          -- ^ Plain text
      ->
        Pro string list map 'One 'Inline
          -- ^ A plain text inline element

    SoftBreak ::
        Pro string list map 'One 'Inline
          -- ^ A line break within a paragraph. When a Prosidy document is
          -- rendered into another format, typically soft breaks are either
          -- replaced with a space character (or, in a CJK writing system,
          -- are simply removed).

-- | The 'Context' of a 'Pro' indicates what kind of elements it may be
-- nested within.
--
-- The DataKinds GHC extension lifts 'Context' to a kind and its constructors
-- 'Root', 'Block' and 'Inline' to types of the 'Context' kind.
data Context where

    -- | The top-level context containing everything else.
    -- Only a 'Document' has this context.
    Root :: Context

    -- | The document body is a block context. A block is either a 'Paragraph'
    -- (text not wrapped in any special notation) or a tag ('TagParagraph',
    -- 'TagBlock', or 'TagLiteral') beginning with (@#-@) or (@#=@).
    --
    -- Blocks have a recursive structure; the body of a 'TagBlock' element is
    -- a block context, permitting a tree of blocks.
    Block :: Context

    -- | The body of a 'Paragraph' or 'TagParagraph' block is an inline context.
    -- The types of inlines are 'String', 'SoftBreak', and 'TagInline'.
    --
    -- Inlines have a recursive structure; the body of a 'TagInline' element
    -- is an inline context, permitting a tree of inlines.
    Inline :: Context

-- | When a Prosidy element that has a body (other Prosidy content within the
-- element, that body consists of a list of elements. The 'Size' of a 'Pro'
-- indicates whether the 'Pro' represents a list of elements or a single
-- element within such a list.
--
-- The DataKinds GHC extension lifts 'Size' to a kind and its constructors
-- 'One' and 'Many' to types of the 'Size' kind.
data Size where

    -- | Indicates that a 'Pro' represents a single inline or block element.
    One :: Size

    -- | Only the 'List' constructor has a size of 'Many'.
    Many :: Size

newtype Tag string = Tag string

data Attrs string map = Attrs (map string ()) (map string string)

{- $string

The representation of text within a 'Pro' value is controlled by the @string@ type parameter.

Some reasonable options for this parameter:

  - 'Data.String.String' from the @base@ package
  - 'Data.Text.Text' from the @text@ package
  - 'Data.Text.Lazy.Text' from the @text@ package

-}

{- $list

The representation of sequences within a 'Pro' value is controlled by the @list@ type parameter.

Some reasonable options for this parameter:

  - The built-in @[]@ type
  - 'Data.Sequence.Seq' from the @containers@ package
  - 'Data.Vector.Vector' from the @vector@ package

-}

{- $map

The representation of key-value mappings within a 'Pro' value is controlled by the @map@ type parameter.

Some reasonable options for this parameter:

  - 'Data.Map.Map' from the @containers@ package
  - 'Data.HashMap.HashMap' from the @unordered-containers@ package

-}

type BasePro
  (size :: Size)
  (context :: Context) =
    Pro
      ([] Data.Char.Char)   -- string
      []                    -- list
      (AssociationList [])  -- map
      size
      context

newtype AssociationList list a b =
    AssociationList (list (a, b))
