{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

{-# LANGUAGE GADTs #-}
    {- The 'Prosidy' type is a GADT; its data constructors have
       different type parameters than the 'Prosidy' type itself.
       This requires enabling the GADTs language extension. -}

{-# LANGUAGE DataKinds #-}
    {- The data kinds extension turns types into kinds and
       data constructors into type constructors. We use this to
       establish the kinds 'Context', 'Size', and 'Level' which
       parameterize the 'Prosidy' type. -}

{-# LANGUAGE KindSignatures #-}
    {- We use the kind signatures extension to annotate type
       parameters with their kind using (::). This is just like
       a type annotation, but what follows the colons is a kind
       rather than a type. -}

{-# LANGUAGE NoImplicitPrelude #-}
    {- This module depends on as little library code as possible,
       even from the base package. Disabling the implicit import
       of the Prelude module makes this more clear. -}

module Prosidy.AbstractSyntax
  (
    -- * Prosidy content
    Prosidy
        ( Document, List, Paragraph, TagParagraph, TagBlock,
          TagLiteral, TagInline, String, SoftBreak, Attrs ),

    Deps (..), -- todo: figure out where this goes

    -- * Content context: size and level
    Context
        ( Context ),
    Size
        ( One, Many ),
    Level
        ( Root, Block, Inline, Meta )

    -- * Other types: string, list, map
    -- $otherTypes

  ) where

import Data.Kind (Type)

-- todo: figure out a better name for this
data Deps =
  Deps
    Type -- ^ String
    (Type -> Type) -- ^ List
    (Type -> Type -> Type) -- ^ Map


---  Prosidy  ---

-- | 'Prosidy' is the type of Prosidy content.

data Prosidy
  (deps :: Deps)
  (context :: Context)
    where

    Document ::
        Prosidy deps ('Context 'One 'Meta)
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Prosidy deps ('Context 'Many 'Block)
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line
          -- breaks.
      ->
        Prosidy deps ('Context 'One 'Root)
          -- ^ A Prosidy document consists of a head and a body. The
          -- first line containing only three dashes (@---@) separates
          -- the head from the body.

    List :: (deps ~ 'Deps string list map) =>
        list (Prosidy deps ('Context 'One level))
          -- ^ e.g. a list of blocks or a list of inlines
      ->
        Prosidy deps ('Context 'Many level)
          -- ^ Lists are important in the structure Prosidy (or of
          -- any markup language) because prose is largely linear;
          -- a document body is a list of paragraphs, and paragraphs
          -- are lists of words.

    Paragraph ::
        Prosidy deps ('Context 'Many 'Inline)
          -- ^ A list of inline elements (plain text, inline tags,
          -- soft breaks).
      ->
        Prosidy deps ('Context 'One 'Block)
          -- ^ A block of text not wrapped in any special notation
          -- is a paragraph.

    TagParagraph :: (deps ~ 'Deps string list map) =>
        string
          -- ^ Tag name
      ->
        Prosidy deps ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        Prosidy deps ('Context 'Many 'Inline)
          -- ^ Tag body: a list of inlines
      ->
        Prosidy deps ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]{inlines}
          -- @

    TagBlock :: (deps ~ 'Deps string list map) =>
        string
          -- ^ Tag name
      ->
        Prosidy deps ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        Prosidy deps ('Context 'Many 'Block)
          -- ^ Tag body: a list of blocks
      ->
        Prosidy deps ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]:end
          -- blocks
          -- #:end
          -- @

    TagLiteral :: (deps ~ 'Deps string list map) =>
        string
          -- ^ Tag name
      ->
        Prosidy deps ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        string
          -- ^ Tag body: a literal string
      ->
        Prosidy deps ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #=tagname[attrs]:end
          -- text
          -- #:end
          -- @

    TagInline :: (deps ~ 'Deps string list map) =>
        string
          -- ^ Tag name
      ->
        Prosidy deps ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        Prosidy deps ('Context 'Many 'Inline)
          -- ^ Tag body: a list of inline elements
      ->
        Prosidy deps ('Context 'One 'Inline)
          -- ^ A tag within a paragraph, of the following form:
          --
          -- @
          -- #tagname[attrs]{inlines}
          -- @

    String :: (deps ~ 'Deps string list map) =>
        string
          -- ^ Plain text
      ->
        Prosidy deps ('Context 'One 'Inline)
          -- ^ A plain text inline element

    SoftBreak ::
        Prosidy deps ('Context 'One 'Inline)
          -- ^ A line break within a paragraph. When a Prosidy document is
          -- rendered into another format, typically soft breaks are either
          -- replaced with a space character (or, in a CJK writing system,
          -- are simply removed).

    Attrs :: (deps ~ 'Deps string list map) =>
        map string ()
          -- ^ Flags
      ->
        map string string
          -- ^ Fields
      ->
        Prosidy deps ('Context 'One 'Meta)


---  Context  ---

{- | The context of a 'Prosidy' indicates what kind of place it is allowed to
appear within the syntax tree.

The DataKinds GHC extension lifts the 'Context' type to a kind and the
'Context' data type constructor to a type constructor. -}

data Context = Context Size Level


---  Level  ---

{- | The level of a 'Prosidy' indicates what kind of elements it may be
nested within.

The DataKinds GHC extension lifts 'Level' to a kind and its constructors
'Root', 'Block' and 'Inline' to types of the 'Level' kind. -}

data Level
  where

    -- | The top level containins everything else.
    -- Only a 'Document' appears at the 'Root' level.
    Root :: Level

    -- | The document body is at the block level. A block is either
    -- a 'Paragraph' (text not wrapped in any special notation) or
    -- a tag ('TagParagraph', 'TagBlock', or 'TagLiteral') beginning
    -- with (@#-@) or (@#=@).
    --
    -- Blocks have a recursive structure; the body of a 'TagBlock'
    -- element is block-level, permitting a tree of blocks.
    Block :: Level

    -- | The body of a 'Paragraph' or 'TagParagraph' block is at the
    -- inline level. The types of inlines are 'String', 'SoftBreak',
    -- and 'TagInline'.
    --
    -- Inlines have a recursive structure; the body of a 'TagInline'
    -- element has an inline level, permitting a tree of inlines.
    Inline :: Level

    -- | The meta level consists of the document header and all
    -- content within @[@...@]@ brackets. 'Attrs' is the only
    -- type of content at the meta level.
    Meta :: Level


---  Size  ---

{- | When a Prosidy element that has a body (other Prosidy content
within the element), that body consists of a list of elements.
The size of a 'Prosidy' indicates whether the 'Prosidy' represents
a list of elements or a single element within such a list.

The DataKinds GHC extension lifts 'Size' to a kind and its
constructors 'One' and 'Many' to types of the 'Size' kind. -}

data Size
  where

    -- | Indicates that a 'Prosidy' value represents a
    -- single inline or block element. This is the size
    -- of most 'Prosidy' constructors.
    One :: Size

    -- | Only the 'List' constructor has an size of 'Many'.
    Many :: Size


{- $otherTypes

=== String

The representation of text within a 'Prosidy' value
is controlled by the @string@ type parameter.

Some reasonable options for this parameter:

  - 'Data.String.String' from the @base@ package
  - 'Data.Text.Text' from the @text@ package
  - 'Data.Text.Lazy.Text' from the @text@ package

=== List

The representation of sequences within a 'Prosidy' value
is controlled by the @list@ type parameter.

Some reasonable options for this parameter:

  - The built-in @[]@ type
  - 'Data.Sequence.Seq' from the @containers@ package
  - 'Data.Vector.Vector' from the @vector@ package

=== Map

The representation of key-value mappings within a 'Prosidy'
value is controlled by the @map@ type parameter.

Some reasonable options for this parameter:

  - 'Data.Map.Map' from the @containers@ package
  - 'Data.HashMap.HashMap' from the
    @unordered-containers@ package

-}
