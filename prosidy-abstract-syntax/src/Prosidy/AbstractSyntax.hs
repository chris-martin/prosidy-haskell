{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, RankNTypes, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

data Pro
  (string :: *) (list :: * -> *) (attrs :: * -> *)
  (size :: Size) (context :: Context)
    where

    Document ::
        attrs string
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Pro string list attrs 'Many 'Block
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line breaks.
      ->
        Pro string list attrs 'One 'Root
          -- ^ A Prosidy document consists of a head and a body. The first line
          -- containing only three dashes (@---@) separates the head from the body.

    List ::
        list (Pro string list attrs 'One context)
          -- ^ e.g. a list of blocks or a list of inlines
      ->
        Pro string list attrs 'Many context
          -- ^ Lists are important in the structure Prosidy (or of any markup
          -- language) because prose is largely linear; a document body is a
          -- list of paragraphs, and paragraphs are lists of words.

    Paragraph ::
        Pro string list attrs 'Many 'Inline
          -- ^ A list of inline elements (plain text, inline tags, soft breaks).
      ->
        Pro string list attrs 'One 'Block
          -- ^ A block of text not wrapped in any special notation is a paragraph.

    TagParagraph ::
        Tag string
          -- ^ Tag name
      ->
        attrs string
          -- ^ Tag attributes
      ->
        Pro string list attrs 'Many 'Inline
          -- ^ Tag body (a list of inline elements)
      ->
        Pro string list attrs 'One 'Block
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]{inlines}
          -- @

    TagBlocks ::
        Tag string
          -- ^ Tag name
      ->
        attrs string
          -- ^ Tag attributes
      ->
        Pro string list attrs 'Many 'Block
          -- ^ Tag body (a list of block elements)
      ->
        Pro string list attrs 'One 'Block
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
        attrs string
          -- ^ Tag attributes
      ->
        string
          -- ^ Tag body (plain text)
      ->
        Pro string list attrs 'One 'Block
          -- ^ A block of the following form:
          --
          -- @
          -- #+tagname[attrs]:end
          -- text
          -- #:end
          -- @

    TagInline ::
        Tag string
          -- ^ Tag name
      ->
        attrs string
          -- ^ Tag attributes
      ->
        Pro string list attrs 'Many 'Inline
          -- ^ Tag body (a list of inline elements)
      ->
        Pro string list attrs 'One 'Inline
          -- ^ A tag within a paragraph, of the following form:
          --
          -- @
          -- #tagname[attrs]{inlines}
          -- @

    String ::
        string
          -- ^ Plain text
      ->
        Pro string list attrs 'One 'Inline
          -- ^ A plain text inline element

    SoftBreak ::
        Pro string list attrs 'One 'Inline
          -- ^ A line break within a paragraph. When a Prosidy document is rendered
          -- into another format, typically soft breaks are either replaced with
          -- a space character (or, in a CJK writing system, are simply removed).

data Size = One | Many

data Context =
    Root
  | Block
      -- ^ A block is either a 'Paragraph' (text not wrapped in any special notation) or a tag ('TagParagraph', 'TagBlocks', or 'TagLiteral') beginning with (@#+@) or (@#-@).
  | Inline

newtype Tag string = Tag string

data Attrs a map = Attrs (map a ()) (map a a)
