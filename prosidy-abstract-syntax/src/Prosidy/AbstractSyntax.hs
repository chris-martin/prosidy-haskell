{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, RankNTypes, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

data Pro (size :: Size) (context :: Context)
         (string :: *) (list :: * -> *) (map :: * -> * -> *) where

    Document ::
        Attrs string map
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Pro 'Many 'Block string list map
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line breaks.
      ->
        Pro 'One 'Root string list map
          -- ^ A Prosidy document consists of a head and a body. The first line
          -- containing only three dashes (@---@) separates the head from the body.

    List ::
        list (Pro 'One context string list map)
          -- ^ e.g. a list of blocks or a list of inlines
      ->
        Pro 'Many context string list map
          -- ^ Lists are important in the structure Prosidy (or of any markup
          -- language) because prose is largely linear; a document body is a
          -- list of paragraphs, and paragraphs are lists of words.

    Paragraph ::
        Pro 'Many 'Inline string list map
          -- ^ A list of inline elements (plain text, inline tags, soft breaks).
      ->
        Pro 'One 'Block string list map
          -- ^ A block of text not wrapped in any special notation is a paragraph.

    TagParagraph ::
        TagName string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Pro 'Many 'Inline string list map
          -- ^ Tag body (a list of inline elements)
      ->
        Pro 'One 'Block string list map
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]{inlines}
          -- @

    TagBlocks ::
        TagName string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Pro 'Many 'Block string list map
          -- ^ Tag body (a list of block elements)
      ->
        Pro 'One 'Block string list map
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]:end
          -- blocks
          -- #:end
          -- @

    TagLiteral ::
        TagName string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        string
          -- ^ Tag body (plain text)
      ->
        Pro 'One 'Block string list map
          -- ^ A block of the following form:
          --
          -- @
          -- #+tagname[attrs]:end
          -- text
          -- #:end
          -- @

    TagInline ::
        TagName string
          -- ^ Tag name
      ->
        Attrs string map
          -- ^ Tag attributes
      ->
        Pro 'Many 'Inline string list map
          -- ^ Tag body (a list of inline elements)
      ->
        Pro 'One 'Inline string list map
          -- ^ A tag within a paragraph, of the following form:
          --
          -- @
          -- #tagname[attrs]{inlines}
          -- @

    String ::
        string
          -- ^ Plain text
      ->
        Pro 'One 'Inline string list map
          -- ^ A plain text inline element

    SoftBreak ::
        Pro 'One 'Inline string list map
          -- ^ A line break within a paragraph. When a Prosidy document is rendered
          -- into another format, typically soft breaks are either replaced with
          -- a space character (or, in a CJK writing system, are simply removed).

data Size = One | Many

data Context =
    Root
  | Block
      -- ^ A block is either a 'Paragraph' (text not wrapped in any special notation) or a tag ('TagParagraph', 'TagBlocks', or 'TagLiteral') beginning with (@#+@) or (@#-@).
  | Inline

newtype TagName string = TagName string

data Attrs string map = Attrs (map string ()) (map string string)
