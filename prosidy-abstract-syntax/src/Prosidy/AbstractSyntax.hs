{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, RankNTypes, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

data Pro (size :: Size) (context :: Context)
         (string :: *) (list :: * -> *) (map :: * -> * -> *) where

    -- | A Prosidy document consists of a head and a body. The first line
    -- containing only three dashes (@---@) separates the head from the body.
    Document ::
        Attrs string map
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Pro 'Many 'BlockCtx string list map
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line breaks.
      ->
        Pro 'One 'RootCtx string list map

    -- | Lists are important in the structure Prosidy (or of any markup language)
    -- because prose is largely linear; a document body is a list of paragraphs,
    -- and paragraphs are lists of words.
    List ::
        list (Pro 'One context string list map)
      ->
        Pro 'Many context string list map

    -- | A block of text not wrapped in any special notation is a paragraph.
    -- A paragraph is comprised of a non-empty list of inline content.
    Paragraph ::
        Pro 'Many 'InlineCtx string list map
      ->
        Pro 'One 'BlockCtx string list map

    -- | A block of the following form:
    --
    -- @
    -- #-tagname[attrs]{inlines}
    -- @
    TagParagraph ::
        (forall list' map'. Pro 'One 'TagNameCtx string list' map')
      ->
        Attrs string map
      ->
        Pro 'Many 'InlineCtx string list map
      ->
        Pro 'One 'BlockCtx string list map

    -- | A block of the following form:
    --
    -- @
    -- #-tagname[attrs]:end
    -- blocks
    -- #:end
    -- @
    TagBlocks ::
        (forall list' map'. Pro 'One 'TagNameCtx string list' map')
      ->
        Attrs string map
      ->
        Pro 'Many 'BlockCtx string list map
      ->
        Pro 'One 'BlockCtx string list map

    -- | A block of the following form:
    --
    -- @
    -- #+tagname[attrs]:end
    -- text
    -- #:end
    -- @
    TagLiteral ::
        (forall list' map'. Pro 'One 'TagNameCtx string list' map')
      ->
        Attrs string map
      ->
        string
      ->
        Pro 'One 'BlockCtx string list map

    -- | A tag within a paragraph, of the following form:
    --
    -- @
    -- #tagname[attrs]{inlines}
    -- @
    TagInline ::
        (forall list' map'. Pro 'One 'TagNameCtx string list' map')
      ->
        Attrs string map
      ->
        Pro 'Many 'InlineCtx string list map
      ->
        Pro 'One 'InlineCtx string list map

    -- | Plain text
    String ::
        string
      ->
        Pro 'One 'InlineCtx string list map

    -- | A line break within a paragraph. When a Prosidy document is rendered
    -- into another format, typically soft breaks are either replaced with
    -- a space character (or, in a CJK writing system, are simply removed).
    SoftBreak ::
        Pro 'One 'InlineCtx string list map

    TagName ::
        string
      ->
        Pro 'One 'TagNameCtx string list map

data Size = One | Many

data Context =
    RootCtx
  | BlockCtx
      -- ^ A block is either a 'Paragraph' (text not wrapped in any special notation) or a tag ('TagParagraph', 'TagBlocks', or 'TagLiteral') beginning with (@#+@) or (@#-@).
  | InlineCtx
  | TagNameCtx

data Attrs string map = Attrs (map string ()) (map string string)
