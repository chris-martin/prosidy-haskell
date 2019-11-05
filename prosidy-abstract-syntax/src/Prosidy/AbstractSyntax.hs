{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, KindSignatures, GADTs, NoImplicitPrelude, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Sequence as List
import qualified Data.Set as Set
import qualified Data.Text as Text

data Pro (size :: Size) (context :: Context) where

    -- | A Prosidy document consists of a head and a body. The first line containing only three dashes (@---@) separates the head from the body.
    Document :: Head -> Body -> Document

    -- | Lists are important in the structure Prosidy (or of any markup language) because prose is largely linear; a document body is a list of paragraphs, and paragraphs are lists of words.
    ProList :: List size (Pro 'One context) -> Pro size context

    -- | A block of text not wrapped in any special notation is a paragraph. A paragraph is comprised of a non-empty list of inline content.
    Paragraph :: Inlines1 -> Block

    -- | A block of the following form:
    --
    -- @
    -- #-tagname[attrs]{inlines}
    -- @
    TagParagraph :: TagName -> Attrs -> Inlines -> Block

    -- | A block of the following form:
    --
    -- @
    -- #-tagname[attrs]:end@
    -- blocks
    -- #:end
    -- @
    TagBlocks :: TagName -> Attrs -> Blocks -> Block

    -- | A block of the following form:
    --
    -- @
    -- #+tagname[attrs]:end@
    -- text
    -- #:end
    -- @
    TagLiteral :: TagName -> Attrs -> Chars -> Block

    -- | A tag within a paragraph, of the following form:
    --
    -- @
    -- #tagname[attrs]{inlines}
    -- @
    TagInline :: TagName -> Attrs -> Inlines -> Inline

    -- | Plain text
    Text :: Chars1 -> Inline

    -- | A line break within a paragraph. When a Prosidy document is rendered into another format, typically soft breaks are either replaced with a space character (or, in a CJK writing system, are simply removed).
    SoftBreak :: Inline

data Size = One | AnyNumber | OneOrMore

data Context = Root | Block | Inline

-- | A Prosidy document consists of a head and a body. The first line containing only three dashes (@---@) separates the head from the body.
type Document = Pro 'One 'Root

-- | The beginning of a Prosidy document is the head. Each non-empty line of the head is an attribute.
type Head = Attrs

-- | A Prosidy document body consists of a list of blocks. Blocks are (typically) separated by two consecutive line breaks.
type Body = Blocks

-- | A block is either a 'Paragraph' (text not wrapped in any special notation) or a tag ('TagParagraph', 'TagBlocks', or 'TagLiteral') beginning with (@#+@) or (@#-@).
type Block = Pro 'One 'Block

type Blocks = Pro 'AnyNumber 'Block
type Blocks1 = Pro 'OneOrMore 'Block
type Inline = Pro 'One 'Inline
type Inlines = Pro 'AnyNumber 'Inline
type Inlines1 = Pro 'OneOrMore 'Inline

newtype TagName = TagName Chars1
data Attrs = Attrs (Set Flag) (Map FieldName FieldValue)
newtype Flag = Flag Chars1
newtype FieldName = FieldName Chars1
newtype FieldValue = FieldValue Chars

-- | A finite sequence of elements, with the parameter @size@ constraining how many elements are in the sequence, and the parameter @element@ indicating the type of the elements. See "Data.Sequence".
data List (size :: Size) (element :: *) where

    -- | A possibly-empty list.
    List0 :: List.Seq element -> List 'AnyNumber element

    -- | A non-empty list. This constructor is designated "unsafe" because it is the user's responsibility to ensure that this list is actually non-empty.
    List1_Unsafe :: List.Seq element -> List 'OneOrMore element

    -- | A possibly-empty packed list of characters. See "Data.Text".
    Text0 :: Text.Text -> Chars

    -- | A non-empty packed list of characters. This constructor is designated "unsafe" because it is the user's responsibility to ensure that this list is actually non-empty.
    Text1_Unsafe :: Text.Text -> Chars1

-- | A single character.
type Char = Char.Char

-- | A possibly-empty list of characters.
type Chars = List 'AnyNumber Char

-- | A non-empty list of characters.
type Chars1 = List 'OneOrMore Char

-- | A possibly-empty map from type @key@ to type @value@, sorted by key. See "Data.Map".
type Map key value = Map.Map key value

-- | A possibly-empty sorted collection of distinct @element@s. See "Data.Set".
type Set element = Set.Set element
