{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving #-}

module Prosidy.AbstractSyntax (
    {- * Document -} Document (..),
    {- * Head -} Head (..),
    {- * Body -} Body (..),
    {- * Tags -} Tag (..), TagName (..),
    {- * Block-level content -} Block (..),
    {- * Paragraphs -} Paragraph0 (..), Paragraph1 (..),
    {- * Tags at the block level -} TagBlock, TagBlockBody (..),
    {- * Inline-level content -} Inline (..), InlineText1 (..),
    {- * Tags at the inline level -} TagInline, TagInlineBody (..),
    {- * Literals -} Literal (..),
    {- * Attributes -} Attrs (..),
    {- * Fields -} Field (..), Fields (..), FieldName (..), FieldValue (..),
    {- * Flags -} Flag (..), Flags (..),
    {- * Lists -} List0, List1 (..), {- $lists -}
    {- * Maps -} Map0, {- $maps -}
    {- * Sets -} Set0, {- $sets -}
    {- * Text -} Text0, Text1 (..)
  ) where

import qualified Data.Map as Map
import qualified Data.Sequence as List
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | A possible-empty list.
type List0 element = List.Seq element

-- | A non-empty list.
newtype List1 element = List1_Unsafe (List0 element) -- ^ The constructor is marked unsafe because it is the user's responsibility to ensure that this list is actually non-empty.

{- $lists For more on lists, see "Data.Sequence". -}

-- | A possibly-empty enumeration of mappings from key to value.
type Map0 key value = Map.Map key value

{- $maps For more on maps, see "Data.Map". -}

-- | A possibly-empty collection of unique elements.
type Set0 element = Set.Set element

{- $sets For more on sets, see "Data.Set". -}

-- | A possibly-empty list of characters.
type Text0 = Text.Text

-- | A non-empty list of characters.
newtype Text1 = Text1_Unsafe Text0  -- ^ The constructor is marked unsafe because it is the user's responsibility to ensure that this text is actually non-empty.

{- $text For more on text, see "Data.Text". -}

-- | The first line containing only three dashes (---) separates the 'Head' from the 'Body'.
data Document = Document
    { docHead :: Head
    , docBody :: Body
    }

-- | The beginning of a prodisy document is the head. Each non-empty line of the head is a 'Field' or a 'Flag'.
newtype Head = Head Attrs

-- | A Prosidy document body consists of a list of blocks. Blocks are (typically) separated by two consecutive line breaks.
newtype Body = Body (List0 Block)

-- | There are two types of blocks:
data Block
    = Block_Para    Paragraph1  -- ^ Text not wrapped in any special notation is a paragraph.
    | Block_Tag     TagBlock    -- ^ A line beginning in @#+@ or @#-@ opens a block-level tag.

-- | A paragraph is a list of inline segments.
newtype Paragraph0 = Paragraph0 (List0 Inline)

-- | A non-empty paragraph.
newtype Paragraph1 = Paragraph1 (List1 Inline)

-- | A tag at the 'Block' level.
type TagBlock = Tag TagBlockBody

-- | A block-level tag may contain one of three things:
data TagBlockBody
    = TagBlock_Para  Paragraph0  -- ^ When a tag opened with @(#-)@ is followed by curly brackets {...}, the brackets enclose a possibly-empty paragraph comprising the tag body.
    | TagBlock_Doc   Body        -- ^ When a tag opened with @(#-)@ is not followed by curly brackets, it begins a nested document body and is later closed with (@#:@).
    | TagBlock_Lit   Literal     -- ^ A tag opened with @(#+)@ begins a literal.

-- | Three types of things can appear within a paragraph:
data Inline
    = Inline_Text   InlineText1  -- ^ Plain text
    | Inline_Tag    TagInline    -- ^ A tag, which begins with (@#@)
    | Inline_Break               -- ^ A line break

-- | Text within a paragraph. This text can contain no line breaks (which are encoded separately as 'Inline_Break').
newtype InlineText1 = InlineText1_Unsafe Text1 -- ^ The constructor is marked unsafe because it is the user's responsibility to ensure that this text actually contains no line breaks.

-- | A tag at the 'Inline' level. When a tag has curly brackets {...} then the brackets enclose a paragraph which comprises the tag body.
type TagInline = Tag TagInlineBody

newtype TagInlineBody = TagInlineBody Paragraph0

-- | Text that matches verbatim with the corresponding Prosidy source.
newtype Literal = Literal Text0

-- | In this example tag:
--
-- @
-- #ingredient[bold, amount='25', unit=\'g']{flour}
-- @
--
-- * @ingredient@ is the 'TagName'
-- * @[bold, amount='25', unit=\'g']@ are the 'Attrs'
-- * @{flour}@ is the body
--
data Tag body = Tag
    { tagName   :: TagName
    , tagAttrs  :: Attrs  -- ^ A block tag may optionally include square brackets [...] containing attributes.
    , tagBody   :: body
    }

newtype TagName = TagName Text1

newtype Flag = Flag Text1

newtype Flags = Flags (Set0 Flag)

data Field = Field
    { fieldName    :: FieldName
    , fieldValue   :: FieldValue
    }

newtype Fields = Fields (Map0 FieldName FieldValue)

newtype FieldName = FieldName Text1

newtype FieldValue = FieldValue Text0

-- | In this example tag:
--
-- @
-- #ingredient[bold, amount='25', unit=\'g']{flour}
-- @
--
-- * @bold@ is a 'Flag'
-- * @amount='25' and unit=\'g'@ are 'Field's
--
data Attrs = Attrs
    { attrsFlags    :: Flags
    , attrsFields   :: Fields
    }
