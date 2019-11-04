{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving #-}

module Prosidy.AbstractSyntax where

import qualified Data.Map as Map
import qualified Data.Maybe as May
import qualified Data.Sequence as List
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | Either 'a' or nothing.
type Maybe a = May.Maybe a

-- | A possible-empty list.
type List0 element = List.Seq element

-- | A non-empty list.
newtype List1 element = List1_Unsafe (List0 element) -- ^ The constructor is marked unsafe because it is the user's responsibility to ensure that this list is actually non-empty.

-- | A possibly-empty enumeration of mappings from key to value.
type Map0 key value = Map.Map key value

-- | A possibly-empty collection of unique elements.
type Set0 element = Set.Set element

-- | A possibly-empty list of characters.
type Text0 = Text.Text

-- | A non-empty list of characters.
newtype Text1 = Text1_Unsafe Text0  -- ^ The constructor is marked unsafe because it is the user's responsibility to ensure that this text is actually non-empty.

data Document = Document
    { docHead :: Head
    , docBody :: Body
    }

newtype Head = Head Attrs

-- | A prosidy document body consists of a list of blocks. Blocks are (typically) separated by two consecutive line breaks.
newtype Body = Body (List0 Block)

-- | There are two types of blocks:
data Block
    = Block_Para    Paragraph1  -- ^ Text not wrapped in any special notation is a paragraph.
    | Block_Tag     TagBlock    -- ^ A line beginning in #+ or #- opens a block-level tag.

-- | A paragraph is a list of inline segments.
newtype Paragraph0 = Paragraph0 (List0 Inline)

-- | A non-empty paragraph.
newtype Paragraph1 = Paragraph1 (List1 Inline)

-- | A tag at the 'Block' level.
type TagBlock = Tag TagBlockBody

-- | A block-level tag may contain one of three things:
data TagBlockBody
    = TagBlock_Para  Paragraph0  -- ^ When a tag opened with #- is followed by curly brackets {...}, the brackets enclose a possibly-empty paragraph comprising the tag body.
    | TagBlock_Doc   Body        -- ^ When a tag opened with #- is not followed by curly brackets, it begins a nested document body and is later closed with (#:).
    | TagBlock_Lit   Literal     -- ^ A tag opened with #+ begins a literal.

-- | Three types of things can appear within a paragraph:
data Inline
    = Inline_Text   InlineText1  -- ^ Plain text
    | Inline_Tag    TagInline    -- ^ A tag, which begins with (#)
    | Inline_Break               -- ^ A line break

-- | Text within a paragraph. This text can contain no line breaks (which are encoded separately as 'Inline_Break').
newtype InlineText1 = InlineText1_Unsafe Text1 -- ^ The constructor is marked unsafe because it is the user's responsibility to ensure that this text actually contains no line breaks.

-- | A tag at the 'Inline' level. When a tag has curly brackets {...} then the brackets enclose a paragraph which comprises the tag body.
type TagInline = Tag Paragraph0

-- | Text that matches verbatim with the corresponding prosidy source.
newtype Literal = Literal Text0

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

data Attrs = Attrs
    { attrsFlags    :: Flags
    , attrsFields   :: Fields
    }
