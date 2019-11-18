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

{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies, ExplicitForAll, StandaloneDeriving, DeriveFunctor #-}

{- | The /abstract syntax tree/ is comprised of the semantic components
of a Prosidy document (the text, paragraphs, tags, etc.) without
any of the less significant formatting details (optional whitespace,
order of tag attributes, etc.). This is probably the representation you
want if you are converting between document formats (e.g. Prosidy to
HTML).

If you are going to modify a Prosidy document by parsing, transforming
the parse result, and then rendering back into the Prosidy language,
you may want to consider using the /concrete syntax tree/
("Prosidy.ConcreteSyntax") instead to preserve the formatting of the
original document. -}

module Prosidy.AbstractSyntax
  (
    -------------------------------------------------------------------
    -- * The AST
    Prosidy ( Document, List, Paragraph, TagParagraph, TagBlock,
              TagLiteral, TagInline, StringInline, SoftBreak, Attrs ),
    -------------------------------------------------------------------
    {- * Context -}      Context ( Context ),
    {- ** Size -}        Size ( One, Many ),
    {- ** Level -}       Level ( Root, Block, Inline, Meta ),
    -------------------------------------------------------------------
    {- * Foundation -}   Foundation ( Foundation ),
    {- ** String -}      String,
    {- ** List -}        List,
                         ListBuilding ( listSingleton, listConcat ),
    {- ** Dict -}        Dict,
                         DictBuilding ( dictSingleton, dictConcat ),
    {- ** Base -}        BaseFoundation,
                         AssociationList ( AssociationList ),
    -------------------------------------------------------------------
    -- * JSON
    {- ** JSON AST -}    JS ( JsString, JsList, JsDict ),
    {- ** Keys -}        JsKey ( .. ), JsKeyString ( .. ),
    {- ** Conversion -}  prosidyJS
    -------------------------------------------------------------------
  ) where

import Data.Char (Char)
import Data.Function (($))
import Data.Functor (Functor (fmap))
import Data.Kind (Type)

import qualified Data.List


---  Prosidy  ---

{- | Abstract syntax tree for the Prosidy markup language.

==== Type parameters

[f]: A type of the 'Foundation' kind, specifying which types
    are used to represent strings, lists, and dictionaries.
    We abbreviate this tersely as @f@ because it is ubiquitous
    but largely uninteresting.

[context]: A type of the 'Context' kind (comprised of a 'Size'
    and a 'Level') describing where this particular type of content
    can appear within a Prosidy document. This parameter is how
    we ensure that the tree is well-formed and that the various
    types of content are nested appropriately; for example,
    'Inline' elements can never contain 'Block' elements. -}

data Prosidy (f :: Foundation) (context :: Context) where

    Document ::
        Prosidy f ('Context 'One 'Meta)
          -- ^ The beginning of a Prosidy document is the head.
          -- Each non-empty line of the head is an attribute.
      ->
        Prosidy f ('Context 'Many 'Block)
          -- ^ A Prosidy document body consists of a list of blocks.
          -- Blocks are (typically) separated by two consecutive line
          -- breaks.
      ->
        Prosidy f ('Context 'One 'Root)
          -- ^ A Prosidy document consists of a head and a body. The
          -- first line containing only three dashes (@---@) separates
          -- the head from the body.

    List ::
        List f (Prosidy f ('Context 'One level))
          -- ^ e.g. a list of blocks or a list of inlines
      ->
        Prosidy f ('Context 'Many level)
          -- ^ Lists are important in the structure Prosidy (or of
          -- any markup language) because prose is largely linear;
          -- a document body is a list of paragraphs, and paragraphs
          -- are lists of words.

    Paragraph ::
        Prosidy f ('Context 'Many 'Inline)
          -- ^ A list of inline elements (plain text, inline tags,
          -- soft breaks).
      ->
        Prosidy f ('Context 'One 'Block)
          -- ^ A block of text not wrapped in any special notation
          -- is a paragraph.

    TagParagraph ::
        String f
          -- ^ Tag name
      ->
        Prosidy f ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        Prosidy f ('Context 'Many 'Inline)
          -- ^ Tag body: a list of inlines
      ->
        Prosidy f ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]{inlines}
          -- @

    TagBlock ::
        String f
          -- ^ Tag name
      ->
        Prosidy f ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        Prosidy f ('Context 'Many 'Block)
          -- ^ Tag body: a list of blocks
      ->
        Prosidy f ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #-tagname[attrs]:end
          -- blocks
          -- #:end
          -- @

    TagLiteral ::
        String f
          -- ^ Tag name
      ->
        Prosidy f ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        String f
          -- ^ Tag body: a literal string
      ->
        Prosidy f ('Context 'One 'Block)
          -- ^ A block of the following form:
          --
          -- @
          -- #=tagname[attrs]:end
          -- text
          -- #:end
          -- @

    TagInline ::
        String f
          -- ^ Tag name
      ->
        Prosidy f ('Context 'One 'Meta)
          -- ^ Tag attributes
      ->
        Prosidy f ('Context 'Many 'Inline)
          -- ^ Tag body: a list of inline elements
      ->
        Prosidy f ('Context 'One 'Inline)
          -- ^ A tag within a paragraph, of the following form:
          --
          -- @
          -- #tagname[attrs]{inlines}
          -- @

    StringInline ::
        String f
          -- ^ Plain text
      ->
        Prosidy f ('Context 'One 'Inline)
          -- ^ A plain text inline element

    SoftBreak ::
        Prosidy f ('Context 'One 'Inline)
          -- ^ A line break within a paragraph. When a Prosidy document is
          -- rendered into another format, typically soft breaks are either
          -- replaced with a space character (or, in a CJK writing system,
          -- are simply removed).

    Attrs ::
        Dict f ()
          -- ^ Flags
      ->
        Dict f (String f)
          -- ^ Fields
      ->
        Prosidy f ('Context 'One 'Meta)


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


---  Foundation  ---

data Foundation =
  Foundation
    Type
      -- ^ String
    (Type -> Type)
      -- ^ List
    (Type -> Type)
      -- ^ Dict (map with string keys)

-- | Some reasonable options for this parameter:
--
-- - 'Data.String.String' from the @base@ package
-- - 'Data.Text.Text' from the @text@ package
-- - 'Data.Text.Lazy.Text' from the @text@ package
type family String a where String ('Foundation string list dict) = string

-- | Some reasonable options for this parameter:
--
-- - The built-in @[]@ type
-- - 'Data.Sequence.Seq' from the @containers@ package
-- - 'Data.Vector.Vector' from the @vector@ package
type family List a where List ('Foundation string list dict) = list

-- | Some reasonable options for this parameter:
--
-- - @'AssociationList' [] Data.String.String@
-- - 'Data.Map.Map' from the @containers@ package
-- - 'Data.HashMap.HashMap' from the
--   @unordered-containers@ package
type family Dict a where Dict ('Foundation string list dict) = dict

class Functor list => ListBuilding (list :: Type -> Type)
  where
    listSingleton :: a -> list a
    listConcat :: list a -> list a -> list a

instance ListBuilding []
  where
    listSingleton x = x : []
    listConcat = (Data.List.++)

class Functor dict => DictBuilding (k :: Type) (dict :: Type -> Type) | dict -> k
  where
    dictSingleton :: k -> v -> dict v
    dictConcat :: dict v -> dict v -> dict v

type BaseFoundation =
  'Foundation
    ([] Char)                       -- string
    []                              -- list
    (AssociationList [] ([] Char))  -- dict

newtype AssociationList list a b =
    AssociationList (list (a, b))

deriving instance (Functor list) => Functor (AssociationList list a)

instance (ListBuilding list) => DictBuilding k (AssociationList list k)
  where
    dictSingleton k v = AssociationList (listSingleton (k, v))
    dictConcat (AssociationList a) (AssociationList b) = AssociationList (listConcat a b)


---  JSON  ---

data JS (f :: Foundation) =
    JsString (String f)
  | JsList (List f (JS f))
  | JsDict (Dict f (JS f))

data JsKey = JK_Attr | JK_Body | JK_Type
    | JK_Paragraph | JK_TagParagraph | JK_Tag
    | JK_TagBlock | JK_TagLiteral | JK_TagInline
    | JK_SoftBreak

class JsKeyString (string :: Type)
  where
    jsKeyString :: JsKey -> string

instance JsKeyString ([] Char)
  where
    jsKeyString =
      \case
        JK_Attr         -> "attr"
        JK_Body         -> "body"
        JK_Type         -> "type"
        JK_Paragraph    -> "paragraph"
        JK_TagParagraph -> "tagParagraph"
        JK_Tag          -> "tag"
        JK_TagBlock     -> "tagBlock"
        JK_TagLiteral   -> "tagLiteral"
        JK_TagInline    -> "tagInline"
        JK_SoftBreak    -> "softBreak"

prosidyJS ::
    (
      JsKeyString (String f),
      Functor (List f),
      DictBuilding (String f) (Dict f)
    )
  =>
    Prosidy f context -> JS f

prosidyJS =
  let a + b = dictConcat a b                     ; infixl 5 +
      k .= v = dictSingleton (jsKeyString k) v   ; infixl 6 .=
  in \case

    StringInline x              -> JsString x

    List xs                     -> JsList $ fmap prosidyJS xs

    Document attr body          -> JsDict $ JK_Attr .= prosidyJS attr
                                          + JK_Body .= prosidyJS body

    Paragraph body              -> JsDict $ JK_Type .= JsString (jsKeyString JK_Paragraph)
                                          + JK_Body .= prosidyJS body

    TagParagraph name attr body -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagParagraph)
                                          + JK_Tag  .= JsString name
                                          + JK_Attr .= prosidyJS attr
                                          + JK_Body .= prosidyJS body

    TagBlock name attr body     -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagBlock)
                                          + JK_Tag  .= JsString name
                                          + JK_Attr .= prosidyJS attr
                                          + JK_Body .= prosidyJS body

    TagLiteral name attr body   -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagLiteral)
                                          + JK_Tag  .= JsString name
                                          + JK_Attr .= prosidyJS attr
                                          + JK_Body .= JsString body

    TagInline name attr body    -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagInline)
                                          + JK_Tag  .= JsString name
                                          + JK_Attr .= prosidyJS attr
                                          + JK_Body .= prosidyJS body

    SoftBreak                   -> JsDict $ JK_Type .= JsString (jsKeyString JK_SoftBreak)

    Attrs _flags _fields        -> let x = x in x -- todo
