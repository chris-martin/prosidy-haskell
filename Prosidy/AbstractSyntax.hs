{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-top-binds
                -fdefer-typed-holes -fno-warn-typed-holes #-}
    {- These are temporary warning suppressions while this module
       undergoes heavily development. (todo: remove this pragma) -}

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

{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies, ExplicitForAll, StandaloneDeriving, DeriveFunctor, TypeApplications, ScopedTypeVariables, QuantifiedConstraints, RankNTypes, LiberalTypeSynonyms, UndecidableInstances #-}

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
    {- ** ... -}         ListOfDictKeys ( listOfDictKeys ),
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
import qualified Data.Char as Char
import Data.Eq (Eq)
import Data.Function (($), fix, id)
import qualified Data.List as List
import System.IO (IO)

-- Either
import Data.Either (Either)
import qualified Data.Either as Either

-- Maybe
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

-- Things related to type-level programming
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))

-- Functors
import Data.Functor (Functor (fmap))
import qualified Control.Applicative
import Control.Applicative (Applicative (pure))
import Control.Monad (Monad ((>>=), return))

-- Numbers
import Data.Int (Int)
import Data.Ratio (Rational)
import Data.Word (Word8, Word64)
import Numeric.Natural (Natural)
import Prelude (Integer)
import qualified Prelude


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
    listConcat = (List.++)

data ListDirection = ListForward | ListBackward

class ListTraversal list
  where
    listTraversal :: ListDirection -> Traversal (list a) (list b) a b

instance ListTraversal []
  where
    listTraversal ListForward = Prelude.traverse
    listTraversal ListBackward = \action xs -> Prelude.traverse action (List.reverse xs)

class Functor dict => DictBuilding (k :: Type) (dict :: Type -> Type) | dict -> k
  where
    dictSingleton :: k -> v -> dict v
    dictConcat :: dict v -> dict v -> dict v

class ListOfDictKeys (f :: Foundation)
  where
    listOfDictKeys :: Proxy f -> Dict f v -> List f (String f)

-- | A minimal specialization of 'Foundation' using only types available in library
-- the @base@. This option is simple, but perhaps not the most performant choice.
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


---  Isomorphism concept  ---

type Iso s t a b = (s -> a, b -> t)
type Iso' s a = (s -> a, a -> s)

overIso :: Iso s t a b -> (a -> b) -> (s -> t)
overIso (f, g) h x = g (h (f x))

viewIso :: Iso s t a b -> s -> a
viewIso (f, _) = f

viewIso' :: Iso' s a -> s -> a
viewIso' (f, _) = f


---  Lenses  ---

type Lens s t a b = (s -> a, s -> b -> t)
type Lens' s a = (s -> a, s -> a -> s)

documentHeadLens :: Lens' (Prosidy f ('Context 'One 'Root)) (Prosidy f ('Context 'One 'Meta))
documentHeadLens = (\(Document head _) -> head, \(Document _ body) head -> Document head body)

documentBodyLens :: Lens' (Prosidy f ('Context 'One 'Root)) (Prosidy f ('Context 'Many 'Block))
documentBodyLens = (\(Document _ body) -> body, \(Document head _) body -> Document head body)


---  Prisms  ---

type Prism s t a b = (s -> Either t a, b -> t)
type Prism' s a = (s -> Maybe a, a -> s)


---  Traversals  ---

type Traversal s t a b = forall f. (Monad f) => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

isoTraversal :: forall s t u v a b. Iso s t u v -> Traversal u v a b -> Traversal s t a b
isoTraversal (convert, convertBack) traverse action (s :: s) =
  do
    (v :: v) <- traverse action (convert s :: u)
    return (convertBack v :: t)

lensTraversal :: forall s t u v a b. Lens s t u v -> Traversal u v a b -> Traversal s t a b
lensTraversal (getPart, reassemble) traverse action (s :: s) =
  do
    (v :: v) <- traverse action (getPart s :: u)
    return (reassemble s v :: t)

traversalTraversal :: forall s t u v a b. Traversal s t u v -> Traversal u v a b -> Traversal s t a b
traversalTraversal traverse1 traverse2 action (s :: s) = traverse1 (traverse2 action) s

nilTraversal :: Traversal s s a b
nilTraversal _ s = pure s

idTraversal :: Traversal' s s
idTraversal f s = f s

prosidyListIso :: Iso' (Prosidy f ('Context 'Many l)) (List f (Prosidy f ('Context 'One l)))
prosidyListIso = (\(List x) -> x, List)

prosidyListTraversal :: ListTraversal (List f) => ListDirection -> Traversal' (Prosidy f ('Context 'Many l)) (Prosidy f ('Context 'One l))
prosidyListTraversal direction = prosidyListIso `isoTraversal` listTraversal direction

blockChildrenTraversal :: Traversal' (Prosidy f ('Context 'One 'Block)) (Prosidy f ('Context 'Many 'Block))
blockChildrenTraversal action block =
    case block of
        TagBlock name attrs children -> fmap (TagBlock name attrs) (action children)
        _ -> pure block

eachBlockChild :: ListTraversal (List f) => BlockDirection -> Traversal' (Prosidy f ('Context 'One 'Block)) (Prosidy f ('Context 'One 'Block))
eachBlockChild blockDirection = blockChildrenTraversal `traversalTraversal` prosidyListTraversal (viewIso' blockListDirectionIso blockDirection)

data TreeDirection = RootToLeaf | LeafToRoot
data BlockDirection = TopToBottom | BottomToTop
data InlineDirection = LeftToRight | RightToLeft

blockListDirectionIso :: Iso' BlockDirection ListDirection
blockListDirectionIso = (\case TopToBottom -> ListForward; BottomToTop -> ListBackward, \case ListForward -> TopToBottom; ListBackward -> BottomToTop)

class BlockTreeTraversal size level
  where
    blockTreeTraversal :: ListTraversal (List f) => TreeDirection -> BlockDirection -> Traversal' (Prosidy f ('Context size level)) (Prosidy f ('Context 'One 'Block))

instance BlockTreeTraversal 'One 'Root
  where
    blockTreeTraversal treeDirection blockDirection = documentBodyLens `lensTraversal` blockTreeTraversal treeDirection blockDirection

instance BlockTreeTraversal 'One 'Block
  where
    blockTreeTraversal treeDirection blockDirection action block =
        case treeDirection of
            RootToLeaf ->
              do
                block' <- action block
                eachBlockChild blockDirection action block'
            LeafToRoot ->
              do
                block' <- eachBlockChild blockDirection action block
                action block'

instance BlockTreeTraversal 'Many 'Block
        where
    blockTreeTraversal treeDirection blockDirection = prosidyListTraversal (viewIso' blockListDirectionIso blockDirection) `traversalTraversal` blockTreeTraversal treeDirection blockDirection


---  JSON  ---

{- | A fairly generic data structure that resembles an abstract syntax
tree for JSON, minus a few aspects of JSON that are irrelevant for our
purposes here. -}

data JS (f :: Foundation) =
    JsString (String f)     -- ^ e.g. @"hello"@
  | JsList (List f (JS f))  -- ^ e.g. @["one", "two", "three"]@
  | JsDict (Dict f (JS f))  -- ^ e.g. @{"numeral": "4", "word": "four"}@

deriving instance
  (
      Eq string,
      forall a. Eq a => Eq (list a),
      forall a. Eq a => Eq (dict a)
  )
  => Eq (JS ('Foundation string list dict))

data JsKey = JK_Attr | JK_Body | JK_Type
    | JK_Paragraph | JK_TagParagraph | JK_Tag
    | JK_TagBlock | JK_TagLiteral | JK_TagInline
    | JK_SoftBreak | JK_Flags | JK_Fields

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
        JK_Flags        -> "flags"
        JK_Fields       -> "fields"

prosidyJS :: forall (f :: Foundation) (context :: Context).
    (
      JsKeyString (String f),
      Functor (List f),
      DictBuilding (String f) (Dict f),
      ListOfDictKeys f
    )
  =>
    Prosidy f context -> JS f

prosidyJS =
  let a + b = dictConcat a b                     ; infixl 5 +
      k .= v = dictSingleton (jsKeyString k) v   ; infixl 6 .=
      f = Proxy @f
  in \case

    StringInline x              -> JsString x

    List xs                     -> JsList $ fmap prosidyJS xs

    Document attr body          -> JsDict $ JK_Attr   .= prosidyJS attr
                                          + JK_Body   .= prosidyJS body

    Paragraph body              -> JsDict $ JK_Type   .= JsString (jsKeyString JK_Paragraph)
                                          + JK_Body   .= prosidyJS body

    TagParagraph name attr body -> JsDict $ JK_Type   .= JsString (jsKeyString JK_TagParagraph)
                                          + JK_Tag    .= JsString name
                                          + JK_Attr   .= prosidyJS attr
                                          + JK_Body   .= prosidyJS body

    TagBlock name attr body     -> JsDict $ JK_Type   .= JsString (jsKeyString JK_TagBlock)
                                          + JK_Tag    .= JsString name
                                          + JK_Attr   .= prosidyJS attr
                                          + JK_Body   .= prosidyJS body

    TagLiteral name attr body   -> JsDict $ JK_Type   .= JsString (jsKeyString JK_TagLiteral)
                                          + JK_Tag    .= JsString name
                                          + JK_Attr   .= prosidyJS attr
                                          + JK_Body   .= JsString body

    TagInline name attr body    -> JsDict $ JK_Type   .= JsString (jsKeyString JK_TagInline)
                                          + JK_Tag    .= JsString name
                                          + JK_Attr   .= prosidyJS attr
                                          + JK_Body   .= prosidyJS body

    SoftBreak                   -> JsDict $ JK_Type   .= JsString (jsKeyString JK_SoftBreak)

    Attrs flags fields          -> JsDict $ JK_Flags  .= JsList (fmap JsString (listOfDictKeys f flags))
                                          + JK_Fields .= JsDict (fmap JsString fields)


---  Generation  ---

data Gen entropy a =
      GenAwait (entropy -> Gen entropy a)
    | GenYield a (Gen entropy a)
    deriving Functor

-- | A generator that ignores its entropy and always yields the same fixed value.
genConst :: a -> Gen entropy a
genConst x = fix (GenYield x)

-- | A generator that simply yields its entropy values unmodified.
genId :: Gen a a
genId = fix (\g -> GenAwait (\e -> GenYield e g))

-- | Function application within the 'Gen' context.
genAp :: Gen entropy (a -> b) -> Gen entropy a -> Gen entropy b
genAp (GenYield f genF) (GenYield x genX) = GenYield (f x) (genAp genF genX)
genAp (GenAwait toGenF) genX = GenAwait (\e -> let genF = toGenF e in genAp genF genX)
genAp genF (GenAwait toGenX) = GenAwait (\e -> let genX = toGenX e in genAp genF genX)

-- | Arrow composition of generators. @'genCompose' f g@
-- uses the values generated by @f@ as the entropy for @g@.
genCompose :: Gen a b -> Gen b c -> Gen a c
genCompose f (GenYield c g) = GenYield c (genCompose f g)
genCompose (GenYield b f) (GenAwait toC) = genCompose f (toC b)
genCompose (GenAwait toB) g = GenAwait (\a -> genCompose (toB a) g)

genWord8AsciiPrint :: Gen Word8 Char
genWord8AsciiPrint =
    fix (\g -> GenAwait (\b -> let c = word8Char b in
        if Char.isPrint c then GenYield c g else g))

word8Int :: Word8 -> Int
word8Int = Prelude.fromIntegral

word8Char :: Word8 -> Char
word8Char b = Char.chr (word8Int b)

genDocument :: (context ~ ('Context 'One 'Root)) =>
    GenOptions context -> Gen entropy (Prosidy f context)
genDocument _ = _

genBlock :: (context ~ ('Context 'One 'Block)) =>
    GenOptions context -> Gen entropy (Prosidy f context)
genBlock _ = _

genInline :: (context ~ ('Context 'One 'Inline)) =>
    GenOptions context -> Gen entropy (Prosidy f context)
genInline _ = _

type GenOptions context = forall a. GenOption context a -> a

data GenOption (context :: Context) a
  where

    GenMaxTotalBlocks :: GenOption ('Context 'One 'Root) Natural
      -- ^ The maximum number of blocks within a document, including
      -- blocks that are nested within other blocks.

    GenMaxBlockDepth :: GenOption ('Context 'One 'Root) Natural
      -- ^ The maximum depth of a block. A top-level block has depth 1;
      -- a block within a 'TagBlock' of depth *n* has depth *n+1*.

genDefault :: GenOption context a -> a
genDefault =
  \case
    GenMaxTotalBlocks -> 20
    GenMaxBlockDepth -> 4
