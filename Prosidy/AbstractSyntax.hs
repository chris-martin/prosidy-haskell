{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

{-# OPTIONS_GHC -fno-warn-unused-imports
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
    {- * Abstract -}
    {- ** Optic -}       Optic ( Iso, Lens, Prism ),
                         OpticForward ( .. ), OpticBackward ( .. ),
                         view, review, preview, over,
    {- ** Isomorphism -} Iso, Iso',
    {- ** Lens -}        Lens, Lens',
    {- ** Prism -}       Prism, Prism',
    {- ** Walk -}        Walk, Walk', nilWalk, idWalk, opticWalk,
    -------------------------------------------------------------------
    {- * Fun with AST -} documentHeadLens, documentBodyLens,
                         prosidyListIso,
    -------------------------------------------------------------------
    {- * AST walking -}  InlineDirection ( LeftToRight, RightToLeft ),
                         BlockDirection ( TopToBottom, BottomToTop ),
                         TreeDirection ( RootToLeaf, LeafToRoot ),
                         prosidyListWalk, blockChildrenWalk,
                         eachBlockChild, blockListDirectionIso,
                         BlockWalk ( blockWalk ),
    -------------------------------------------------------------------
    {- * JSON -}
    {- ** JSON AST -}    JS ( JsString, JsList, JsDict ),
    {- ** Keys -}        JsKey ( .. ), JsKeyString ( .. ),
    {- ** Conversion -}  prosidyJS,
    -------------------------------------------------------------------
    {- * Random gen -}   genConst, genId, genAp, genCompose,
                         genWord8AsciiPrint, genDocument, genBlock,
                         genInline, GenOption ( .. ), genDefault
    -------------------------------------------------------------------
  ) where

import Data.Char (Char)
import qualified Data.Char as Char
import Data.Eq (Eq)
import Data.Function (($), fix, id)
import qualified Data.List as List
import System.IO (IO)
import Data.Functor.Identity (Identity (Identity))

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

class ListWalk list
  where
    listWalk :: ListDirection -> Walk (list a) (list b) a b

instance ListWalk []
  where
    listWalk ListForward = Prelude.traverse
    listWalk ListBackward = \action xs -> Prelude.traverse action (List.reverse xs)

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


---  Optics concepts  ---

data OpticForward = ForwardTotal | ForwardPartial

data OpticBackward = BackwardTotal | BackwardReassemble

data Optic (forward :: OpticForward) (backward :: OpticBackward) s t a b
  where
    Iso :: (s -> a) -> (b -> t) -> Optic 'ForwardTotal 'BackwardTotal s t a b
    Lens :: (s -> a) -> (s -> b -> t) -> Optic 'ForwardTotal 'BackwardReassemble s t a b
    Prism :: (s -> Either t a) -> (b -> t) -> Optic 'ForwardPartial 'BackwardTotal s t a b

view :: Optic 'ForwardTotal backward s t a b -> s -> a
view (Iso f _) x = f x
view (Lens f _) x = f x

review :: Optic forward 'BackwardTotal s t a b -> b -> t
review (Iso _ f) x = f x
review (Prism _ f) x = f x

preview :: Optic forward backward s t a b -> s -> Maybe a
preview (Iso f _) x = Maybe.Just (f x)
preview (Lens f _) x = Maybe.Just (f x)
preview (Prism f _) x = Either.either (\_ -> Maybe.Nothing) Maybe.Just (f x)

over :: Optic forward backward s t a b -> (a -> b) -> (s -> t)
over (Iso f g) h x = g (h (f x))
over (Lens f g) h x = g x (h (f x))
over (Prism f g) h x = (Either.either id (\a -> g (h a)) (f x))

type Iso s t a b = Optic 'ForwardTotal 'BackwardTotal s t a b
type Iso' s a = Iso s s a a
type Lens s t a b = Optic 'ForwardTotal 'BackwardReassemble s t a b
type Lens' s a = Lens s s a a
type Prism s t a b = Optic 'ForwardPartial 'BackwardTotal s t a b
type Prism' s a = Prism s s a a


---  Walk concept  ---

type Walk s t a b = forall f. (Monad f) => (a -> f b) -> s -> f t
type Walk' s a = Walk s s a a

opticWalk :: forall forward backward s t u v a b.
    Optic forward backward s t u v -> Walk u v a b -> Walk s t a b

opticWalk (Iso convert convertBack) walk action (s :: s) =
  do
    (v :: v) <- walk action (convert s :: u)
    return (convertBack v :: t)

opticWalk (Lens getPart reassemble) walk action (s :: s) =
  do
    (v :: v) <- walk action (getPart s :: u)
    return (reassemble s v :: t)

opticWalk (Prism narrow widen) walk action (s :: s) =
    case (narrow s) of
        Either.Left (t :: t) -> return t
        Either.Right (u :: u) ->
          do
            (v :: v) <- walk action u
            return (widen v :: t)

walkWalk :: forall s t u v a b. Walk s t u v -> Walk u v a b -> Walk s t a b
walkWalk traverse1 traverse2 action (s :: s) = traverse1 (traverse2 action) s

nilWalk :: Walk s s a b
nilWalk _ s = pure s

idWalk :: Walk' s s
idWalk f s = f s


---  Various basic AST manipulations  ---

documentHeadLens ::
    Lens'
        (Prosidy f ('Context 'One 'Root))
        (Prosidy f ('Context 'One 'Meta))

documentHeadLens =
    Lens
        (\(Document head _) -> head)
        (\(Document _ body) head -> Document head body)

documentBodyLens ::
    Lens'
        (Prosidy f ('Context 'One 'Root))
        (Prosidy f ('Context 'Many 'Block))

documentBodyLens =
    Lens
        (\(Document _ body) -> body)
        (\(Document head _) body -> Document head body)

prosidyListIso ::
    Iso'
        (Prosidy f ('Context 'Many l))
        (List f (Prosidy f ('Context 'One l)))

prosidyListIso = Iso (\(List x) -> x) List


---  Walking around in the AST  ---

data TreeDirection = RootToLeaf | LeafToRoot

data BlockDirection = TopToBottom | BottomToTop

data InlineDirection = LeftToRight | RightToLeft

prosidyListWalk :: ListWalk (List f) =>
    ListDirection
    ->
    Walk' (Prosidy f ('Context 'Many l))
          (Prosidy f ('Context 'One l))

prosidyListWalk direction =
    prosidyListIso `opticWalk` listWalk direction

blockChildrenWalk ::
    Walk' (Prosidy f ('Context 'One 'Block))
          (Prosidy f ('Context 'Many 'Block))

blockChildrenWalk action block =
    case block of
        TagBlock name attrs children ->
            TagBlock name attrs `fmap` action children
        _ -> pure block

eachBlockChild :: ListWalk (List f) =>
    BlockDirection
    ->
    Walk' (Prosidy f ('Context 'One 'Block))
          (Prosidy f ('Context 'One 'Block))

eachBlockChild blockDirection =
    blockChildrenWalk `walkWalk`
    prosidyListWalk (blockListDirectionIso `view` blockDirection)

blockListDirectionIso :: Iso' BlockDirection ListDirection
blockListDirectionIso =
    Iso
        (\case TopToBottom -> ListForward
               BottomToTop -> ListBackward)
        (\case ListForward -> TopToBottom
               ListBackward -> BottomToTop)

class BlockWalk size level
  where
    blockWalk :: ListWalk (List f) =>
        TreeDirection
        ->
        BlockDirection
        ->
        Walk' (Prosidy f ('Context size level))
              (Prosidy f ('Context 'One 'Block))

instance BlockWalk 'One 'Root
  where
    blockWalk treeDirection blockDirection =
        documentBodyLens `opticWalk`
        blockWalk treeDirection blockDirection

instance BlockWalk 'One 'Block
  where
    blockWalk treeDirection blockDirection action block =
        case treeDirection of
            RootToLeaf ->
              do
                block' <- action block
                eachBlockChild blockDirection action block'
            LeafToRoot ->
              do
                block' <- eachBlockChild blockDirection action block
                action block'

instance BlockWalk 'Many 'Block
  where
    blockWalk treeDirection blockDirection =
        prosidyListWalk (view blockListDirectionIso blockDirection)
        `walkWalk` blockWalk treeDirection blockDirection


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
