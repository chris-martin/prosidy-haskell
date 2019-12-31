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

{-# LANGUAGE LambdaCase, FlexibleContexts, FunctionalDependencies, TypeApplications, ScopedTypeVariables, RankNTypes #-}

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
    {- * Fun with AST -} documentHeadLens, documentBodyLens,
                         prosidyListIso,
    -------------------------------------------------------------------
    {- * AST walking -}  Walk ( Walk ), nilWalk, idWalk,
                         InlineDirection ( LeftToRight, RightToLeft ),
                         BlockDirection ( TopToBottom, BottomToTop ),
                         TreeDirection ( RootToLeaf, LeafToRoot ),
                         prosidyListWalk, blockChildrenWalk,
                         eachBlockChild, blockListDirectionIso,
                         BlockWalk ( blockWalk ),
    -------------------------------------------------------------------
    {- * JSON -}         prosidyJS,
    -------------------------------------------------------------------
    {- * Generation -}   genDocument, genBlock, genInline,
                         GenOption ( .. ), genDefault
    -------------------------------------------------------------------
  ) where

import Prosidy.Foundation
import Prosidy.GenerationConcepts
import Prosidy.JsonConcepts
import Prosidy.OpticsConcepts

import Data.Eq (Eq)
import Data.Function (($), fix, id)
import qualified Data.List as List
import System.IO (IO)
import Data.Functor.Identity (Identity (Identity))

-- Char
import Data.Char (Char)
import qualified Data.Char as Char

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

data ListDirection = ListForward | ListBackward

class ListWalk list
  where
    listWalk :: ListDirection -> Walk (list a) (list b) a b

instance ListWalk []
  where
    listWalk ListForward = Walk Prelude.traverse
    listWalk ListBackward = Walk (\action xs -> Prelude.traverse action (List.reverse xs))


---  Walk concept  ---

data Walk a a' b b' = Walk (forall f. (Monad f) => (b -> f b') -> a -> f a')

applyWalk :: Monad f => Walk a a' b b' -> (b -> f b') -> a -> f a'
applyWalk (Walk walk) f a = walk f a

instance Optic Walk

instance OpticCompose Iso Walk Walk
  where
    opticCompose (Iso convert convertBack) (Walk walk) = Walk $ \action s ->
      do
        v <- walk action (convert s)
        return (convertBack v)

instance OpticCompose Lens Walk Walk
  where
    opticCompose (Lens separate) (Walk walk) = Walk $ \action s ->
      do
        let Separation u r = separate s
        v <- walk action u
        return (r v)

instance OpticCompose Prism Walk Walk
  where
    opticCompose (Prism narrow widen) (Walk walk) = Walk $ \action s ->
        case (narrow s) of
            No t -> return t
            Ok u ->
              do
                v <- walk action u
                return (widen v)

instance OpticCompose AffineTraversal Walk Walk
  where
    opticCompose (AffineTraversal separate) (Walk walk) = Walk $ \action s ->
      do
        case (separate s) of
            No t -> return t
            Ok (Separation u r) ->
              do
                v <- walk action u
                return (r v)

instance OpticCompose Walk Walk Walk
  where
    opticCompose (Walk x) (Walk y) = Walk (\action s -> x (y action) s)

nilWalk :: Walk s s a b
nilWalk = Walk $ \_action s -> pure s

idWalk :: Simple Walk s s
idWalk = Walk $ \action s -> action s


---  Various basic AST manipulations  ---

documentHeadLens ::
    Simple Lens
        (Prosidy f ('Context 'One 'Root))
        (Prosidy f ('Context 'One 'Meta))

documentHeadLens =
    Lens
        (\(Document head body) ->
            Separation head (\head' -> Document head' body)
        )

documentBodyLens ::
    Simple Lens
        (Prosidy f ('Context 'One 'Root))
        (Prosidy f ('Context 'Many 'Block))

documentBodyLens =
    Lens
        (\(Document head body) ->
            Separation body (\body' -> Document head body')
        )

prosidyListIso ::
    Simple Iso
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
    Simple Walk (Prosidy f ('Context 'Many l))
                (Prosidy f ('Context 'One l))

prosidyListWalk direction =
    prosidyListIso `opticCompose` listWalk direction

blockChildrenWalk ::
    Simple Walk (Prosidy f ('Context 'One 'Block))
                (Prosidy f ('Context 'Many 'Block))

blockChildrenWalk = Walk $ \action block ->
    case block of
        TagBlock name attrs children ->
            TagBlock name attrs `fmap` action children
        _ -> pure block

eachBlockChild :: ListWalk (List f) =>
    BlockDirection
    ->
    Simple Walk (Prosidy f ('Context 'One 'Block))
                (Prosidy f ('Context 'One 'Block))

eachBlockChild blockDirection =
    blockChildrenWalk `opticCompose`
    prosidyListWalk (blockListDirectionIso `forward` blockDirection)

blockListDirectionIso :: Simple Iso BlockDirection ListDirection
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
        Simple Walk (Prosidy f ('Context size level))
                    (Prosidy f ('Context 'One 'Block))

instance BlockWalk 'One 'Root
  where
    blockWalk treeDirection blockDirection =
        documentBodyLens `opticCompose`
        blockWalk treeDirection blockDirection

instance BlockWalk 'One 'Block
  where
    blockWalk treeDirection blockDirection = Walk $ \action block ->
        case treeDirection of
            RootToLeaf ->
              do
                block' <- action block
                applyWalk (eachBlockChild blockDirection) action block'
            LeafToRoot ->
              do
                block' <- applyWalk (eachBlockChild blockDirection) action block
                action block'

instance BlockWalk 'Many 'Block
  where
    blockWalk treeDirection blockDirection =
        prosidyListWalk (blockListDirectionIso `forward` blockDirection)
        `opticCompose` blockWalk treeDirection blockDirection


---  JSON  ---

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


---  Random generation  ---

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
