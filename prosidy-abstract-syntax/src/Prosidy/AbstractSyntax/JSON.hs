{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FunctionalDependencies, GADTs, KindSignatures, LambdaCase,
             MultiParamTypeClasses, NoImplicitPrelude,
             QuantifiedConstraints, RankNTypes #-}

module Prosidy.AbstractSyntax.JSON where

import qualified Prosidy.AbstractSyntax as P
import Prosidy.AbstractSyntax (Foundation, String, List, Dict)

import Data.Function (($))
import Data.Functor (Functor (fmap))
import Data.Kind (Type)

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

class MapBuilding (k :: Type) (map :: Type -> Type) | map -> k
  where
    kv :: k -> v -> map v
    mapcat :: map v -> map v -> map v

prosidyJS ::
    (
      JsKeyString (String f),
      Functor (List f),
      MapBuilding (String f) (Dict f)
    )
  =>
    P.Prosidy f context -> JS f

prosidyJS =
  let a + b = mapcat a b              ; infixl 5 +
      k .= v = kv (jsKeyString k) v   ; infixl 6 .=
  in \case

    P.StringInline x              -> JsString x

    P.List xs                     -> JsList $ fmap prosidyJS xs

    P.Document attr body          -> JsDict $ JK_Attr .= prosidyJS attr
                                            + JK_Body .= prosidyJS body

    P.Paragraph body              -> JsDict $ JK_Type .= JsString (jsKeyString JK_Paragraph)
                                            + JK_Body .= prosidyJS body

    P.TagParagraph name attr body -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagParagraph)
                                            + JK_Tag  .= JsString name
                                            + JK_Attr .= prosidyJS attr
                                            + JK_Body .= prosidyJS body

    P.TagBlock name attr body     -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagBlock)
                                            + JK_Tag  .= JsString name
                                            + JK_Attr .= prosidyJS attr
                                            + JK_Body .= prosidyJS body

    P.TagLiteral name attr body   -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagLiteral)
                                            + JK_Tag  .= JsString name
                                            + JK_Attr .= prosidyJS attr
                                            + JK_Body .= JsString body

    P.TagInline name attr body    -> JsDict $ JK_Type .= JsString (jsKeyString JK_TagInline)
                                            + JK_Tag  .= JsString name
                                            + JK_Attr .= prosidyJS attr
                                            + JK_Body .= prosidyJS body

    P.SoftBreak                   -> JsDict $ JK_Type .= JsString (jsKeyString JK_SoftBreak)

    P.Attrs _flags _fields        -> let x = x in x -- todo
