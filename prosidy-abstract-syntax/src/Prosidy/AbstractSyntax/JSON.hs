{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses,
             NoImplicitPrelude, QuantifiedConstraints, RankNTypes #-}

module Prosidy.AbstractSyntax.JSON where

import qualified Prosidy.AbstractSyntax as P
import Prosidy.AbstractSyntax (Foundation, String, List, Dict)

import Data.Functor (Functor (fmap))

data JS (f :: Foundation) =
    JsString (String f)
  | JsList (List f (JS f))
  | JsDict (Dict f (JS f))

data JsKey = JK_Attr | JK_Body | JK_Type
    | JK_Paragraph | JK_TagParagraph | JK_TagName
    | JK_TagBlock | JK_TagLiteral | JK_TagInline
    | JK_SoftBreak

class IsString string
  where
    jsKeyString :: JsKey -> string

class Functor list => IsList list
  where

class IsDict k map | map -> k
  where
    kv :: k -> v -> map v
    mapcat :: map v -> map v -> map v

type Requirements f =
  (IsString (String f), IsList (List f), IsDict (String f) (Dict f))

convert_prosidy_to_JS :: Requirements f =>
    P.Prosidy f context -> JS f

convert_prosidy_to_JS (P.Document attr body) =
  JsDict
    ( kv (jsKeyString JK_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString JK_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.List xs) =
  JsList
    (
      fmap convert_prosidy_to_JS xs
    )

convert_prosidy_to_JS (P.Paragraph body) =
  JsDict
    ( kv (jsKeyString JK_Type) (JsString (jsKeyString JK_Paragraph)) `mapcat`
      kv (jsKeyString JK_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.TagParagraph name attr body) =
  JsDict
    ( kv (jsKeyString JK_Type) (JsString (jsKeyString JK_TagParagraph)) `mapcat`
      kv (jsKeyString JK_TagName) (JsString name) `mapcat`
      kv (jsKeyString JK_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString JK_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.TagBlock name attr body) =
  JsDict
    ( kv (jsKeyString JK_Type) (JsString (jsKeyString JK_TagBlock)) `mapcat`
      kv (jsKeyString JK_TagName) (JsString name) `mapcat`
      kv (jsKeyString JK_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString JK_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.TagLiteral name attr body) =
  JsDict
    ( kv (jsKeyString JK_Type) (JsString (jsKeyString JK_TagLiteral)) `mapcat`
      kv (jsKeyString JK_TagName) (JsString name) `mapcat`
      kv (jsKeyString JK_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString JK_Body) (JsString body)
    )

convert_prosidy_to_JS (P.TagInline name attr body) =
  JsDict
    ( kv (jsKeyString JK_Type) (JsString (jsKeyString JK_TagInline)) `mapcat`
      kv (jsKeyString JK_TagName) (JsString name) `mapcat`
      kv (jsKeyString JK_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString JK_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.StringInline x) = JsString x

convert_prosidy_to_JS P.SoftBreak =
  JsDict
    (
        kv (jsKeyString JK_Type) (JsString (jsKeyString JK_SoftBreak))
    )

convert_prosidy_to_JS (P.Attrs _flags _fields) = let x = x in x -- todo
