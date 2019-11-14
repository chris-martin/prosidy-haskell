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

data JsKey = String_Attr | String_Body | String_Type
    | String_Paragraph | String_TagParagraph | String_TagName
    | String_TagBlock | String_TagLiteral | String_TagInline
    | String_SoftBreak

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
    ( kv (jsKeyString String_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString String_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.List xs) =
  JsList
    (
      fmap convert_prosidy_to_JS xs
    )

convert_prosidy_to_JS (P.Paragraph body) =
  JsDict
    ( kv (jsKeyString String_Type) (JsString (jsKeyString String_Paragraph)) `mapcat`
      kv (jsKeyString String_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.TagParagraph name attr body) =
  JsDict
    ( kv (jsKeyString String_Type) (JsString (jsKeyString String_TagParagraph)) `mapcat`
      kv (jsKeyString String_TagName) (JsString name) `mapcat`
      kv (jsKeyString String_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString String_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.TagBlock name attr body) =
  JsDict
    ( kv (jsKeyString String_Type) (JsString (jsKeyString String_TagBlock)) `mapcat`
      kv (jsKeyString String_TagName) (JsString name) `mapcat`
      kv (jsKeyString String_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString String_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.TagLiteral name attr body) =
  JsDict
    ( kv (jsKeyString String_Type) (JsString (jsKeyString String_TagLiteral)) `mapcat`
      kv (jsKeyString String_TagName) (JsString name) `mapcat`
      kv (jsKeyString String_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString String_Body) (JsString body)
    )

convert_prosidy_to_JS (P.TagInline name attr body) =
  JsDict
    ( kv (jsKeyString String_Type) (JsString (jsKeyString String_TagInline)) `mapcat`
      kv (jsKeyString String_TagName) (JsString name) `mapcat`
      kv (jsKeyString String_Attr) (convert_prosidy_to_JS attr) `mapcat`
      kv (jsKeyString String_Body) (convert_prosidy_to_JS body)
    )

convert_prosidy_to_JS (P.StringInline x) = JsString x

convert_prosidy_to_JS P.SoftBreak =
  JsDict
    (
        kv (jsKeyString String_Type) (JsString (jsKeyString String_SoftBreak))
    )

convert_prosidy_to_JS (P.Attrs _flags _fields) = let x = x in x -- todo
