{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses,
             NoImplicitPrelude, QuantifiedConstraints, RankNTypes #-}

module Prosidy.AbstractSyntax.JSON where

import qualified Prosidy.AbstractSyntax as P
import Prosidy.AbstractSyntax (Foundation, String, List, Dict)

import Data.Functor (Functor (fmap))

data JSON (f :: Foundation) =
    JsString (String f)
  | JsList (List f (JSON f))
  | JsDict (Dict f (JSON f))

data SpecialString = String_Attr | String_Body | String_Type
    | String_Paragraph | String_TagParagraph | String_TagName
    | String_TagBlock | String_TagLiteral | String_TagInline
    | String_SoftBreak

class IsString string
  where
    specialString :: SpecialString -> string

class Functor list => IsList list
  where

class IsDict k map | map -> k
  where
    kv :: k -> v -> map v
    mapcat :: map v -> map v -> map v

type Requirements f =
  (IsString (String f), IsList (List f), IsDict (String f) (Dict f))

convert_prosidy_to_JSON :: Requirements f =>
    P.Prosidy f context -> JSON f

convert_prosidy_to_JSON (P.Document attr body) =
  JsDict
    ( kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.List xs) =
  JsList
    (
      fmap convert_prosidy_to_JSON xs
    )

convert_prosidy_to_JSON (P.Paragraph body) =
  JsDict
    ( kv (specialString String_Type) (JsString (specialString String_Paragraph)) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.TagParagraph name attr body) =
  JsDict
    ( kv (specialString String_Type) (JsString (specialString String_TagParagraph)) `mapcat`
      kv (specialString String_TagName) (JsString name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.TagBlock name attr body) =
  JsDict
    ( kv (specialString String_Type) (JsString (specialString String_TagBlock)) `mapcat`
      kv (specialString String_TagName) (JsString name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.TagLiteral name attr body) =
  JsDict
    ( kv (specialString String_Type) (JsString (specialString String_TagLiteral)) `mapcat`
      kv (specialString String_TagName) (JsString name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (JsString body)
    )

convert_prosidy_to_JSON (P.TagInline name attr body) =
  JsDict
    ( kv (specialString String_Type) (JsString (specialString String_TagInline)) `mapcat`
      kv (specialString String_TagName) (JsString name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.StringInline x) = JsString x

convert_prosidy_to_JSON P.SoftBreak =
  JsDict
    (
        kv (specialString String_Type) (JsString (specialString String_SoftBreak))
    )

convert_prosidy_to_JSON (P.Attrs _flags _fields) = let x = x in x -- todo
