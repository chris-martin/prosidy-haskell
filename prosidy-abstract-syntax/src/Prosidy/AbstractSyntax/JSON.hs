{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs, KindSignatures, NoImplicitPrelude #-}

module Prosidy.AbstractSyntax.JSON where

import qualified Prosidy.AbstractSyntax as P

import Data.Functor (Functor (fmap))
import Data.Kind (Type)

data JSON
  (string :: Type) (list :: Type -> Type) (map :: Type -> Type -> Type)
    where
      String :: string -> JSON string list map

      List :: list (JSON string list map) -> JSON string list map

      Map :: map string (JSON string list map) -> JSON string list map

data SpecialString = String_Attr | String_Body | String_Type | String_Paragraph | String_TagParagraph | String_TagName | String_TagBlock | String_TagLiteral | String_TagInline | String_SoftBreak

class String string
  where
    specialString :: SpecialString -> string

class Functor list => List list
  where

class Map map
  where
    kv :: k -> v -> map k v
    mapcat :: map k v -> map k v -> map k v

convert_prosidy_to_JSON ::
    (String string, List list, Map map) =>
    P.Prosidy string list map context -> JSON string list map

convert_prosidy_to_JSON (P.Document attr body) =
  Map
    ( kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.List xs) =
  List
    (
      fmap convert_prosidy_to_JSON xs
    )

convert_prosidy_to_JSON (P.Paragraph body) =
  Map
    ( kv (specialString String_Type) (String (specialString String_Paragraph)) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.TagParagraph name attr body) =
  Map
    ( kv (specialString String_Type) (String (specialString String_TagParagraph)) `mapcat`
      kv (specialString String_TagName) (String name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.TagBlock name attr body) =
  Map
    ( kv (specialString String_Type) (String (specialString String_TagBlock)) `mapcat`
      kv (specialString String_TagName) (String name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.TagLiteral name attr body) =
  Map
    ( kv (specialString String_Type) (String (specialString String_TagLiteral)) `mapcat`
      kv (specialString String_TagName) (String name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (String body)
    )

convert_prosidy_to_JSON (P.TagInline name attr body) =
  Map
    ( kv (specialString String_Type) (String (specialString String_TagInline)) `mapcat`
      kv (specialString String_TagName) (String name) `mapcat`
      kv (specialString String_Attr) (convert_prosidy_to_JSON attr) `mapcat`
      kv (specialString String_Body) (convert_prosidy_to_JSON body)
    )

convert_prosidy_to_JSON (P.String x) = String x

convert_prosidy_to_JSON P.SoftBreak =
  Map
    (
        kv (specialString String_Type) (String (specialString String_SoftBreak))
    )

convert_prosidy_to_JSON (P.Attrs flags fields) = _ -- todo
