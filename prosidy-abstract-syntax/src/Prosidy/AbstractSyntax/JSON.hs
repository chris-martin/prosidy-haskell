{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs, KindSignatures, LambdaCase, NoImplicitPrelude #-}

module Prosidy.AbstractSyntax.JSON where

import qualified Prosidy.AbstractSyntax as P

import Data.Kind (Type)

data JSON
  (string :: Type) (list :: Type -> Type) (map :: Type -> Type -> Type)
    where
      String :: string -> JSON string list map

      List :: list (JSON string list map) -> JSON string list map

      Map :: map string (JSON string list map) -> JSON string list map

data SpecialString = String_Head | String_Body

class String string
  where
    specialString :: SpecialString -> string

class Map map
  where
    kv :: k -> v -> map k v
    mapcat :: map k v -> map k v -> map k v

convert_prosidy_to_JSON ::
    (String string, Map map) =>
    P.Prosidy string list map context -> JSON string list map

convert_prosidy_to_JSON =
  \case
    P.Document head body ->
        Map
          ( kv (specialString String_Head) (convert_prosidy_to_JSON head) `mapcat`
            kv (specialString String_Body) (convert_prosidy_to_JSON body)
          )
