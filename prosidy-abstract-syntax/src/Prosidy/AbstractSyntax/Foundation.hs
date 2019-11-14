{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, NoImplicitPrelude, TypeFamilies #-}

module Prosidy.AbstractSyntax.Foundation where

import Data.Kind (Type)

data Foundation =
  Foundation
    Type -- ^ String
    (Type -> Type) -- ^ List
    (Type -> Type -> Type) -- ^ Map

type family String a where String ('Foundation string list map) = string
type family List a where List ('Foundation string list map) = list
type family Map a where Map ('Foundation string list map) = map
