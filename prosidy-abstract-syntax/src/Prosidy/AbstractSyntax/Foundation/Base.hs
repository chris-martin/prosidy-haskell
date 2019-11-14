{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds, NoImplicitPrelude #-}

module Prosidy.AbstractSyntax.Foundation.Base where

import Prosidy.AbstractSyntax.Foundation

import Data.Char (Char)

type BaseFoundation =
  'Foundation
    ([] Char)             -- string
    []                    -- list
    (AssociationList [])  -- map

newtype AssociationList list a b =
    AssociationList (list (a, b))
