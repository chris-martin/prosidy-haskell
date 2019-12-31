{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE KindSignatures #-}
    {- We use the kind signatures extension to annotate type
       parameters with their kind using (::). This is just like
       a type annotation, but what follows the colons is a kind
       rather than a type. -}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances, QuantifiedConstraints #-}

module Prosidy.JsonConcepts
  (
    {- * JSON AST -}  JS ( JsString, JsList, JsDict ),
    {- * Keys -}      JsKey ( .. ), JsKeyString ( .. )
  ) where

import Prosidy.Foundation

-- Char
import Data.Char (Char)

-- Eq
import Data.Eq (Eq)

-- Things related to type-level programming
import Data.Kind (Type)

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
    jsKeyString jk = case jk of
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
