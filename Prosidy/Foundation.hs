{-# OPTIONS_GHC -Wall #-}
-- All GHC warnings are enabled.

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE KindSignatures #-}
-- We use the kind signatures extension to annotate type parameters with their kind using (::). This is just like a type annotation, but what follows the colons is a kind rather than a type.

{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}

{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies, FlexibleInstances #-}

module Prosidy.Foundation (

    -- * Foundation
    Foundation (Foundation),

    -- * String
    String,

    -- * List
    List, ListBuilding (listSingleton, listConcat), ListWalk (..), ListDirection (..),

    -- * Dict
    Dict, DictBuilding (dictSingleton, dictConcat),

    -- * ...
    ListOfDictKeys (listOfDictKeys),

    -- * Base
    BaseFoundation, AssociationList (AssociationList)

  ) where

import Prosidy.OpticsConcepts

import qualified Data.Traversable as Traversable

-- List
import qualified Data.List as List

-- Char
import Data.Char (Char)

-- Things related to type-level programming
import Data.Kind (Type)
import Data.Proxy (Proxy)

-- Functors
import Data.Functor (Functor)


---  Foundation  ---

data Foundation =
  Foundation
    Type -- ^ String
    (Type -> Type) -- ^ List
    (Type -> Type) -- ^ Dict (map with string keys)

-- | Some reasonable options for this parameter:
--
-- - 'Data.String.String' from the @base@ package
-- - 'Data.Text.Text' from the @text@ package
-- - 'Data.Text.Lazy.Text' from the @text@ package

type family String a where String ('Foundation string list dict) = string

-- | Some reasonable options for this parameter:
--
-- - The built-in @[]@ type
-- - 'Data.Sequence.Seq' from the @containers@ package
-- - 'Data.Vector.Vector' from the @vector@ package

type family List a where List ('Foundation string list dict) = list

-- | Some reasonable options for this parameter:
--
-- - @'AssociationList' [] Data.String.String@
-- - 'Data.Map.Map' from the @containers@ package
-- - 'Data.HashMap.HashMap' from the
--   @unordered-containers@ package

type family Dict a where Dict ('Foundation string list dict) = dict

class Functor list => ListBuilding (list :: Type -> Type)
  where
    listSingleton :: a -> list a
    listConcat :: list a -> list a -> list a

instance ListBuilding []
  where
    listSingleton x = x : []
    listConcat = (List.++)

class Functor dict => DictBuilding (k :: Type) (dict :: Type -> Type) | dict -> k
  where
    dictSingleton :: k -> v -> dict v
    dictConcat :: dict v -> dict v -> dict v

class ListOfDictKeys (f :: Foundation)
  where
    listOfDictKeys :: Proxy f -> Dict f v -> List f (String f)

-- | A minimal specialization of 'Foundation' using only types available in library the @base@. This option is simple, but perhaps not the most performant choice.

type BaseFoundation =
  'Foundation
    ([] Char) -- string
    [] -- list
    (AssociationList [] ([] Char))  -- dict

newtype AssociationList list a b = AssociationList (list (a, b))

deriving instance (Functor list) => Functor (AssociationList list a)

instance (ListBuilding list) => DictBuilding k (AssociationList list k)
  where
    dictSingleton k v = AssociationList (listSingleton (k, v))
    dictConcat (AssociationList a) (AssociationList b) = AssociationList (listConcat a b)

data ListDirection = ListForward | ListBackward

class ListWalk list
  where
    listWalk :: ListDirection -> MonadicTraversal (list a) (list b) a b

instance ListWalk []
  where
    listWalk ListForward = MonadicTraversal Traversable.for
    listWalk ListBackward = MonadicTraversal (\xs action -> Traversable.for (List.reverse xs) action)
