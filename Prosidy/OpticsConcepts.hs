{-# OPTIONS_GHC -Wall #-}
    {- All GHC warnings are enabled. -}

{-# OPTIONS_GHC -Werror #-}
    {- This module may not emit any warnings. -}

{-# LANGUAGE KindSignatures #-}
    {- We use the kind signatures extension to annotate type
       parameters with their kind using (::). This is just like
       a type annotation, but what follows the colons is a kind
       rather than a type. -}

{-# LANGUAGE NoImplicitPrelude #-}
    {- This module has extremely little external dependency,
       not even from Prelude. -}

{-# LANGUAGE FunctionalDependencies #-}
    {- The 'OpticCompose' class has a functional dependency. -}

{-# LANGUAGE ScopedTypeVariables #-}

module Prosidy.OpticsConcepts
  (
    {- * Optic -}           Optic,
    {- * Try -}             Try ( .. ), recover, no, ok, overTry,
    {- * Separation -}      Separation ( .. ), part, reassemble,
                            afterReassemble, beforeReassemble,
    {- * Try separation -}  TrySeparation,
    {- * Operations -}      Forward ( forward ), ForwardTry ( forwardTry ),
                            Backward ( backward ), Over ( over ),
    {- * Optic types -}     Iso ( Iso ), Lens ( Lens ), Prism ( Prism ),
                            AffineTraversal ( AffineTraversal ),
    {- * Composition -}     OpticCompose ( .. ),
    {- * Simple -}          Simple
  ) where

-- | Function composition: @ab ▶ bc@ converts from @a@ to @b@, then from @b@ to @c@.
(▶) :: (a -> b) -> (b -> c) -> (a -> c)
(ab ▶ bc) a = bc (ab (a))

(◀) :: (a -> b) -> (c -> a) -> c -> b
(ab ◀ bc) c = ab (bc (c))

data Try a b = No a | Ok b

recover :: Try a a -> a
recover (No a) = a
recover (Ok a) = a

overNo :: (a -> a') -> Try a b -> Try a' b
overNo f (No a) = No (f a)
overNo _ (Ok b) = Ok b

overOk :: (b -> b') -> Try a b -> Try a b'
overOk _ (No a) = No a
overOk f (Ok b) = Ok (f b)

overTry :: (a -> a') -> (b -> b') -> Try a b -> Try a' b'
overTry f _ (No a) = No (f a)
overTry _ f (Ok b) = Ok (f b)

ok :: Prism (Try z b) (Try z b') b b'
ok = Prism abTry Ok
  where
    abTry (Ok b) = Ok b
    abTry (No z) = No (No z)

no :: Prism (Try b z) (Try b' z) b b'
no = Prism abTry No
  where
    abTry (No b) = Ok b
    abTry (Ok z) = No (Ok z)

-- | The result of applying a lens.
--
-- Represents the separation of /a/ into two components:
--
-- - 1. /b/
-- - 2. everything else

data Separation a' b b' =
    Separation
        b          -- ^ The part of the original object targeted by the lens.
        (b' -> a') -- ^ The remainder of the original object, represented as
                   --   a function that constructs a new version of the original
                   --   object with the lens-targeted part replaced by a new value.

part :: Separation a b b' -> b
part (Separation b _) = b

reassemble :: Separation a b b -> a
reassemble (Separation b ab') = ab' b

-- | Modifies a separation to perform an additional transformation after reassembly.
afterReassemble :: (b' -> a') -> Separation b' c c' -> Separation a' c c'
afterReassemble f (Separation b bc') = Separation b (f ◀ bc')

-- | Modifies a separation to perform a transformation on the reassembly parameter.
beforeReassemble :: (a -> b') -> Separation a' b b' -> Separation a' b a
beforeReassemble f (Separation b bc') = Separation b (f ▶ bc')

type TrySeparation a' b b' = Try a' (Separation a' b b')

-- |
-- >  ╭──────────╮
-- >  │  a    b  │
-- >  │          │
-- >  │  a'   b' │
-- >  ╰──────────╯

class Optic (o :: * -> * -> * -> * -> *)

-- |
-- >                    ╭──────────╮   ╭──────────╮   ╭──────────╮
-- >                    │  a    b  │   │  b    c  │   │  a    c  │
-- >  opticCompose  ::  │          │ → │          │ → │          │
-- >                    │  a'   b' │   │  b'   c' │   │  a'   c' │
-- >                    ╰──────────╯   ╰──────────╯   ╰──────────╯
-- >                         x              y              z

class (Optic x, Optic y, Optic z) => OpticCompose x y z | x y -> z
  where
    opticCompose :: x a a' b b' -> y b b' c c' -> z a a' c c'

data Iso a a' b b' = Iso (a -> b) (b' -> a')

instance Optic Iso

data Lens a a' b b' = Lens (a -> Separation a' b b')

instance Optic Lens

data Prism a a' b b' = Prism (a -> Try a' b) (b' -> a')

instance Optic Prism

data AffineTraversal a a' b b' = AffineTraversal (a -> TrySeparation a' b b')

instance Optic AffineTraversal

type Simple o a b = o a a b b

instance OpticCompose Iso Iso Iso
  where
    opticCompose (Iso ab ab') (Iso bc bc') = Iso (ab ▶ bc) (ab' ◀ bc')

instance OpticCompose Iso Lens Lens
  where
    opticCompose (Iso ab ab') (Lens bcSep) =
        Lens (ab ▶ bcSep ▶ afterReassemble ab')

instance OpticCompose Iso Prism Prism
  where
    opticCompose (Iso ab ab') (Prism bcTry bc') =
        Prism (ab ▶ bcTry ▶ overNo ab') (ab' ◀ bc')

instance OpticCompose Iso AffineTraversal AffineTraversal
  where
    opticCompose (Iso ab ab') (AffineTraversal bcTrySep) =
        AffineTraversal (ab ▶ bcTrySep ▶ overTry ab' (afterReassemble ab'))

-- |
-- >               ╭──────────╮   ╭──────────╮
-- >               │  a    b  │   │  a  → b  │
-- >  forward  ::  │          │ → │          │
-- >               │  a'   b' │   │          │
-- >               ╰──────────╯   ╰──────────╯

class Optic o => Forward o
  where
    forward :: o a a' b b' -> a -> b

instance Forward Iso
  where
    forward (Iso ab _ab') = ab

instance Forward Lens
  where
    forward (Lens abSep) = abSep ▶ part

-- |
-- >                  ╭──────────╮   ╭──────────╮
-- >                  │  a    b  │   │  a  → b  │
-- >  forwardTry  ::  │          │ → │  ↓       │
-- >                  │  a'   b' │   │  a'      │
-- >                  ╰──────────╯   ╰──────────╯

class Optic o => ForwardTry o
  where
    forwardTry :: o a a' b b' -> a -> Try a' b

instance ForwardTry Prism
  where
    forwardTry (Prism abTry _ab') = abTry

instance ForwardTry AffineTraversal
  where
    forwardTry (AffineTraversal abTrySep) = abTrySep ▶ overOk part

-- |
-- >                ╭──────────╮   ╭──────────╮
-- >                │  a    b  │   │          │
-- >  backward  ::  │          │ → │          │
-- >                │  a'   b' │   │  a' ← b' │
-- >                ╰──────────╯   ╰──────────╯

class Optic o => Backward o
  where
    backward :: o a a' b b' -> b' -> a'

instance Backward Iso
  where
    backward (Iso _ab ab') = ab'

instance Backward Prism
  where
    backward (Prism _abTry ab') = ab'

-- |
-- >            ╭──────────╮   ╭──────────╮   ╭──────────╮
-- >            │  a    b  │   │       b  │   │  a       │
-- >  over  ::  │          │ → │       ↓  │ → │  ↓       │
-- >            │  a'   b' │   │       b' │   │  a'      │
-- >            ╰──────────╯   ╰──────────╯   ╰──────────╯

class Optic o => Over o
  where
    over :: o a a' b b' -> (b -> b') -> (a -> a')

instance Over Iso
  where
    over (Iso ab ab') f = ab ▶ f ▶ ab'

instance Over Lens
  where
    over (Lens abSep) f = abSep ▶ beforeReassemble f ▶ reassemble

instance Over Prism
  where
    over (Prism abTry ab') f = abTry ▶ overOk (f ▶ ab') ▶ recover

instance Over AffineTraversal
  where
    over (AffineTraversal abTrySep) f = abTrySep ▶ overOk (beforeReassemble f ▶ reassemble) ▶ recover
