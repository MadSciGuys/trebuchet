{-|
Module:      Treb.Filter
Description: Generic filtering language.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitka@its.jnj.com
Stability:   Provisional
Portability: POSIX

Simple generic language for defining filters on arbitrary collection types.
-}

{-# LANGUAGE TypeFamilies #-}

module Treb.Filter where

import Data.Aeson.Types (Parser)

-- | The class of filterable collections, defined in terms of the atomic filter
--   operators relevant to the type. Here's a simple example instance for
--   '[Int]':
--
-- > {-# LANGUAGE TypeFamilies, FlexibleInstances #-}
-- > import Data.List (intersect, union, (\\))
-- >
-- > data IntListOp = IntEq Int
-- >                | IntGt Int
-- >                | IntLt Int
-- >
-- > instance Filterable [Int] where
-- >     type Atom [Int] = IntListOp
-- >     appAtom (IntEq i) = filter (== i)
-- >     appAtom (IntGt i) = filter (> i)
-- >     appAtom (IntLt i) = filter (< i)
-- >     conj = intersect
-- >     disj = union
-- >     neg = flip (\\)
class Filterable c where
    -- | An associated type that, for the instantiated class type, represents
    --   the atomic filter operations for that type. For example, a 'String'
    --   instance might include a regular expression operator, while an 'Int'
    --   instance might include equality, greater-than, and less-than
    --   operators.
    type Atom c :: *
    -- | Application of an 'Atom' to a collection.
    appAtom :: Atom c -> c -> c
    -- | Collection negation (or difference).
    neg  :: c -> c -> c
    -- | Collection conjuction.
    conj :: c -> c -> c
    -- | Collection disjuction.
    disj :: c -> c -> c

-- | A filter for some type member of the 'Filterable' class.
data Filter c = FilterAtom (Atom c)
              | FilterNeg  (Filter c)
              | FilterConj (Filter c) (Filter c)
              | FilterDisj (Filter c) (Filter c)

-- | 'Filter' application.
appFilter :: Filterable c => Filter c -> c -> c
appFilter (FilterAtom a)   c = appAtom a c
appFilter (FilterNeg f)    c = neg  (appFilter f c) c
appFilter (FilterConj l r) c = conj (appFilter l c) (appFilter r c)
appFilter (FilterDisj l r) c = disj (appFilter l c) (appFilter r c)

-- | Recursively flatten adjacent filter conjunctions.
conjList :: Filter a -> Filter a -> [Filter a]
conjList (FilterConj l r) (FilterConj l' r') = conjList l r ++ conjList l' r'
conjList (FilterConj l r) r'                 = conjList l r ++ [r']
conjList l                (FilterConj l' r') = l : conjList l' r'
conjList l                r                  = [l, r]

-- | Recursively flatten adjacent filter disjunctions.
disjList :: Filter a -> Filter a -> [Filter a]
disjList (FilterConj l r) (FilterConj l' r') = disjList l r ++ disjList l' r'
disjList (FilterConj l r) r'                 = disjList l r ++ [r']
disjList l                (FilterConj l' r') = l : disjList l' r'
disjList l                r                  = [l, r]

conjUnlist :: [Filter a] -> Parser (Filter a)
conjUnlist [] = fail "empty conjunction list"
conjUnlist (c:[]) = return c
conjUnlist (c:cs) = FilterConj c <$> conjUnlist cs

disjUnlist :: [Filter a] -> Parser (Filter a)
disjUnlist [] = fail "empty disjunction list"
disjUnlist (d:[]) = return d
disjUnlist (d:ds) = FilterDisj d <$> disjUnlist ds
