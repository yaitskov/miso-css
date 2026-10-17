{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.Css.List where

import Data.Type.Bool ( If )
import Data.Type.Equality ( type (==) )
import Prelude

type family Append a b where
  Append (x:xs) b = x : Append xs b
  Append '[]    b = b

type family RemoveElem s l x where
  RemoveElem _ _ '[] = Nothing
  RemoveElem s k (h:t) =
    If (k == h)
      (Just '( k, Append s t))
      (RemoveElem (h : s) k t)

type family FilterOut x l where
  FilterOut x '[] = '[]
  FilterOut x (h:t) = If (x == h) (FilterOut x t) (h : FilterOut x t)

type family CatMaybes l where
  CatMaybes '[] = '[]
  CatMaybes (Just h : t) = h : CatMaybes t
  CatMaybes (Nothing : t) = CatMaybes t

type family Diff l1 l2 where
  Diff l1 '[] = l1
  Diff '[] _ = '[]
  Diff t1 (h2:t2) = Diff (FilterOut h2 t1) t2

type family PrependMb mb l where
  PrependMb Nothing l = l
  PrependMb (Just x) l = x : l

type family Elem e l where
  Elem _ '[] = False
  Elem h (h : l) = True
  Elem h (_ : l) = Elem h l

type family MergeUniq a b s where
  MergeUniq '[] b s = Right (Append b s)
  MergeUniq (ah : at) b s =
    If (Elem ah b)
      (Left ah)
      (MergeUniq at b (ah : s))

type family MergeDedup a b s where
  MergeDedup '[] b s = Append b s
  MergeDedup (ah : at) b s =
    If (Elem ah b)
      (MergeDedup at b s)
      (MergeDedup at b (ah : s))

type family FindDuplicates s where
  FindDuplicates '[] = '[]
  FindDuplicates (ah : at) =
    If (Elem ah at)
      (ah : FindDuplicates at)
      (FindDuplicates at)

type family IsSubSetCase rer t where
  IsSubSetCase Nothing         _t = False
  IsSubSetCase (Just '( _, l)) t  = IsSubSet t l

type family IsSubSet a b where
  IsSubSet '[] _ = True
  IsSubSet (h : t) l = IsSubSetCase (RemoveElem '[] h l) t

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ xs@[] = ([], xs)
spanMaybe p xs@(x : xs') = case p x of
  Just y -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
  Nothing -> ([], xs)
