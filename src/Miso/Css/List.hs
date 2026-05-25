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

type family IsSubSetCase rer t where
  IsSubSetCase Nothing         _t = False
  IsSubSetCase (Just '( _, l)) t  = IsSubSet t l

type family IsSubSet a b where
  IsSubSet '[] _ = True
  IsSubSet (h : t) l = IsSubSetCase (RemoveElem '[] h l) t
