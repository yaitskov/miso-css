{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.StyleMock
  ( module Miso.Css.Test.StyleMock
  , module X
  ) where

import Data.Proxy as X ( Proxy(Proxy) )
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Char8 qualified as C8
import Miso.Css.Miso as X
import Miso.Css.Operator as X
import Miso.Css.Prelude as X
import Miso.Css.Segment as X
import Miso.Css.Sibling
    ( SiblingBranch(NilSibBranch, AddSegToSibBranch),
      Sibling(NilSib, AddClassToSib) )
import Miso.Css.Style as X
import Miso.Css.Style.PostAppend as Post
import Miso.Css.Tags as X
import Miso.Html ( ToHtml(toHtml) )
import Test.Tasty ( TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

go :: forall m a en es r ei atrs kids cls ecs children.
  (MapMaybeFilterOutFullyMatchedHead '[] ecs ~ '[]) =>
  L.ByteString -> E m a en es r ei atrs kids cls ecs children -> TestTree
go ex el =
  testCase (C8.unpack $ C8.toStrict ex) do
    toHtml (toView el) @?= ex
a :: OrClass '[] "a"
a = TopOrClass pa
b :: OrClass '[] "b"
b = TopOrClass pb
c :: OrClass '[] "c"
c = TopOrClass pc
d :: OrClass '[] "d"
d = TopOrClass pd
pul :: Proxy "ul"
pul = Proxy @"ul"
{- HLINT ignore "Use camelCase" -}
ul_a :: OrClass '[ '[ '(NowOrLater, '[T "ul"], '[], '[])]] "a"
ul_a = AddAncestorBranch (AddTagAncestor pul $ CssOrphan nol) a
-- #x .a
id_a :: OrClass '[ '[ '(NowOrLater, '[I "b"], '[], '[])]] "a"
id_a = AddAncestorBranch (AddIdAncestor pb $ CssOrphan nol) a
pa :: Proxy "a"
pa = Proxy @"a"
pb :: Proxy "b"
pb = Proxy @"b"
pc :: Proxy "c"
pc = Proxy @"c"
pd :: Proxy "d"
pd = Proxy @"d"
nol :: Proxy NowOrLater
nol = Proxy @NowOrLater
jn :: Proxy JustNow
jn = Proxy @JustNow

acn :: Proxy AutoClean
acn = Proxy @AutoClean
-- nol_c = AddAncestorBranch (CssOrphan nol) c
-- jn_c =  AddAncestorBranch (CssOrphan jn) c
ac :: OrClass '[ '[ '(NowOrLater, '[C "a"], '[], '[])]] "c"
ac = AddAncestorBranch (AddAncestor pa $ CssOrphan nol) c

ab :: OrClass '[ [ '(AutoClean, '[], '[], '[])
                 , '(NowOrLater, '[C "a"], '[], '[])
                 ] ] "b"
ab = AddAncestorBranch (NextAncestor acn . AddAncestor pa $ CssOrphan nol) b


-- .a  .b  .c
abc :: OrClass '[['(NowOrLater, '[C "b"], '[], '[]), '(NowOrLater, '[C "a"], '[], '[])]] "c"
abc =
  AddAncestorBranch
  (AddAncestor pb . NextAncestor nol . AddAncestor pa $ CssOrphan nol)
  c
ab_c :: OrClass '[ '[ '(NowOrLater, [C "b", C "a"], '[], '[])]] "c"
ab_c =
  AddAncestorBranch
  (AddAncestor pb . AddAncestor pa $ CssOrphan nol)
  c
ba_c :: OrClass '[ '[ '(NowOrLater, [C "a", C "b"], '[], '[])]] "c"
ba_c =
  AddAncestorBranch
  (AddAncestor pa . AddAncestor pb $ CssOrphan nol)
  c
idC_and_a_b :: OrClass '[ '[ '(NowOrLater, [I "c", C "a"], '[], '[])]] "b"
idC_and_a_b =
  AddAncestorBranch
  (AddIdAncestor pc . AddAncestor pa $ CssOrphan nol)
  b
ul_and_a_b :: OrClass '[ '[ '(NowOrLater, [T "ul", C "a"], '[], '[])]] "b"
ul_and_a_b =
  AddAncestorBranch
  (AddTagAncestor pul . AddAncestor pa $ CssOrphan nol)
  b
-- id_a = AddAncestorBranch (AddIdAncestor pb $ CssOrphan nol) a
a_id_a :: OrClass '[ '[ '(AutoClean, '[I "a"], '[], '[])]] "a"
a_id_a =
  AddAncestorBranch
  (AddIdAncestor pa $ CssOrphan acn)
  a

-- .a.b
a_next_to_b :: OrClass '[ '[ '(AutoClean, '[C "a"], '[], '[])]] "b"
a_next_to_b =
  AddAncestorBranch
  (AddAncestor pa $ CssOrphan acn) b
-- .b.a
b_next_to_a :: OrClass '[ '[ '(AutoClean, '[C "b"], '[], '[])]] "a"
b_next_to_a =
  AddAncestorBranch
  (AddAncestor pb $ CssOrphan acn) a
-- .a[a]
a_wants_a_attr :: OrClass '[ '[ '(AutoClean, '[A "a"], '[], '[])]] "a"
a_wants_a_attr = AddAncestorBranch (AddAttr pa $ CssOrphan acn) a
-- [a] > .a
a_wants_a_attr_in_parent :: OrClass '[ '[ '(JustNow, '[A "a"], '[], '[])]] "a"
a_wants_a_attr_in_parent = AddAncestorBranch (AddAttr pa $ CssOrphan jn) a

pdiv :: Proxy "div"
pdiv = Proxy @"div"
div_child :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[T "div"], '[], '[])
     ] ]   "div_child"
div_child = -- div > .div_child
  AddAncestorBranch
  (NextAncestor acn . AddTagAncestor pdiv $ CssOrphan jn)
  (TopOrClass (Proxy @"div_child"))
div_ul_child :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[T "ul"], '[], '[])
     , '(JustNow, '[T "div"], '[], '[])
     ]
   ] "div_ul_child"
div_ul_child =
  AddAncestorBranch
  (NextAncestor acn . AddTagAncestor pul . NextAncestor jn . AddTagAncestor pdiv $ CssOrphan jn)
  (TopOrClass (Proxy @"div_ul_child"))
-- .a > .b > .c
a_dir_b_dir_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "b"], '[], '[])
     , '(NowOrLater, '[C "a"], '[], '[])
     ]
   ] "c"
a_dir_b_dir_c =
  AddAncestorBranch
  (NextAncestor acn . AddAncestor pb . NextAncestor jn . AddAncestor pa $ CssOrphan nol)
  c
-- .a > .b > .c > .d
a_dir_b_dir_c_dir_d :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "c"], '[], '[])
     , '(JustNow, '[C "b"], '[], '[])
     , '(JustNow, '[C "a"], '[], '[])
     ]
   ] "d"
a_dir_b_dir_c_dir_d =
  AddAncestorBranch
  (NextAncestor acn . AddAncestor pc .
   NextAncestor jn . AddAncestor pb .
   NextAncestor jn . AddAncestor pa $ CssOrphan jn)
  d
-- * > * > .c
star_dir_star_dir_c :: OrClass   '[['(JustNow, '[], '[], '[]), '(JustNow, '[], '[], '[])]] "c"
star_dir_star_dir_c =
  AddAncestorBranch
  (NextAncestor jn $ CssOrphan jn)
  c
a_dir_b :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "a"], '[], '[])
     ] ] "b"
a_dir_b = AddAncestorBranch (NextAncestor acn . AddAncestor pa $ CssOrphan jn) b
-- a_neighbour_b =
a_b_dir_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "b"], '[], '[])
     , '(NowOrLater, '[C "a"], '[], '[])
     ] ] "c"
a_b_dir_c =
  AddAncestorBranch
  (NextAncestor acn . AddAncestor pb . NextAncestor jn . AddAncestor pa $ CssOrphan nol)
  c
a_dirSib_b :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(NowOrLater, '[], '[], '[ '[ '(JustNow, '[C "a"])]])
     ] ] "b"
a_dirSib_b =
  AddAncestorBranch
    (NextAncestor acn $ AddSiblingBranch
      (AddSegToSibBranch (AddClassToSib pa $ NilSib jn) NilSibBranch)
      (CssOrphan nol))
    b
a_genSib_b :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(NowOrLater, '[], '[], '[ '[ '(NowOrLater, '[C "a"])]])
     ] ] "b"
a_genSib_b =
  AddAncestorBranch
    (NextAncestor acn $ AddSiblingBranch
      (AddSegToSibBranch (AddClassToSib pa $ NilSib nol) NilSibBranch)
      (CssOrphan nol))
    b
root_b :: OrClass '[ '[ '(NowOrLater, '[R], '[], '[])]] "b"
root_b =
  AddAncestorBranch
    (AddRoot $ CssOrphan nol)
    b
root_dir_body_dir_a_dir_b :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "a"], '[], '[])
     , '(JustNow, '[T BODY], '[], '[])
     , '(JustNow, '[R], '[], '[])
     ]
   ] "b"
root_dir_body_dir_a_dir_b =
  AddAncestorBranch
    ( NextAncestor acn .
      AddAncestor pa .
      NextAncestor jn .
      AddTagAncestor (Proxy @BODY) .
      NextAncestor jn .
      AddRoot $
      CssOrphan jn)
    b
