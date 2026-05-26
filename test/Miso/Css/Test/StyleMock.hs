{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}

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
import Miso.Css.Style as X
import Miso.Css.Style.PostAppend as Post
import Miso.Css.Tags as X
import Miso.Html as X ( ToHtml(toHtml) )
import Test.Tasty ( TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

domLbs :: forall m a en es r ei atrs kids cls ecs children.
  ( MapMaybeFilterOutFullyMatchedHead '[] ecs ~ '[]
  , DuplicatedIds kids ~ '[]
  ) =>
  E m a en es r ei atrs kids cls ecs children ->
  L.ByteString
domLbs = toHtml . toView

doNotTc :: forall m a en es r ei atrs kids cls ecs children.
  forall exDids -> (DuplicatedIds kids ~ exDids) =>
  forall exEcs -> (MapMaybeFilterOutFullyMatchedHead '[] ecs ~ exEcs) =>
  E m a en es r ei atrs kids cls ecs children ->
  TestTree
doNotTc _ _ _ =
  testCase "see type message from the checker" do
    () @?= ()

go :: forall m a en es r ei atrs kids cls ecs children.
  ( MapMaybeFilterOutFullyMatchedHead '[] ecs ~ '[]
  , DuplicatedIds kids ~ '[]
  ) =>
  L.ByteString -> E m a en es r ei atrs kids cls ecs children -> TestTree
go ex el =
  testCase (C8.unpack $ C8.toStrict ex) do
    domLbs el @?= ex
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
ul_a = AddAncestorBranch (AddSubSegConstraint (Proxy @T) pul $ CssOrphan nol) a
-- #x .a
id_a :: OrClass '[ '[ '(NowOrLater, '[I "b"], '[], '[])]] "a"
id_a = AddAncestorBranch (AddSubSegConstraint (Proxy @I) pb $ CssOrphan nol) a
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
nol_c :: OrClass '[ '[ '(NowOrLater, '[], '[], '[])]] "c"
nol_c = AddAncestorBranch (CssOrphan nol) c
jn_c :: OrClass '[ '[ '(JustNow, '[], '[], '[])]] "c"
jn_c =  AddAncestorBranch (CssOrphan jn) c
ac :: OrClass '[ '[ '(NowOrLater, '[C "a"], '[], '[])]] "c"
ac = AddAncestorBranch (AddSubSegConstraint (Proxy @C) pa $ CssOrphan nol) c

ab :: OrClass '[ [ '(AutoClean, '[], '[], '[])
                 , '(NowOrLater, '[C "a"], '[], '[])
                 ] ] "b"
ab = AddAncestorBranch (NextAncestor acn . AddSubSegConstraint (Proxy @C) pa $ CssOrphan nol) b


-- .a _ .b _ .c
abc :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(NowOrLater, '[C "b"], '[], '[])
     , '(NowOrLater, '[C "a"], '[], '[])
     ] ] "c"
abc =
  AddAncestorBranch
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @C) pb
  . NextAncestor nol
  . AddSubSegConstraint (Proxy @C) pa
  $ CssOrphan nol )
  c
-- .a.b _ .c
ab_c :: OrClass '[['(AutoClean, '[], '[], '[]), '(NowOrLater, [C "b", C "a"], '[], '[])]] "c"
ab_c =
  AddAncestorBranch
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @C) pb
  . AddSubSegConstraint (Proxy @C) pa
  $ CssOrphan nol )
  c
-- .b.a _ .c
ba_c :: OrClass '[ '[ '(NowOrLater, [C "a", C "b"], '[], '[])]] "c"
ba_c =
  AddAncestorBranch
  (AddSubSegConstraint (Proxy @C) pa . AddSubSegConstraint (Proxy @C) pb $ CssOrphan nol)
  c
-- #c.a _ .b
idC_and_a_b :: OrClass '[ '[ '(NowOrLater, [I "c", C "a"], '[], '[])]] "b"
idC_and_a_b =
  AddAncestorBranch
  (AddSubSegConstraint (Proxy @I) pc . AddSubSegConstraint (Proxy @C) pa $ CssOrphan nol)
  b
ul_and_a_b :: OrClass '[ '[ '(NowOrLater, [T "ul", C "a"], '[], '[])]] "b"
ul_and_a_b =
  AddAncestorBranch
  (AddSubSegConstraint (Proxy @T) pul . AddSubSegConstraint (Proxy @C) pa $ CssOrphan nol)
  b
-- id_a = AddAncestorBranch (AddSubSegConstraint (Proxy @I) pb $ CssOrphan nol) a
a_id_a :: OrClass '[ '[ '(AutoClean, '[I "a"], '[], '[])]] "a"
a_id_a =
  AddAncestorBranch
  (AddSubSegConstraint (Proxy @I) pa $ CssOrphan acn)
  a

-- .a.b
a_next_to_b :: OrClass '[ '[ '(AutoClean, '[C "a"], '[], '[])]] "b"
a_next_to_b =
  AddAncestorBranch
  (AddSubSegConstraint (Proxy @C) pa $ CssOrphan acn) b
-- .b.a
b_next_to_a :: OrClass '[ '[ '(AutoClean, '[C "b"], '[], '[])]] "a"
b_next_to_a =
  AddAncestorBranch
  (AddSubSegConstraint (Proxy @C) pb $ CssOrphan acn) a
-- .a[a]
a_wants_a_attr :: OrClass '[ '[ '(AutoClean, '[A "a"], '[], '[])]] "a"
a_wants_a_attr = AddAncestorBranch (AddSubSegConstraint (Proxy @A) pa $ CssOrphan acn) a
-- [a] > .a
a_wants_a_attr_in_parent :: OrClass '[ '[ '(JustNow, '[A "a"], '[], '[])]] "a"
a_wants_a_attr_in_parent = AddAncestorBranch (AddSubSegConstraint (Proxy @A) pa $ CssOrphan jn) a

pdiv :: Proxy "div"
pdiv = Proxy @"div"
div_child :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[T "div"], '[], '[])
     ] ]   "div_child"
div_child = -- div > .div_child
  AddAncestorBranch
  (NextAncestor acn . AddSubSegConstraint (Proxy @T) pdiv $ CssOrphan jn)
  (TopOrClass (Proxy @"div_child"))
div_ul_child :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[T "ul"], '[], '[])
     , '(JustNow, '[T "div"], '[], '[])
     ]
   ] "div_ul_child"
div_ul_child =
  AddAncestorBranch
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @T) pul
  . NextAncestor jn
  . AddSubSegConstraint (Proxy @T) pdiv
  $ CssOrphan jn
  )
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
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @C) pb
  . NextAncestor jn
  . AddSubSegConstraint (Proxy @C) pa
  $ CssOrphan nol)
  c
a_dir_b_sp_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(NowOrLater, '[C "b"], '[], '[])
     , '(JustNow, '[C "a"], '[], '[])
     ] ] "c"
a_dir_b_sp_c =
  AddAncestorBranch
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @C) pb
  . NextAncestor nol
  . AddSubSegConstraint (Proxy @C) pa
  $ CssOrphan jn )
  c
a_sp_b_dir_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "b"], '[], '[])
     , '(NowOrLater, '[C "a"], '[], '[])
     ] ] "c"
a_sp_b_dir_c =
  AddAncestorBranch
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @C) pb
  . NextAncestor jn
  . AddSubSegConstraint (Proxy @C) pa
  $ CssOrphan nol )
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
  (NextAncestor acn . AddSubSegConstraint (Proxy @C) pc .
   NextAncestor jn . AddSubSegConstraint (Proxy @C) pb .
   NextAncestor jn . AddSubSegConstraint (Proxy @C) pa $ CssOrphan jn)
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
a_dir_b = AddAncestorBranch (NextAncestor acn . AddSubSegConstraint (Proxy @C) pa $ CssOrphan jn) b
-- a_neighbour_b =
a_b_dir_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "b"], '[], '[])
     , '(NowOrLater, '[C "a"], '[], '[])
     ] ] "c"
a_b_dir_c =
  AddAncestorBranch
  ( NextAncestor acn
  . AddSubSegConstraint (Proxy @C) pb
  . NextAncestor jn
  . AddSubSegConstraint (Proxy @C) pa
  $ CssOrphan nol )
  c
-- .a + .b
a_dirSib_b :: OrClass
  '[ [ '(AutoClean,  '[], '[], '[])
     , '(NowOrLater, '[], '[], '[ '[ '(JustNow, '[C "a"])]])
     ] ] "b"
a_dirSib_b =
  AddAncestorBranch
    (NextAncestor acn $ AddSiblingBranch
      (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib jn) NilSibBranch)
      (CssOrphan nol))
    b
-- _ .c > .a + .b
c_dir_a_dirSib_b :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "c"], '[], '[ '[ '(JustNow, '[C "a"])]]) ] ] "b"
c_dir_a_dirSib_b =
  AddAncestorBranch
    ( NextAncestor acn
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib jn) NilSibBranch)
    . AddSubSegConstraint (Proxy @C) pc
    $ CssOrphan jn )
    b
-- _ .c > .a + .b _ .d
c_dir_a_dirSib_b_spc_d :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(NowOrLater, '[C "b"], '[], '[])
     , '(JustNow, '[C "c"], '[], '[ '[ '(JustNow, '[C "a"])]])
     ] ] "d"
c_dir_a_dirSib_b_spc_d =
  AddAncestorBranch
    ( NextAncestor acn
    . AddSubSegConstraint (Proxy @C) pb
    . NextAncestor nol
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib jn) NilSibBranch)
    . AddSubSegConstraint (Proxy @C) pc
    $ CssOrphan jn )
    d
-- .a.b > .c
a_with_b_dir_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, [C "b", C "a"], '[], '[])
     ] ] "c"
a_with_b_dir_c =
  AddAncestorBranch
    ( NextAncestor acn
    . AddSubSegConstraint (Proxy @C) pb
    . AddSubSegConstraint (Proxy @C) pa
    $ CssOrphan jn )
    c
-- .a.b + .c
a_with_b_dirSib_c :: OrClass '[ '[ '(JustNow, '[], '[], '[ '[ '(JustNow, [C "b", C "a"])]])]] "c"
a_with_b_dirSib_c =
  AddAncestorBranch
    ( AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @C) pb $ AddSib (Proxy @C) pa $ NilSib jn) NilSibBranch)
    $ CssOrphan jn ) -- or nol - does not matter
    c

pspan :: Proxy "span"
pspan = Proxy @"span"
pp :: Proxy "p"
pp = Proxy @"p"
-- div ~ p + span > .a + .b
div_genSib_p_dirSib_span_dir_a_dirSib_b :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[T "span"], '[], '[ '[ '(JustNow, '[C "a"])]])
     , '(JustNow, '[], '[], [ '[ '(JustNow, '[T "p"])], '[ '(NowOrLater, '[T "div"])]])
     ] ] "b"

div_genSib_p_dirSib_span_dir_a_dirSib_b =
  AddAncestorBranch
    ( NextAncestor acn
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib jn) NilSibBranch)
    . AddSubSegConstraint (Proxy @T) pspan
    . NextAncestor jn
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @T) pp $ NilSib jn) NilSibBranch)
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @T) pdiv $ NilSib nol) NilSibBranch)
    $ CssOrphan jn )
    b

-- .a + .b > .c
a_dirSib_b_dir_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(JustNow, '[C "b"], '[], '[])
     , '(JustNow, '[], '[], '[ '[ '(JustNow, '[C "a"]) ] ])
     ] ] "c"
a_dirSib_b_dir_c =
  AddAncestorBranch
    ( NextAncestor acn
    . AddSubSegConstraint (Proxy @C) pb
    . NextAncestor jn
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib jn) NilSibBranch)
    $ CssOrphan jn )
    c

-- .a ~ .b
a_genSib_b :: OrClass
  '[ [ '(AutoClean,  '[], '[], '[])
     , '(NowOrLater, '[], '[], '[ '[ '(NowOrLater, '[C "a"]) ]])
     ] ] "b"
a_genSib_b =
  AddAncestorBranch
    (NextAncestor acn $ AddSiblingBranch
      (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib nol) NilSibBranch)
      (CssOrphan nol))
    b

-- .a ~ .b _ .c
a_genSib_b_spc_c :: OrClass
  '[ [ '(AutoClean, '[], '[], '[])
     , '(NowOrLater, '[C "b"], '[], '[])
     , '(NowOrLater, '[], '[], '[ '[ '(NowOrLater, '[C "a"])]])
     ] ] "c"
a_genSib_b_spc_c =
  AddAncestorBranch
    ( NextAncestor acn
    . AddSubSegConstraint (Proxy @C) pb
    . NextAncestor nol
    . AddSiblingBranch (AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib nol) NilSibBranch)
    $ CssOrphan nol )
    c

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
      AddSubSegConstraint (Proxy @C) pa .
      NextAncestor jn .
      AddSubSegConstraint (Proxy @T) (Proxy @BODY) .
      NextAncestor jn .
      AddRoot $
      CssOrphan jn)
    b
