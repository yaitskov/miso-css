{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.Style where

import Data.Proxy ( Proxy(Proxy) )
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Char8 qualified as C8
import Miso.Css.Miso ( toView )
import Miso.Css.Operator ( (<@), (=.), (</), (=#) )
import Miso.Css.Prelude
import Miso.Css.Segment
import Miso.Css.Sibling
import Miso.Css.Style
import Miso.Css.Tags
import Miso.Html ( ToHtml(toHtml) )
import Miso.Html qualified as MH
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )


test_style :: TestTree
test_style =
  testGroup "Style"
  [ testCase "div_" do
      toHtml (MH.div_ [] []) @?= "<div></div>"
  , testGroup "eToView"
    [ testGroup "empty eacs"
      [ testGroup "bitter"
        [ testGroup "class"
          [ "cdata hello" `go` CDataE "cdata hello"
          -- No instance for ‘ghc-internal-9.1202.0:GHC.Internal.Data.String.IsString
          --                      (E en0 es0 cls0 '[])’
          --                        arising from the literal ‘"cdata hello"’
          -- , "cdata hello" `go` "cdata hello"
          , "<div></div>" `go` NilE (Proxy @"div")
          , """<div class="c"></div>""" `go`
            AppClsE (TopOrClass (Proxy @"c")) (NilE (Proxy @"div"))
          , """<div class="c">cd</div>""" `go`
            AppendChildE
              (CDataE "cd")
              (AppClsE (TopOrClass (Proxy @"c"))
                (NilE (Proxy @"div")))
          , """<div class="c"><br/></div>""" `go`
            AppendChildE
              (NilE $ Proxy @"br")
              (AppClsE (TopOrClass (Proxy @"c"))
                (NilE (Proxy @"div")))
          , """<div class="a"><div class="b"></div></div>""" `go`
            AppendChildE
              (AppClsE (TopOrClass (Proxy @"b"))
                (NilE $ Proxy @"div"))
              (AppClsE (TopOrClass (Proxy @"a"))
                (NilE (Proxy @"div")))
          ]
        ]
      , testGroup "sweet"
        [ go "<div> </div>" $ div_ <@ ""
        , go """<div class="c"> </div>""" $ div_ =. c <@ ""
        , go """<div class="c">cd</div>""" $
          div_ =. c <@ "cd"
        , go """<div class="c"><hr/></div>""" $ div_ =. c </ hr_
        , go """<div class="a"><div class="b"></div></div>""" $
          div_ =. a </ div_ =. b
        , go """<div class="a"><div class="b"></div><div class="c"></div></div>""" $
          div_ =. a </ div_ =. b </ div_ =. c
        , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
          div_ =. a </ (div_ =. b </ div_ =. c)
        ]
      ]
    , testGroup "non-empty-eacs"
      [ testGroup "bitter"
        [ """<div class="a"><div class="b"></div></div>""" `go`
          AppendChildE
          (AppClsE ab (NilE (Proxy @"div")))
          (AppClsE a  (NilE (Proxy @"div")))
        , """<div class="a"><div class="b"><div class="c"></div></div></div>""" `go`
          AppendChildE
          (AppendChildE
           (AppClsE abc (NilE (Proxy @"div")))
           (AppClsE b   (NilE (Proxy @"div"))))
          (AppClsE a (NilE (Proxy @"div")))
        , """<div class="a"><div class="b"><div class="c"></div></div></div>""" `go`
          AppendChildE
          (AppendChildE
           (AppClsE abc (NilE (Proxy @"div")))
           (AppClsE ab  (NilE (Proxy @"div"))))
          (AppClsE a
           (NilE (Proxy @"div")))
        ]
      , testGroup "sweet"
        [ testGroup "tag"
          [ go """<ul><li class="a"></li></ul>""" $
            ul_ </ li_ =. ul_a
          , go """<ul class="a"><li class="b"></li></ul>""" $
            ul_ =. a </ li_ =. ul_and_a_b
          , go """<div><div class="div_child"></div></div>""" $
            div_ </ div_ =. div_child
          , go """<div><ul><div class="div_ul_child"></div></ul></div>""" $
            div_ </ (ul_ </ div_ =. div_ul_child)
          ]
        , testGroup "star"
          [ go """<div><ul><li class="c"></li></ul></div>""" $
            div_ </ (ul_ </ li_ =. star_dir_star_dir_c)
          ]
        , testGroup "sibling"
          [ testGroup "adjacent"
            [ go """<div><div class="a"></div><div class="b"></div></div>""" $
              div_ </ div_ =. a </ div_ =. a_dirSib_b
            ]
          , testGroup "general"
            [ go """<div><div class="a"></div><span></span><div class="b"></div></div>""" $
              div_ </ div_ =. a </ span_ </ div_ =. a_genSib_b
            ]
          ]
        , testGroup "id"
          [ go """<div id="a"></div>""" $ div_ =# pa
          , go """<div id="a"><div id="b"></div></div>""" $ div_ =# pa </ div_ =# pb
          -- Should Not Check due to duplicated ID
          -- , go """<div id="b"><div id="b"></div></div>""" $ div_ =# pb </ div_ =# pb
          ]
        , testGroup "id+class"
          [ go """<div id="a" class="a"></div>""" $ div_ =# pa =. a
          , go """<div id="a"><div class="b"></div></div>""" $
            div_ =# pa </ div_ =. b
          , go """<div id="b"><div class="a"></div></div>""" $
            div_ =# pb </ div_ =. id_a
          , go """<div id="c" class="a"><div class="b"></div></div>""" $
            div_ =# pc =. a </ div_ =. idC_and_a_b
          ]
        , testGroup "class"
          [ go """<div class="a"><div class="b"></div></div>""" $
            div_ =. a </ (div_ =. ab)
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. ab </ div_ =. abc)
          , go """<div class="a"><div class="b"><ul><div class="c"></div></ul></div></div>""" $
            div_ =. a </ (div_ =. ab </ (ul_ </ div_ =. abc))
          , go """<div class="a"><div class="b"></div><div class="c"></div></div>""" $
            div_ =. a </ div_ =. ab </ div_ =. ac
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. ab </ div_ =. ac)
          , go """<div class="a b"><div class="c"></div></div>""" $
            div_ =. a =. b </ (div_ =. ab_c)
          , go """<div class="a b"><div class="c"></div></div>""" $
            div_ =. a =. b </ (div_ =. ba_c)
          , go """<div class="b a"><div class="c"></div></div>""" $
            div_ =. b =. a </ (div_ =. ba_c)
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. b </ div_ =. a_dir_b_dir_c)
          , go """<div class="a"><div class="b"></div></div>""" $
            div_ =. a </ div_ =. a_dir_b
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. b </ div_ =. a_b_dir_c)
          ]
       ]
     ]
   ]
  ]
  where
    go :: L.ByteString -> E en es ei kids cls '[] children -> TestTree
    go ex el =
      testCase (C8.unpack $ C8.toStrict ex) do
        toHtml (toView el) @?= ex
    a = TopOrClass pa
    b = TopOrClass pb
    c = TopOrClass pc
    pul = Proxy @"ul"
    ul_a = AddAncestorBranch (AddTagAncestor pul $ CssOrphan nol) a
    -- #x .a
    id_a = AddAncestorBranch (AddIdAncestor pb $ CssOrphan nol) a
    pa = Proxy @"a"
    pb = Proxy @"b"
    pc = Proxy @"c"
    nol = Proxy @NowOrLater
    jn = Proxy @JustNow
    ac = AddAncestorBranch (AddAncestor pa $ CssOrphan nol) c
    ab = AddAncestorBranch (AddAncestor pa $ CssOrphan nol) b
    -- .a  .b  .c
    abc =
      AddAncestorBranch
      (AddAncestor pb . NextAncestor nol . AddAncestor pa $ CssOrphan nol)
      c
    ab_c =
      AddAncestorBranch
      (AddAncestor pb . AddAncestor pa $ CssOrphan nol)
      c
    ba_c =
      AddAncestorBranch
      (AddAncestor pa . AddAncestor pb $ CssOrphan nol)
      c
    idC_and_a_b =
      AddAncestorBranch
      (AddIdAncestor pc . AddAncestor pa $ CssOrphan nol)
      b
    ul_and_a_b =
      AddAncestorBranch
      (AddTagAncestor pul . AddAncestor pa $ CssOrphan nol)
      b
    pdiv = Proxy @"div"
    div_child =
      AddAncestorBranch
      (AddTagAncestor pdiv $ CssOrphan jn)
      (TopOrClass (Proxy @"div_child"))
    div_ul_child =
      AddAncestorBranch
      (AddTagAncestor pul . NextAncestor jn . AddTagAncestor pdiv $ CssOrphan jn)
      (TopOrClass (Proxy @"div_ul_child"))
    -- .a > .b > .c
    a_dir_b_dir_c =
      AddAncestorBranch
      (AddAncestor pb . NextAncestor jn . AddAncestor pa $ CssOrphan jn)
      c
    -- * > * > .c
    star_dir_star_dir_c =
      AddAncestorBranch
      (NextAncestor jn $ CssOrphan jn)
      c
    a_dir_b = AddAncestorBranch (AddAncestor pa $ CssOrphan jn) b
    -- a_neighbour_b =
    a_b_dir_c =
      AddAncestorBranch
      (AddAncestor pb . NextAncestor jn . AddAncestor pa $ CssOrphan nol)
      c
    a_dirSib_b =
      AddAncestorBranch
        (AddSiblingBranch
          (AddSegToSibBranch (AddClassToSib pa $ NilSib jn) NilSibBranch)
          (CssOrphan nol))
        b
    a_genSib_b =
      AddAncestorBranch
        (AddSiblingBranch
          (AddSegToSibBranch (AddClassToSib pa $ NilSib nol) NilSibBranch)
          (CssOrphan nol))
        b
