{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.Style where

import Data.Proxy ( Proxy(Proxy) )
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Char8 qualified as C8
import Miso.Css.Miso ( toView )
import Miso.Css.Operator ( (<@), (=.), (</), (=#) )
import Miso.Css.Prelude
import Miso.Css.Style
import Miso.Css.Tags ( div_, hr_, ul_, li_ )
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
          (AppClsE
            (AddAncestorBranch
              (AddAncestor (Proxy @"a") CssOrphan)
              (TopOrClass (Proxy @"b")))
            (NilE (Proxy @"div")))
          (AppClsE (TopOrClass (Proxy @"a"))
            (NilE (Proxy @"div")))
        , """<div class="a"><div class="b"><div class="c"></div></div></div>""" `go`
          AppendChildE
          (AppendChildE
           (AppClsE
             (AddAncestorBranch
               (AddAncestor (Proxy @"b") (AddAncestor (Proxy @"a") CssOrphan))
               (TopOrClass (Proxy @"c")))
             (NilE (Proxy @"div")))
           (AppClsE (TopOrClass (Proxy @"b"))
             (NilE (Proxy @"div"))))
          (AppClsE (TopOrClass (Proxy @"a"))
           (NilE (Proxy @"div")))
        , """<div class="a"><div class="b"><div class="c"></div></div></div>""" `go`
          AppendChildE
          (AppendChildE
           (AppClsE
             (AddAncestorBranch
               (AddAncestor (Proxy @"b") (AddAncestor (Proxy @"a") CssOrphan))
               (TopOrClass (Proxy @"c")))
             (NilE (Proxy @"div")))
           (AppClsE
            (AddAncestorBranch
             (AddAncestor (Proxy @"a") CssOrphan)
              (TopOrClass (Proxy @"b")))
             (NilE (Proxy @"div"))))
          (AppClsE (TopOrClass (Proxy @"a"))
           (NilE (Proxy @"div")))
        ]
      , testGroup "sweet"
        [ testGroup "tag"
          [ go """<ul><li class="a"></li></ul>""" $
            ul_ </ li_ =. ul_a
          ]
        , testGroup "id"
          [ go """<div id="a"></div>""" $ div_ =# pa
          , go """<div id="a"><div id="b"></div></div>""" $ div_ =# pa </ div_ =# pb
          -- Should Not Check due to duplicated ID
          -- , go """<div id="b"><div id="b"></div></div>""" $ div_ =# pb </ div_ =# pb
          ]
        , testGroup "id+class"
          [ go """<div id="a" class="a"></div>""" $ div_ =# pa =. a
          , go """<div id="a"><div class="b"></div></div>""" $ div_ =# pa </ div_ =. b
          , go """<div id="b"><div class="a"></div></div>""" $ div_ =# pb </ div_ =. id_a
          ]
        , testGroup "class"
          [ go """<div class="a"><div class="b"></div></div>""" $
            div_ =. a </ (div_ =. ab)
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. ab </ div_ =. abc)
          , go """<div class="a"><div class="b"></div><div class="c"></div></div>""" $
            div_ =. a </ div_ =. ab </ div_ =. ac
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. ab </ div_ =. ac)
          ]
        ]
      ]
    ]
  ]
  where
    go :: L.ByteString -> E en es ei kids cls '[] -> TestTree
    go ex el =
      testCase (C8.unpack $ C8.toStrict ex) do
        toHtml (toView el) @?= ex
    a = TopOrClass (Proxy @"a")
    b = TopOrClass (Proxy @"b")
    c = TopOrClass (Proxy @"c")
    ul_a = AddAncestorBranch (AddTagAncestor (Proxy @"ul") CssOrphan) a
    -- #x .a
    id_a = AddAncestorBranch (AddIdAncestor (Proxy @"b") CssOrphan) a
    pa = Proxy @"a"
    pb = Proxy @"b"
    ac = AddAncestorBranch (AddAncestor pa CssOrphan) c
    ab = AddAncestorBranch (AddAncestor pa CssOrphan) b
    abc = AddAncestorBranch
            (AddAncestor pb (AddAncestor pa CssOrphan))
            c
