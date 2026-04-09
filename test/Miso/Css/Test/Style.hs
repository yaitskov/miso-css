{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.Style where

import Data.Proxy ( Proxy(Proxy) )
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Char8 qualified as C8
import Miso.Css.Operator
import Miso.Css.Prelude
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
    ]
  ]
  where
    go :: L.ByteString -> E en es cls '[] -> TestTree
    go ex el =
      testCase (C8.unpack $ C8.toStrict ex) do
        toHtml (eToView el) @?= ex
    a = TopOrClass (Proxy @"a")
    b = TopOrClass (Proxy @"b")
    c = TopOrClass (Proxy @"c")
