{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.Style where

import Data.Proxy ( Proxy(Proxy) )
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Char8 qualified as C8
import Miso.Css.Style
import Miso.Css.Prelude
import Miso.Html ( div_, ToHtml(toHtml) )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

test_style :: TestTree
test_style =
  testGroup "Style"
  [ testCase "div_" do
      toHtml (div_ [] []) @?= "<div></div>"
  , testGroup "eToView"
    [ testGroup "empty eacs"
      [ "cdata hello" `go` CDataE "cdata hello"
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
