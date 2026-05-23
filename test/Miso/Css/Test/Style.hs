{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.Style where

import Miso.Css.Test.StyleMock
import Miso.Html ( ToHtml(toHtml) )
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MH
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
        -- groupped to easy comment/uncomment all at once
        -- , testGroup "should-fail-to-type-check"
        --   [ testGroup "dangling hierarchy relation"
        --     [ go """<div class="c"></div>""" $ div_ =. nol_c ]
        --   , testGroup "duplicated ID"
        --     [ go """<div id="b"><div id="b"></div></div>""" $ div_ =# pb </ div_ =# pb ]
        --   , go """<div id="a"><div class="a"></div></div>""" $ div_ =# pa </ (div_  =. a_id_a )
        --   , testGroup ".a is applied to this elem rather than parent one"
        --     [ go """<div class="a"><div class="b"></div></div>""" $
        --       div_ </ (div_ =. ab =. a)
        --     ]
        --   ]
        , testGroup "star"
          [ go """<div><ul><li class="c"></li></ul></div>""" $
            div_ </ (ul_ </ li_ =. star_dir_star_dir_c)
          ]
        , testGroup ":root"
          [ go """<div class="a"><div class="b"></div></div>"""
            (SealDomE (div_ =. a </ div_  =. root_b))
          , go """<div class="b"></div>""" $
            page (div_  =. root_b)
          , go """<div><div class="b"></div></div>"""
            (SealDomE $ VirtualBodyE (div_ </ div_  =. root_b))
          , go """<div class="a"><div class="b"></div></div>""" $
            page (div_ =. a </ div_ =. root_dir_body_dir_a_dir_b)
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
          ]
        , testGroup "id+class+raw"
          [ go """<div id="a" class="a"><p>h</p></div>""" $
            div_ =# pa =. a =< MH.p_ [] [ "h" ]
          , go """<div id="b"><div class="a"><i class="rc">aaa</i></div></div>""" $
            div_ =# pb </ (div_ =. id_a =<  MH.i_ [ MH.class_ "rc" ] [ "aaa" ])
          ]
        , testGroup "id+class"
          [ go """<div id="a" class="a"></div>""" $ div_ =# pa =. a_id_a
          , go """<div class="a" id="a"></div>""" $ div_ =. a_id_a =# pa
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
          , go """<div class="a"><div class="b"><div class="c"><div class="d"></div></div></div></div>""" $
            div_ =. a </ (div_ =. b </ (div_ =. c </ div_ =. a_dir_b_dir_c_dir_d))
          , go """<div class="a"><div class="b"></div></div>""" $
            div_ =. a </ div_ =. a_dir_b
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. b </ div_ =. a_b_dir_c)
          ]
        ]
      ]
    ]
  ]
