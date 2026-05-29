{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Miso.Css.Test.Style where

import Miso (MisoString)
import Miso.Css.Test.StyleMock
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MH

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
        , testGroup "misc"
          [ testGroup "dangling hierarchy relation"
            [ doNotTc [] [] $ div_ =. nol_c
            , doNotTc [] [] $ div_ =. jn_c
            ]
          ]
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
            , testGroup ".a + .b"
              [ doNotTc [] [[[(NowOrLater, [B], [], [[(JustNow, [B]), (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a_dirSib_b
              ]
            , go """<div><div class="a"></div><div class="b"><div class="c"></div></div></div>"""  $
              div_ </ div_ =. a </ (div_ =. b </ div_ =. a_dirSib_b_dir_c)
            , testGroup "div between a and b"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "a"])]])]]] $
                div_ </ div_ =. a </ div_ </ (div_ =. b </ div_ =. a_dirSib_b_dir_c)
              ]
            , testGroup "a missing"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "a"])]])]]] $
                div_ </ div_ </ (div_ =. b </ div_ =. a_dirSib_b_dir_c)
              ]
            , testGroup "a and b are flipped"
              [ doNotTc [] [[[ (JustNow, [B], [], [[(JustNow, [B]), (JustNow, [C "a"])]])]]] $
                div_ </ (div_ =. b </ div_ =. a_dirSib_b_dir_c) </ div_ =. a
              ]
            , testGroup "b missing"
              [ doNotTc [] [[[(JustNow, [B, C "b"], [], []),(JustNow, [], [], [[ (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ (div_ </ div_ =. a_dirSib_b_dir_c)
              ]
            , testGroup "b in root 1"
              [ doNotTc [] [[[ (JustNow, [B], [], [[(JustNow, [B]), (JustNow, [C "a"])]])]]] $
                div_ =. b </ div_ =. a </ div_ =. a_dirSib_b_dir_c
              ]
            , testGroup "b in root 2"
              [ doNotTc [] [[[(JustNow, [B], [C "b"], []), (JustNow, [], [], [[ (JustNow, [C "a"])]])]]] $
                div_ =. b </ div_ =. a </ (div_ </ div_ =. a_dirSib_b_dir_c)
              ]
            , testGroup "b with c 1"
              [ doNotTc [] [[[(JustNow, [B, C "b"], [], []), (JustNow, [], [], [[ (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ div_ =. a_dirSib_b_dir_c =. b
              ]
            , testGroup "b with c 2"
              [ doNotTc [] [[[(JustNow, [B, C "b"], [], []), (JustNow, [], [], [[ (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ (div_ </ div_ =. a_dirSib_b_dir_c =. b)
              ]
            , testGroup "c is wrapped in div"
              [ doNotTc [] [[[(JustNow, [B], [C "b"], []), (JustNow, [], [], [[(JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ (div_ =. b </ (div_ </ div_ =. a_dirSib_b_dir_c))
              ]
            , go """<div><div class="a"></div><div class="b"></div><div class="c"></div></div>""" $
              div_ </ div_ =. a </ div_ =. b </ div_ =. a_dirSib_b_dirSib_c
            , testGroup "flipped a and b"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "b"]), (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. b </ div_ =. a </ div_ =. a_dirSib_b_dirSib_c
              ]
            , testGroup "extra elem between a and b"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "a"])]])]]] $
                div_ </ div_ =. a </ div_ </ div_ =. b </ div_ =. a_dirSib_b_dirSib_c
              ]
            , testGroup "extra elem between b and c"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "b"]), (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ div_ =. b </ div_ </ div_ =. a_dirSib_b_dirSib_c
              ]
            , testGroup "missing a class"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "a"])]])]]] $
                div_ </ div_ </ div_ =. b </ div_ =. a_dirSib_b_dirSib_c
              ]
            , testGroup "missing a tag"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B]), (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. b </ div_ =. a_dirSib_b_dirSib_c
              ]
            , testGroup "missing b tag"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, C "b"]), (JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ div_ =. a_dirSib_b_dirSib_c
              ]
            , go """<div class="c"><div class="a"></div><div class="b"></div></div>"""  $
              div_ =. c </ div_ =. a </ div_ =. c_dir_a_dirSib_b
            , go """<div class="c"><div class="b"></div><div class="a"></div><div class="b"></div></div>"""  $
              div_ =. c </ div_ =. b </ div_ =. a </ div_ =. c_dir_a_dirSib_b
            , go """<div class="c"><div></div><div class="a"></div><div class="b"></div></div>"""  $
              div_ =. c </ div_ </ div_ =. a </ div_ =. c_dir_a_dirSib_b
            , go """<div class="c"><div class="a"></div><div class="b"></div><div></div></div>"""  $
              div_ =. c </ div_ =. a </ div_ =. c_dir_a_dirSib_b </ div_
            , go """<div class="c"><div class="a"></div><div class="b"><div class="d"></div></div></div>"""  $
              div_ =. c </ div_ =. a </ (div_ =. b </ div_ =. c_dir_a_dirSib_b_spc_d)
            , go """<div class="c"><div class="a"></div><div class="b"><div><div class="d"></div></div></div></div>"""  $
              div_ =. c </ div_ =. a </ (div_ =. b </ (div_ </ div_ =. c_dir_a_dirSib_b_spc_d))
            , testGroup "parent c is missing"
              [ doNotTc [] [[[(JustNow, [B, C "c"], [], [[(JustNow, [C "a"])]])]]] $
                div_ </ div_ =. a </ div_ =. c_dir_a_dirSib_b
              ]
            , testGroup "extra node between .a and .b"
              [ doNotTc [] [[[(JustNow, [B], [C "c"], [[(JustNow, [B, C "a"])]])]]] $
                div_ =. c </ div_ =. a </ div_ </ div_ =. c_dir_a_dirSib_b
              ]
            , testGroup "extra node between .c and .a + .b"
              [ doNotTc [] [[[(JustNow, [B], [C "c"], [[(JustNow, [C "a"])]])]]] $
                div_ =. c </ (div_ </ (div_ =. a </ div_ =. c_dir_a_dirSib_b))
              ]
            , testGroup "a and b wrapped into extra div"
              [ doNotTc [] [[[(JustNow, [B], [C "c"], [[(JustNow, [C "a"])]])]]] $
                div_ =. c  </ (div_ </ div_ =. a </ (div_ =. b </ div_ =. c_dir_a_dirSib_b_spc_d))
              ]
            , testGroup "parent c is missing"
              [ doNotTc [] [[[(JustNow, [B, C "c"], [], [[(JustNow, [C "a"])]])]]] $
                div_  </ div_ =. a </ (div_ =. b </ div_ =. c_dir_a_dirSib_b_spc_d)
              ]
            , go """<div><div class="a b"></div><div class="c"></div></div>""" $
              div_ </ div_ =. a =. b </ div_ =. a_with_b_dirSib_c
            ]
          , testGroup "general"
            [ go """<div><div class="a"></div><span></span><div class="b"></div></div>""" $
              div_ </ div_ =. a </ span_ </ div_ =. a_genSib_b
            , go """<div><div class="a"></div><div class="b"><div class="c"></div></div></div>"""  $
              div_ </ div_ =. a </ (div_ =. b </ div_ =. a_genSib_b_spc_c)
            , go """<div><div class="a"></div><div></div><div class="b"><div class="c"></div></div></div>"""  $
              div_ </ div_ =. a </ div_ </ (div_ =. b </ div_ =. a_genSib_b_spc_c)
            , go """<div><div class="a"></div><div></div><div></div><div class="b"><div class="c"></div></div></div>"""  $
              div_ </ div_ =. a </ div_ </ div_ </ (div_ =. b </ div_ =. a_genSib_b_spc_c)
            , go """<div><div class="a"></div><div class="b"><div><div class="c"></div></div></div></div>"""  $
              div_ </ div_ =. a </ (div_ =. b </ (div_ </ div_ =. a_genSib_b_spc_c))
            , go """<div><div class="a"></div><div class="b"><div><div><div class="c"></div></div></div></div></div>"""  $
              div_ </ div_ =. a </ (div_ =. b </ (div_ </ (div_ </ div_ =. a_genSib_b_spc_c)))
            , testGroup "a missing"
              [ doNotTc [] [[[(NowOrLater, [B], [], [[(JustNow, [B]), (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ </ (div_ =. b </ div_ =. a_genSib_b_spc_c)
              ]
            , testGroup "a in parent"
              [ doNotTc [] [[[(NowOrLater, [B], [], [[(JustNow, [B]), (NowOrLater, [C "a"])]])]]] $
                div_ =. a </ div_ </ (div_ =. b </ div_ =. a_genSib_b_spc_c)
              ]
            , testGroup "b missing"
              [ doNotTc [] [[[(NowOrLater, [C "b"], [], []), (NowOrLater, [], [], [[ (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ =. a </ (div_ </ div_ =. a_genSib_b_spc_c)
              ]
            , testGroup "b on same tag with c"
              [ doNotTc [] [[[(NowOrLater, [C "b"], [], []), (NowOrLater, [], [], [[ (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ =. a </ (div_ </ div_ =. a_genSib_b_spc_c =. b)
              ]
            , go """<div><div class="a"></div><div class="b"></div><div class="c"></div></div>""" $
              div_ </ div_ =. a </ div_ =. b </ div_ =. a_genSib_b_genSib_c
            , testGroup "flipped a and b"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B]), (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ =. b </ div_ =. a </ div_ =. a_genSib_b_genSib_c
              ]
            , testGroup "extra elem between a and b"
              [ go """<div><div class="a"></div><div></div><div class="b"></div><div class="c"></div></div>""" $
                div_ </ div_ =. a </ div_ </ div_ =. b </ div_ =. a_genSib_b_genSib_c
              ]
            , testGroup "extra elem between b and c"
              [ go """<div><div class="a"></div><div class="b"></div><div></div><div class="c"></div></div>""" $
                div_ </ div_ =. a </ div_ =. b </ div_ </ div_ =. a_genSib_b_genSib_c
              ]
            , testGroup "prepended div before a"
              [ go """<div><div></div><div class="a"></div><div class="b"></div><div class="c"></div></div>""" $
                div_ </ div_ </ div_ =. a </ div_ =. b </ div_ =. a_genSib_b_genSib_c
              ]
            , testGroup "appended div after c"
              [ go """<div><div class="a"></div><div class="b"></div><div class="c"></div><div></div></div>""" $
                div_ </ div_ =. a </ div_ =. b </ div_ =. a_genSib_b_genSib_c </ div_
              ]
            , testGroup "missing a class"
              [ doNotTc [] [[[(JustNow, [B], [], [[ (JustNow, [B])
                                                  , (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ </ div_ =. b </ div_ =. a_genSib_b_genSib_c
              ]
            , testGroup "missing a tag"
              [ doNotTc [] [[[(JustNow, [B], [],[[ (JustNow, [B])
                                                 , (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ =. b </ div_ =. a_genSib_b_genSib_c
              ]
            , testGroup "missing b tag"
              [ doNotTc [] [[[(JustNow, [B], [], [[ (JustNow, [B])
                                                  , (NowOrLater, [C "b"])
                                                  , (NowOrLater, [C "a"])]])]]] $
                div_ </ div_ =. a </ div_ =. a_genSib_b_genSib_c
              ]
            ]
          , testGroup "mix"
            [ go """<div><div></div><p></p><span><div class="a"></div><div class="b"></div></span></div>"""  $
              div_ </ div_ </ p_ </ (span_ </ div_ =. a </ div_ =. div_genSib_p_dirSib_span_dir_a_dirSib_b)
            , go """<div><div></div><p></p><p></p><span><div class="a"></div><div class="b"></div></span></div>"""  $
              div_ </ div_ </ p_ </ p_ </ (span_ </ div_ =. a </ div_ =. div_genSib_p_dirSib_span_dir_a_dirSib_b)
            , go """<div><div></div><p></p><span><p></p><div class="a"></div><div class="b"></div></span></div>"""  $
              div_ </ div_ </ p_ </ (span_ </ p_ </ div_ =. a </ div_ =. div_genSib_p_dirSib_span_dir_a_dirSib_b)
            , testGroup "li tag is between p and span"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, T "p"]), (NowOrLater, [T "div"])]])]]] $
                div_ </ div_ </ p_ </ li_ </ (span_ </ div_ =. a </ div_ =. div_genSib_p_dirSib_span_dir_a_dirSib_b)
              ]
            , testGroup ".a and .b are wrapped into extra div"
              [ doNotTc [] [[[ (JustNow, [B], [T "span"], [ [ (JustNow, [C "a"])]])
                              , (JustNow, [], [], [[ (JustNow, [T "p"])
                                                   , (NowOrLater, [T "div"])]])]]] $
                div_ </ div_ </ p_ </ (span_ </ (div_ </ div_ =. a </ div_ =. div_genSib_p_dirSib_span_dir_a_dirSib_b))
              ]
            ]
          , testGroup "mix2"
            [ go """<div><div></div><p></p><span><div class="a"></div><div class="b"></div></span></div>"""  $
              div_ </ div_ </ p_ </ (span_ </ div_ =. a </ div_ =. div_dirSib_p_genSib_span_dir_a_dirSib_b)
            , go """<div><li></li><div></div><p></p><span><div class="a"></div><div class="b"></div></span></div>"""  $
              div_ </ li_ </ div_ </ p_ </ (span_ </ div_ =. a </ div_ =. div_dirSib_p_genSib_span_dir_a_dirSib_b)
            , go """<div><div></div><p></p><li></li><span><div class="a"></div><div class="b"></div></span></div>"""  $
              div_ </ div_ </ p_ </ li_ </ (span_ </ div_ =. a </ div_ =. div_dirSib_p_genSib_span_dir_a_dirSib_b)
            , testGroup "li tag is between div and p"
              [ doNotTc [] [[[(JustNow, [B], [], [[(JustNow, [B, T "div"])]])]]] $
                div_ </ div_ </ li_ </ p_ </ (span_ </ div_ =. a </ div_ =. div_dirSib_p_genSib_span_dir_a_dirSib_b)
              ]
            , testGroup ".a and .b are wrapped into extra div"
              [ doNotTc [] [[[ (JustNow, [B], [T "span"], [[(JustNow, [C "a"])]])
                             , (JustNow, [], [], [[(NowOrLater, [T "p"]), (JustNow, [T "div"])]])]]] $
                div_ </ div_ </ p_ </ (span_ </ (div_ </ div_ =. a </ div_ =. div_dirSib_p_genSib_span_dir_a_dirSib_b))
              ]
            ]
          ]
        , testGroup "id"
          [ go """<div id="a"></div>""" $ div_ =# pa
          , go """<div id="a"><div id="b"></div></div>""" $ div_ =# pa </ div_ =# pb
          , testGroup "duplicated ID"
            [ doNotTc [DuplicatedId "b"] []  $ div_ =# pb </ div_ =# pb
            , doNotTc [DuplicatedId "b"] []  $ div_ </ div_ =# pb </ div_ =# pb
            , doNotTc [DuplicatedId "b"] []  $ div_ </ div_ =# pb </ div_ </ div_ =# pb
            , doNotTc [DuplicatedId "b"] []  $ div_ =# pb </ (div_ </ (div_ </ div_ =# pb))
            , doNotTc [DuplicatedId "b"] []  $ div_ =# pb </ (div_ =# pc </ (div_ =# pa </ div_ =# pb))
            ]
          ]
        , testGroup "attr"
          [ go """<div a="av"><div class="a"></div></div>""" $
            div_ =<| atr @"a" av </ div_ =. a_wants_a_attr_in_parent
          , go """<div class="a" a="av"></div>""" $  div_ =. a_wants_a_attr =<| atr @"a" av
          , go """<div class="a" a="av"></div>""" $  div_ =. a_wants_a_attr =<| atr @"a" av =<| atr @"a" av
          , go """<div a="av" class="a"></div>""" $  div_ =<| atr @"a" av =. a_wants_a_attr =<| atr @"a" av
          , go """<div a="av" class="a"></div>""" $  div_ =<| atr @"a" av =. a_wants_a_attr
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
          , testGroup "#a is on parent but required to be in tag with class"
            [ doNotTc [] [[[(AutoClean, [B], [I "a"], [])]]] $
              div_ =# pa </ (div_  =. a_id_a )
            ]
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
          , go """<div class="a b"><div class="c"></div></div>""" $
            div_ =. a =. b </ (div_ =. a_with_b_dir_c)
          , go """<div class="b a"><div class="c"></div></div>""" $
            div_ =. b =. a </ (div_ =. a_with_b_dir_c)
          , go """<div class="b c a"><div class="c"></div></div>""" $
            div_ =. b =. c =. a </ (div_ =. a_with_b_dir_c)
          , testGroup "extra div around c"
            [ doNotTc [] [[[(JustNow, [B], [C "b", C "a"], [])]]] do
              div_ =. b =. a </ (div_ </ div_ =. a_with_b_dir_c)
            ]
          , testGroup "a missing"
            [ doNotTc [] [[[(JustNow, [B, C "a"], [C "b"], [])]]] do
              div_ =. b =. b </ div_ =. a_with_b_dir_c
            ]
          , testGroup "a is parent of b"
            [ doNotTc [] [[[(JustNow, [B], [C "a", C "b"], [])]]] do
              div_ =. a </ (div_ =. b </ div_ =. a_with_b_dir_c)
            ]
          , testGroup "a and b are applied to siblings"
            [ doNotTc [] [[[(JustNow, [B, C "a"], [C "b"],[])]]] do
              div_ </ div_ =. a </ (div_ =. b </ div_ =. a_with_b_dir_c)
            ]
          , testGroup ".a is applied to this elem rather than parent one"
            [ doNotTc [] [[[(NowOrLater, [C "a"], [], [])]]] do
              div_ </ (div_ =. ab =. a)
            ]
          , testGroup ".a is not applied to parent elem"
            [ doNotTc [] [[[(NowOrLater, [C "a"], [], [])]]] do
              div_ </ (div_ =. ab)
            , doNotTc [] [[[(NowOrLater, [C "a"], [], [])]]] do
              div_ =. ab
            ]
          , go """<div class="b a"></div>""" $
            div_ =. a_next_to_b =. b_next_to_a
          , go """<div class="a b"></div>""" $
            div_  =. b_next_to_a =. a_next_to_b
          , go """<div class="a b"></div>""" $
            div_  =. b_next_to_a =. a_next_to_b
          , go """<div class="a b a"></div>""" $
            div_  =. b_next_to_a =. a_next_to_b =. b_next_to_a
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. ab </ div_ =. abc)
          , testGroup "b is missing"
            [ doNotTc [] [[[(NowOrLater, [C "b"], [], []), (NowOrLater, [C "a"], [], [])]]] $
              div_ =. a </ (div_ </ div_ =. abc)
            , doNotTc [] [[[(NowOrLater, [C "b"], [], []), (NowOrLater, [C "a"], [], [])]]] $
              div_ =. a </ div_ =. abc
            ]
          , testGroup "a and b are flipped"
            [ doNotTc [] [[[(NowOrLater, [C "a"], [], [])]]] $
              div_ =. b </ (div_ =. a </ div_ =. abc)
            ]
          , testGroup "a is missing"
            [ doNotTc [] [[[(NowOrLater, [C "a"], [], [])]]] $
              div_ =. b </ div_ =. abc
            ]
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
          , go """<div class="a"><div class="b"><p><div class="c"></div></p></div></div>""" $
            div_ =. a </ (div_ =. b </ (p_ </ div_ =. a_dir_b_sp_c))
          , go """<div class="a"><p><div class="b"><div class="c"></div></div></p></div>""" $
            div_ =. a </ (p_ </ (div_ =. b </ div_ =. a_sp_b_dir_c))
          , go """<div class="a"><div class="b"><div class="c"><div class="d"></div></div></div></div>""" $
            div_ =. a </ (div_ =. b </ (div_ =. c </ div_ =. a_dir_b_dir_c_dir_d))
          , go """<div class="a"><div class="b"></div></div>""" $
            div_ =. a </ div_ =. a_dir_b
          , testGroup ".a > .b"
            [ testGroup "a is missing"
              [ doNotTc [] [[[(JustNow, [B, C "a"], [], [])]]] $ div_ </ div_ =. a_dir_b
              , doNotTc [] [[[(JustNow, [C "a"], [], [])]]] $ div_ =. a_dir_b
              ]
            ]
          , go """<div class="a"><div class="b"><div class="c"></div></div></div>""" $
            div_ =. a </ (div_ =. b </ div_ =. a_dir_b_spc_c)
          , go """<div class="a"><div class="b"><div><div class="c"></div></div></div></div>""" $
            div_ =. a </ (div_ =. b </ (div_ </ div_ =. a_dir_b_spc_c))

          , go """<div class="c"><div class="a b"></div></div>""" $
            div_ =. c </ div_ =. a =. c_dir_a_with_b
          , go """<div class="c"><div class="b a"></div></div>""" $
            div_ =. c </ div_ =. c_dir_a_with_b =. a
          , go """<div class="c"><div></div><div class="b a"></div></div>""" $
            div_ =. c </ div_ </ div_ =. c_dir_a_with_b =. a
          , go """<div class="c"><div class="b a"></div><div></div></div>""" $
            div_ =. c </ div_ =. c_dir_a_with_b =. a </ div_
          , testGroup "a is missing"
            [ doNotTc [] [[[(AutoClean, [B, C "a"], [], []), (JustNow, [C "c"], [], [])]]] $
              div_ =. c  </ div_ =. c_dir_a_with_b
            ]
          , testGroup "c is missing"
            [ doNotTc [] [[[(JustNow, [B, C "c"], [], [])]]] $
              div_ </ div_ =. a =. c_dir_a_with_b
            ]
          , testGroup "c is sibling"
            [ doNotTc [] [[[(JustNow, [B, C "c"], [], [])]]] $
              div_  </ div_ =. c </ div_ =. a =. c_dir_a_with_b
            ]
          , testGroup "c and a are mixed"
            [ doNotTc [] [[[(AutoClean, [B], [C "a"], []), (JustNow, [C "c"], [], [])]]] $
              div_ =. a </ div_ =. c =. c_dir_a_with_b
            , doNotTc [] [[[(AutoClean, [B], [C "a"], []), (JustNow, [C "c"], [], [])]]] $
              div_ =. a </ div_ =. c_dir_a_with_b =. c
            ]
          , testGroup "a is sibling"
            [ doNotTc [] [[[(AutoClean, [B, C "a"], [], []), (JustNow, [C "c"], [], [])]]] $
              div_  =. c </ div_ =. a </ div_ =. c_dir_a_with_b
            , doNotTc [] [[[(AutoClean, [B, C "a"], [], []), (JustNow, [C "c"], [], [])]]] $
              div_  =. c </ div_ =. c_dir_a_with_b </ div_ =. a
            ]
          ]
        ]
      ]
    ]
  ]
  where
    av :: MisoString = "av"
