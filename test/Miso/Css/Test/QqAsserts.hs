{-# LANGUAGE MultilineStrings #-}
module Miso.Css.Test.QqAsserts where

import Miso.Css (class_, CssIdentifier(id_))
import Miso.Css.Test.QqDefs (fooBar, FooBar(..), cssAsLiteralText)
import Prelude
import Test.Tasty.HUnit ( (@=?) )

unit_camelCaseLiteral :: IO ()
unit_camelCaseLiteral = ("foo-bar" :: String) @=? class_ fooBar

unit_camelCaseLiteral_Id :: IO ()
unit_camelCaseLiteral_Id = ("foo-bar" :: String) @=? id_ FooBar

unit_exportCssInputAsIs :: IO ()
unit_exportCssInputAsIs = css @=? cssAsLiteralText
  where
    css :: String
    css = """
      .foo-bar {
        color: #1212ff;
      }

      #foo-bar {
        color: #f212ff;
      }
    """
