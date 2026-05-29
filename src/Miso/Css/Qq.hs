-- | Module provides a quasi quoter translating CSS classes to Haskell functions
module Miso.Css.Qq where

import CssParser as CP ( parseCssP, P(Failed, Ok) )
import Data.String ( IsString )
import Miso.Css.Gen ( selectorsToDecs )
import Miso.Css.Parser ( indexFile )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax
import Prelude

{- | quasi quoter accepts CSS and generates definitions for CSS classes

> .foo > .bar {
>   padding: 0px;
> }

is expanded as:

@
{-# INLINE fooBar #-}

foo = TopOrClass (Proxy @"foo")

bar =
  AddAncestorBranch
    (CssOrphan jn & ( AddSubSegConstraint (Proxy @C) (Proxy @"foo")))
    (TopOrClass (Proxy @"bar"))

{-# INLINE cssAsLiteralText #-}
cssAsLiteralText :: IsString s => s
cssAsLiteralText = ".foo > .bar { padding: 0px; }"
@

-}
css :: QuasiQuoter
css = QuasiQuoter
  { quoteExp  = \_ -> fail "quoteExp: not implemented"
  , quotePat  = \_ -> fail "quotePat: not implemented"
  , quoteType = \_ -> fail "quoteType: not implemented"
  , quoteDec  = cssToDecs Nothing n
  }
  where
    n = mkName "cssAsLiteralText"

cssToDecs :: Maybe FilePath -> Name -> String -> Q [Dec]
cssToDecs fileNameMb inputExportName s =
  case parseCssP s of
    Ok cssFile ->
      (cssAsLiteralTextD inputExportName s <>) <$> selectorsToDecs (indexFile cssFile)
    Failed cssErr ->
      case fileNameMb of
        Nothing -> fail $ "Failed to parse QuasiQuoted CSS due: " <> cssErr
        Just fn -> fail $ "Failed to parse CSS from " <> fn <> " due: " <> cssErr

{- | generate definition like:
@@
  {-# INLINE cssAsLiteralText #-}
  cssAsLiteralText :: IsString s => s
  cssAsLiteralText = s
@@
-}
cssAsLiteralTextD :: Name -> String -> [Dec]
cssAsLiteralTextD n s =
  [ SigD n
    (ForallT
      [PlainTV st InferredSpec]
      [AppT (ConT ''IsString) (VarT st)]
      (VarT st))
  , FunD n [ Clause [] body [] ]
  , PragmaD (InlineP n Inline FunLike AllPhases)
  ]
  where
    st = mkName "s"
    body = NormalB (LitE (StringL s))
