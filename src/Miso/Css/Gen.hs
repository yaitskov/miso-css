module Miso.Css.Gen where

import Language.Haskell.TH.Syntax
import Miso.Css.Segment
import Miso.Css.Prelude
import Miso.Css.Parser
import CssParser
import Miso.Css.Style
import Data.Proxy
import Data.Text (unpack)

selectorsToDecs :: SelIdxByLeafClass -> Q [ Dec ]
selectorsToDecs _ = pure []


identToName :: Ident -> Name
identToName (Ident i) = mkName $ unpack i
tagRelToMs :: TagRelation -> Maybe MatchScope
tagRelToMs = \case
  Child -> pure JustNow
  Descendant -> pure NowOrLater
  NextSibling -> Nothing
  GeneralSibling -> Nothing

-- data MatchScope = NowOrLater | JustNow deriving (Show, Eq)
-- data SubSeg = C Symbol | T Symbol | I Symbol
-- type Seg =
--   ( MatchScope
--   , [SubSeg] -- unmatched
--   , [SubSeg] -- matched
--   , [[(MatchScope, [SubSeg])]] -- siblings
--   )
-- CssOrphan :: Proxy ms -> AncestorConstraint '[ '( ms, '[], '[], '[] )]
-- selListToAncestorConstraint :: Exp -> [ (TagRelation, TagSelector) ] ->  Q Exp
-- selListToAncestorConstraint base  l = _
-- a_dir_b_dir_c :: OrClass
--   '[ [ '(AutoClean, '[], '[], '[])
--      , '(JustNow, '[C "b"], '[], '[])
--      , '(JustNow, '[C "a"], '[], '[])
--      ]
--    ] "c"
-- a_dir_b_sp_c =
--   AddAncestorBranch
--   (NextAncestor acn . AddAncestor pb . NextAncestor nol . AddAncestor pa $ CssOrphan jn)
--   c

-- _ .a > .b _ .c  -- normal
-- drop first tag relation
-- ">"  -> CssOrphan jn
-- ".a" -> AddAncestor pa
-- "_"  -> NexnAncestor
-- ".b" -> AddAncestor pb
-- last TagSelector is special case and it is processed nonrecursively

--                    > .a _ .b acn .c

-- Now it is clear how to produce a selector composed of just Descendant and Child relation,
-- so next step is to cover cases involving siblings:
--
--      _ .c > .a + .b
--      > .c + .a acn .b
--
-- (NextAncestor acn $ AddSiblingBranch
--   (AddSegToSibBranch (AddClassToSib pa $ NilSib jn) NilSibBranch)
--   (CssOrphan nol))
-- .a > .b + .c

--
-- special handling for first pair -> AddAncestorBranch
--
selectorToExp :: Ident -> [ (TagRelation, TagSelector) ] -> Q Exp
selectorToExp i _s = do
  base <- topOpClass i

  pure base
  -- where
  --   go =
-- .a > .b > .c

-- :: OrClass [] "span"
topOpClass :: Ident -> Q Exp
topOpClass i = do
  let
    n = identToName i
    t = [t| Proxy $(pure $ ConT n) |]
   in
     [e| TopOrClass (Proxy :: $t) |]
