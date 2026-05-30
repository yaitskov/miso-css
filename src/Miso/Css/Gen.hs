module Miso.Css.Gen where

import Control.Monad.State ( MonadState(put), runState, State )
import CssParser
    ( TagSubSelector(Hash, AtomicClass, HasAttr),
      TagRelation(..),
      TagSelector(tagSubSelectors, tagName),
      Ident(..),
      AttrName(attrName),
      TagName(AmpersandTag, TagName) )
import Data.Map.Strict qualified as M
import Data.Text (unpack)
import Language.Haskell.TH.Syntax
import Miso.Css.Escape ( escapeValIden )
import Miso.Css.List ( spanMaybe )
import Miso.Css.Parser ( SelIdxByLeafClass, Selectors )
import Miso.Css.Prelude
import Miso.Css.Segment
    ( MatchScope(NowOrLater, AutoClean, JustNow), SubSeg(A, T, C) )
import Miso.Css.Sibling (SiblingBranch(NilSibBranch))
import Miso.Css.Style

type TsFilter = TagSubSelector -> Bool
type TsTr = (TagSelector, TagRelation)
type TsSibRel = (TagSelector, SibRel)
type TsHierRel = (TagSelector, HierRel)

data SibRel = SibDir | SibGen deriving (Show, Eq, Ord)
data HierRel = HierChild | HierDescendant deriving (Show, Eq, Ord)

selectorsToDecs :: SelIdxByLeafClass -> Q [ Dec ]
selectorsToDecs m = concat <$> mapM go (M.toList m)
  where
    go (i, sels) =
      [d|$(pure . VarP $ identToName i) = $(selectorsToExp i sels)|]

-- type of result Exp is :: OrClass p c
selectorsToExp :: Ident -> Selectors -> Q Exp
selectorsToExp i sels =
  [| ($(composeExpsWithDot <$> mapM (selectorToExp i) sels))
        (TopOrClass $(identToSymbol i)) |]

identToName :: Ident -> Name
identToName (Ident i) = mkName . escapeValIden $ unpack i

identToSymbol :: Ident -> Q Exp
identToSymbol (Ident i) =
  [| Proxy :: Proxy $(pure . LitT . StrTyLit $ unpack i) |]

-- type of result exp  is :: OrClass p c -> OrClass p c
selectorToExp :: Ident -> [ (TagRelation, TagSelector) ] -> Q Exp
selectorToExp i s =
  case runState (shiftSelector s) Nothing of
    (tsTrs, Just (lastTs :: TagSelector)) -> do
      [| AddAncestorBranch ($(foldShiftedTsTr iFilter lastTs tsTrs)) |]
    o -> fail $ "Dead code on " <> show i <> " " <> show s <> " due: " <> show o
  where
    iFilter = \case
      AtomicClass c -> c /= i
      _ -> True

-- .a > .b > .c
tagNameToExp :: TagName -> Q [Exp]
tagNameToExp = \case
  TagName i -> fmap (:[]) [| AddSubSegConstraint (Proxy @T) $(identToSymbol i) |]
  AmpersandTag -> pure [] -- todo expand local alias
  _ -> pure []

tagSubSelectorToExp :: TagSubSelector -> Q [Exp]
tagSubSelectorToExp = \case
  AtomicClass c ->
    fmap (:[]) [| AddSubSegConstraint (Proxy @C) $(identToSymbol c) |]
  HasAttr an ->
    fmap (:[]) [| AddSubSegConstraint (Proxy @A) $(identToSymbol an.attrName) |]
  Hash i ->
    fmap (:[]) [| AddSubSegConstraint (Proxy @A) $(identToSymbol i) |]
  _ -> pure []

-- every Exp represents a function :: AncestorConstraint -> AncestorConstraint
tagSelectorToExp :: (TagSubSelector -> Bool) -> TagSelector -> Q Exp
tagSelectorToExp tsFilter ts = do
  tn <- tagNameToExp ts.tagName
  composeExpsWithArr . (tn <>) . concat <$>
    mapM tagSubSelectorToExp (filter tsFilter ts.tagSubSelectors)

jn :: Proxy JustNow
jn = Proxy @JustNow
nol :: Proxy NowOrLater
nol = Proxy @NowOrLater
acn :: Proxy AutoClean
acn = Proxy @AutoClean

passAll :: b -> Bool
passAll = const True

lastSelectorToExp :: TsFilter -> TagSelector -> Q Exp
lastSelectorToExp tsf lastTs = [| NextAncestor acn >>> $(tagSelectorToExp tsf lastTs) |]

-- returns exp that is :: AncestorConstraint p
foldShiftedTsTr :: TsFilter -> TagSelector -> [ TsTr ] -> Q Exp
foldShiftedTsTr tsFilter lastTs tsTrs =
  case spanSiblings tsTrs of
    ([], []) -> [| (CssOrphan acn & $(tagSelectorToExp tsFilter lastTs)) |]
    ([], [(ts, Child)]) ->
      [| (CssOrphan jn & ($(tagSelectorToExp passAll ts) >>> $(lastSelectorToExp tsFilter lastTs))) |]
    ([], [(ts, Descendant)]) ->
      [| (CssOrphan nol & ($(tagSelectorToExp passAll ts) >>> $(lastSelectorToExp tsFilter lastTs))) |]

    ([], (ts, Child) : tsTrs') ->
      [| (CssOrphan jn & ($(go (tagSelectorToExp passAll ts) tsTrs'))) |]
    ([], (ts, Descendant) : tsTrs') ->
      [| (CssOrphan nol & ($(go (tagSelectorToExp passAll ts) tsTrs'))) |]

    (tsSrs, []) ->
      [| (CssOrphan jn & ($(tsTrsToAddSiblingBranchExp tsSrs) >>> $(lastSelectorToExp tsFilter lastTs))) |]

    (tsSrs, [(ts, Child)]) ->
      [| (CssOrphan jn & (   $(tsTrsToAddSiblingBranchExp tsSrs)
                         >>> NextAncestor jn
                         >>> $(tagSelectorToExp passAll ts))) |]
    (tsSrs, [(ts, Descendant)]) ->
      [| (CssOrphan nol & (   $(tsTrsToAddSiblingBranchExp tsSrs)
                          >>> NextAncestor jn
                          >>> $(tagSelectorToExp passAll ts))) |]

    (tsSrs, (ts, Child) : tsTrs') ->
      [| (CssOrphan jn & (   $(tsTrsToAddSiblingBranchExp tsSrs)
                         >>> NextAncestor jn
                         >>> $(go (tagSelectorToExp passAll ts) tsTrs'))) |]
    (tsSrs, (ts, Descendant) : tsTrs') ->
      [| (CssOrphan nol & (   $(tsTrsToAddSiblingBranchExp tsSrs)
                          >>> NextAncestor jn
                          >>> $(go (tagSelectorToExp passAll ts) tsTrs'))) |]

    o -> fail $ "Unexpected tsTrs" <> show tsTrs <>  " due case: " <> show o
  where
    go b tsTrs' =
      case spanSiblings tsTrs' of
        ([], []) -> [| $b >>> $(lastSelectorToExp tsFilter lastTs) |]
        ([], [(ts, Child)]) ->
          [| $b >>> NextAncestor jn
                >>> $(tagSelectorToExp passAll ts)
                >>> $(lastSelectorToExp tsFilter lastTs) |]
        ([], [(ts, Descendant)]) ->
          [| $b >>> NextAncestor nol
                >>> $(tagSelectorToExp passAll ts)
                >>> $(lastSelectorToExp tsFilter lastTs) |]
        ([], (ts, Child) : tsTrs'') ->
          go [| $b >>> NextAncestor jn >>> $(tagSelectorToExp passAll ts) |] tsTrs''
        ([], (ts, Descendant) : tsTrs'') ->
          go [| $b >>> NextAncestor nol >>> $(tagSelectorToExp passAll ts) |] tsTrs''
        (tsSrs, []) ->
          [| $b >>> $(tsTrsToAddSiblingBranchExp tsSrs) >>> $(lastSelectorToExp tsFilter lastTs) |]
        (tsSrs, tsTrs'') ->
          go [| $b >>> $(tsTrsToAddSiblingBranchExp tsSrs) |] tsTrs''

sibRelToNilSibExp :: SibRel -> Q Exp
sibRelToNilSibExp = \case
  SibDir -> [| NilSib (Proxy @NowOrLater) |]
  SibGen -> [| NilSib (Proxy @JustNow) |]

-- @AddSiblingBranch (... >>> ... $ NilSibBranch)@
tsTrsToAddSiblingBranchExp :: [TsSibRel] -> Q Exp
tsTrsToAddSiblingBranchExp tsSrs =
  [| AddSiblingBranch (($(go)) NilSibBranch) |]
  where
    go = composeExpsWithArr <$> mapM tsTrToAddSegToSibBrExp tsSrs

-- @AddSegToSibBranch (AddSib (Proxy @C) pa $ NilSib nol)@
tsTrToAddSegToSibBrExp :: TsSibRel -> Q Exp
tsTrToAddSegToSibBrExp (ts, sr) =
  [| AddSegToSibBranch (($(tagSelectorToSibBrSeg ts)) $(sibRelToNilSibExp sr)) |]

-- Exp is :: Sibling ms ss -> Sibling ms ss
-- @AddSib (Proxy @C) pa@
tagSelectorToSibBrSeg :: TagSelector -> Q Exp
tagSelectorToSibBrSeg ts = composeExpsWithDot <$> go
  where
    go :: Q [Exp]
    go = (:) <$> tagNameToAddSibExp ts.tagName
             <*> mapM tagSubSelectorToAddSibExp ts.tagSubSelectors

dotOp :: (b -> c) -> (a -> b) -> a -> c
dotOp = (.)

-- compose the list of unary functions Exp
composeExpsWithDot :: [Exp] -> Exp
composeExpsWithDot = foldr go (VarE 'id)
  where
    go aE = UInfixE aE (VarE 'dotOp)

composeExpsWithArr :: [Exp] -> Exp
composeExpsWithArr = foldl' go (VarE 'id)
  where
    go bE = UInfixE bE (VarE 'arr)

arr :: (a -> b) -> (b -> c) -> a -> c
arr = (>>>)

tagNameToAddSibExp :: TagName -> Q Exp
tagNameToAddSibExp = \case
  TagName i ->
    [| AddSib (Proxy @T) $(identToSymbol i) |]
  AmpersandTag -> [| id |] -- todo expand local alias
  _ -> [| id |]

tagSubSelectorToAddSibExp :: TagSubSelector -> Q Exp
tagSubSelectorToAddSibExp = \case
  AtomicClass c ->
    [| AddSib (Proxy @C) $(identToSymbol c) |]
  HasAttr an ->
    [| AddSib (Proxy @A) $(identToSymbol an.attrName) |]
  Hash i ->
    [| AddSib (Proxy @A) $(identToSymbol i) |]
  _ -> [| id |]

-- every Exp represents a function :: AncestorConstraint -> AncestorConstraint
tagRelationToExp :: TagRelation -> Q [Exp]
tagRelationToExp = \case
  Descendant -> (:[]) <$> [| NextAncestor (Proxy @NowOrLater) |]
  Child -> (:[]) <$> [| NextAncestor (Proxy @JustNow) |]
  NextSibling -> (:[]) <$> [| NextAncestor (Proxy @JustNow) |]
  GeneralSibling -> (:[]) <$> [| NextAncestor (Proxy @NowOrLater) |]

isSiblingRel :: TagRelation -> Maybe SibRel
isSiblingRel = \case
  NextSibling -> pure SibDir
  GeneralSibling -> pure SibGen
  _ -> Nothing

isHierRel :: TagRelation -> Maybe HierRel
isHierRel = \case
  Child -> pure HierChild
  Descendant -> pure HierDescendant
  _ -> Nothing

spanSiblings :: [TsTr] -> ([TsSibRel], [TsTr])
spanSiblings = spanMaybe (\(ts, tr) -> isSiblingRel tr <&> (ts,))

spanHierarchy :: [TsTr] -> ([TsHierRel], [TsTr])
spanHierarchy = spanMaybe (\(ts, tr) -> isHierRel tr <&> (ts,))

shiftSelector :: [(TagRelation, TagSelector)] -> State (Maybe TagSelector) [TsTr]
shiftSelector = \case
  [] -> pure []
  [(_, s)] -> put (Just s) >> pure []
  (_, s) : t -> go s t
  where
    go s = \case
      [] -> pure []
      [(r, lastS)] -> put (Just lastS) >> pure [(s, r)]
      (r', s') : t -> ((s, r') :) <$> go s' t
