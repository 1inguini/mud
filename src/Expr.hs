{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- 式
module Expr where

import           Prelude            as P

-- import           Data.Char
-- import           Data.IORef
-- import           Data.List       (intercalate)
import qualified Data.List.NonEmpty as NE
import           Data.Map           as Map (Map, (!?))
import           Data.Maybe
import           Data.Proxy
import           Data.Text          (Text)
import           Debug.Trace
import           Text.Megaparsec    (PosState (..), SourcePos, Stream (..),
                                     Token, Tokens, option, try)

-- import           Env
import           RecList


sepBy p sep = option [] $ sepBy1 p sep

sepBy1 p sep =
  (:[]) <$> p >>= sepBy'
  where
    sepBy' accm = option accm $ try $ do
      p' <- sep *> p
      sepBy' $ accm <> [p']

type Name = Text
type Param = Text
type Type = Text
type ASTEnv = GeneralEnv ASTMeta
type Env = GeneralEnv ExprMeta

type GeneralEnv a = -- IORef
  Map Text [(RecList Type, a)]

type OpMaps = [Map Text Bool]

-- data CodeMeta = CodeMeta { codeFileName :: FilePath
--                          , codePos      :: (Int, Int) }
--               deriving (Eq, Show)

-- emptyCode :: CodeMeta
-- emptyCode = CodeMeta { codeFileName = "interactive"
--                      , codePos      = (0,0) }

data ASTMeta = ASTMeta { astSrcPos :: SourcePos
                       , ast       :: AST}
             deriving (Eq)

instance Ord ASTMeta where
  compare _ _ = EQ

instance Show ASTMeta where
  show ASTMeta { ast = ast } = show ast

data ExprMeta = ExprMeta { exprSrcPos :: SourcePos
                         , expr       :: Expr}
               deriving (Eq)
type NameAST = ASTMeta

instance Show ExprMeta where
  show ExprMeta { expr = expr } = show expr

type Types = RecList Type


data OpPrecedence = StrongerThan Op
                  | EqualTo      Op
                  | WeakerThan   Op
                  deriving (Show, Ord, Eq)

newtype Op = OpLit Text deriving (Eq, Ord, Show)

newtype OpLaw = OpLaw (Maybe InfixOpLaw)
              deriving (Eq, Ord, Show)

data InfixOpLaw = InfixOpLaw
                  { isRightAssoc :: Bool
                  , precedence   :: OpPrecedence
                  } deriving (Eq, Ord, Show)


data AST
  = ASTComment Text
  | ASTInt    { astInt  :: Integer }
  | ASTDouble { astDub  :: Double }
  | ASTStr    { astStr  :: Text}
  | ASTList   { astList :: [ASTMeta] }
  | ASTBool   { astBool :: Bool }

  | ASTVar    { astVar  :: Name }
  -- | ASTType   { astType :: Types }

  -- | ASTBinOp       { astOp   :: Op
  --                  , astArg0 :: ASTMeta
  --                  , astArg1 :: ASTMeta }

  | ASTPrefix      { astOp  :: Op
                   , astArg :: ASTMeta }

  | ASTPostfix     { astArg :: ASTMeta
                   , astOp  :: Op }

  | ASTSeq         { astSeq    :: [ASTMeta] }

  | ASTExpr        { astExpr :: [(SourcePos, Either Op AST)] }

  | ASTAssign      { astAssignName :: NameAST
                   , astAssignVar  :: ASTMeta }

  | ASTFunDef      { astType       :: Types
                   , astFunDefName :: Name
                   , astFunParams  :: [Param]
                   , astFunBody    :: ASTMeta }

  | ASTOpDef       { astType      :: Types
                   , astOpDefName :: Name
                   , astOpAssoc   :: Maybe OpLaw
                   , astOpParams  :: [Param]
                   , astOpBody    :: ASTMeta }

  | ASTApply       { astApplyFun :: ASTMeta
                   , astApplyArg :: ASTMeta }

  | ASTAnonFun     { astPattern :: [ASTMeta]
                   , astGuard   :: Maybe ASTMeta
                   , astBody    :: ASTMeta }

  | ASTIf          { astIfCond :: ASTMeta
                   , astIfThen :: ASTMeta
                   , astIfElse :: ASTMeta }

  | ASTTypeSig     { astType       :: Types
                   , astTypeSigVar :: ASTMeta}

  | ASTTypeDef     { astTypeDefName   :: NameAST
                   , astTypeDefFields :: [(Text, Types)]}

  | ASTStructType  { astFields :: [(Text, Types)] }

  | ASTStructValue { astStructValue :: Map Name ASTMeta }

  -- | ASTCall        { astType        :: Types
  --                  , astCallFunName :: Name }
  deriving (Eq, Show)


instance Ord AST where
  compare _ _ = EQ

newtype OpASTList = OpASTList { unOpASTList :: [(SourcePos, Either Op AST)] }
                  deriving (Show, Eq)

instance Stream OpASTList where
  type Token  OpASTList = (Either Op AST)
  type Tokens OpASTList = [Either Op AST]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  take1_ (OpASTList [])         = Nothing
  take1_ (OpASTList ((_,t):ts)) = Just (t, OpASTList ts)
  takeN_ n (OpASTList s)
    | n <= 0    = Just ([], OpASTList s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (snd <$> x, OpASTList s')
  takeWhile_ f (OpASTList s) =
    let (x, s') = span (\(_,a) -> f a) s
    in (snd <$> x, OpASTList s')
  showTokens Proxy = show
  reachOffset offset pst@PosState { pstateOffset    = defOffset
                                  , pstateSourcePos = defSrcPos
                                  , pstateInput     = inputs } =
    case drop (offset - defOffset) (unOpASTList inputs) of
      [] ->
        ( defSrcPos
        , "<missing input>"
        , pst { pstateInput = OpASTList [] }
        )
      (x@(pos, _):xs) ->
        ( pos
        , "<missing input>"
        , pst { pstateInput = OpASTList (x:xs) }
        )

type NameExpr = ExprMeta

data Expr
  = ExprInt    { exprInt  :: Integer }
  | ExprStr    { exprStr  :: Text}
  | ExprDouble { exprDub  :: Double }
  | ExprType   { exprType :: Types }
  | ExprList   { exprList :: [ExprMeta] }
  | ExprBool   { exprBool :: Bool }
  | ExprVar    { exprVar  :: Name }

  | ExprSeq         { exprSeqFunDefs  :: [ExprMeta]
                    , exprSeqTypeDefs :: [ExprMeta]
                    , exprSeqAnonFuns :: [ExprMeta]
                    , exprSeqExprs    :: [ExprMeta] }

  | ExprAssign      { exprAssignName :: NameExpr
                    , exprAssignVar  :: ExprMeta }

  | ExprFun         { exprType      :: Types
                    , exprFunParams :: [Param]
                    , exprFunBody   :: ExprMeta
                    -- , exprClosure   :: ExprEnv
                    }

  | ExprApply       { exprApplyFun :: ExprMeta
                    , exprApplyArg :: ExprMeta }

  | ExprAnonFun     { exprCaseType    :: Types
                    -- , exprCasePattern :: [ExprMeta]
                    , exprCaseBranchs :: [([ExprMeta],ExprMeta,Maybe ExprMeta)] }

  | ExprTypeSig     { exprType       :: Types
                    , exprTypeSigVar :: ExprMeta}

  | ExprTypeDef     { exprTypeDefName   :: NameExpr
                    , exprTypeDefFields :: [(Text, Types)]}

  | ExprStructType  { exprFields :: [(Text, Types)] }

  | ExprStructValue { exprStructValue :: Map Name ExprMeta }

  -- | ExprCall        { exprType        :: Types
  --                   , exprCallFunName :: Name }
  deriving (Eq, Show)

-- instance Eq AST where
--   (ASTIntLit i1) == (ASTIntLit i2) = i1 == i2
--   (ASTStrLit s1) == (ASTStrLit s2) = s1 == s2
--   (ASTListLit l1) == (ASTListLit l2) = l1 == l2
--   (ASTBoolLit b1) == (ASTBoolLit b2) = b1 == b2
--   e1 == e2 = trace (show (e1,e2)) False

typeOf' :: Expr -> Maybe Types
typeOf' ExprInt    {} = pure $ Elem "Int"
typeOf' ExprStr    {} = pure $ Elem "Text"
typeOf' ExprBool   {} = pure $ Elem "Bool"
typeOf' ExprDouble {} = pure $ Elem "Double"
-- typeOf' ExprTypeSig { exprType = t } = pure t
-- typeOf' ExprFun { exprType = t } = pure t

typeOf' ExprList { exprList = e:_ } = do
  e' <- typeOf' $ expr e
  pure $ Elems [Elem "List", e']

typeOf' ExprList { exprList = [] }  =
  pure $ Elems [Elem "List", Elem "a"]

typeOf' ExprStructValue { exprStructValue = s } =
  case s !? "type" of
    Just ExprMeta { expr = ExprStr str } -> pure $ Elem str
    _                                    -> Nothing

typeOf' expr = pure $ exprType expr

-- -- パターンマッチの元になる式と、マッチさせる対象のリストから、変数または関数とそれに対する式のリストの組を返す
-- -- 例: a 2 [e;es] に 10 2 [1,2,3] をマッチさせる場合、(["a", "e", "es"], [IntLit 10, IntLit 1, ListLit [2,3]]) を返す
-- paramsAndArgs :: [Expr] -> [Expr] -> Maybe ([Text], [Expr])
-- paramsAndArgs [] [] = pure ([],[])
-- paramsAndArgs (ExprVar { exprVar = v }:e1s) (e:e2s) = do
--   rests <- paramsAndArgs e1s e2s
--   pure (v : fst rests, e : snd rests)
-- paramsAndArgs
--   (ExprListLit { exprList = [ExprVar h, ExprVar t] } : e1s)
--   (ExprListLit (e2:e2'): e2s) = do
--   rests <- paramsAndArgs e1s e2s
--   pure (h:t: fst rests, e2: ExprListLit e2' : snd rests)
-- paramsAndArgs (_:e1s) (_:e2s) = paramsAndArgs e1s e2s
-- paramsAndArgs _ _ = Nothing
