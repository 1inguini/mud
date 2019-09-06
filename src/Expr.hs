{-# LANGUAGE OverloadedStrings #-}
-- 式
module Expr where

-- import           Data.Char
-- import           Data.IORef
-- import           Data.List       (intercalate)
import           Data.Map        as Map hiding (foldr, map, take)
import           Data.Maybe
import           Data.Text
import           Debug.Trace
import qualified Text.Megaparsec as MP

-- import           Env
import           RecList

type Name = Text
type Param = Text
type Type = Text
type ASTEnv = GeneralEnv ASTMeta
type Env = GeneralEnv ExprMeta

type GeneralEnv a = -- IORef
  Map Text [(RecList Type, a)]

-- data CodeMeta = CodeMeta { codeFileName :: FilePath
--                          , codePos      :: (Int, Int) }
--               deriving (Eq, Show)

-- emptyCode :: CodeMeta
-- emptyCode = CodeMeta { codeFileName = "interactive"
--                      , codePos      = (0,0) }

data ASTMeta = ASTMeta { astSrcPos :: MP.SourcePos
                       , ast       :: AST}
               deriving (Eq)

instance Show ASTMeta where
  show ASTMeta { ast = ast } = show ast

data ExprMeta = ExprMeta { exprSrcPos :: MP.SourcePos
                         , expr       :: Expr}
               deriving (Eq)
type NameAST = ASTMeta

instance Show ExprMeta where
  show ExprMeta { expr = expr } = show expr

type Types = RecList Type

data AST
  = ASTInt    { astInt  :: Integer }
  | ASTDouble { astDub  :: Double }
  | ASTStr    { astStr  :: Text}
  | ASTList   { astList :: [ASTMeta] }
  | ASTBool   { astBool :: Bool }

  | ASTVar    { astVar  :: Name }
  -- | ASTType   { astType :: Types }

  | ASTBinOp       { astOp   :: Op
                   , astArg0 :: ASTMeta
                   , astArg1 :: ASTMeta }

  | ASTUnary       { astOp  :: Op
                   , astArg :: ASTMeta }

  | ASTSeq         { astSeq :: [ASTMeta] }

  | ASTAssign      { astAssignName :: NameAST
                   , astAssignVar  :: ASTMeta }

  | ASTFunDef      { astType       :: Types
                   , astFunDefName :: NameAST
                   , astFunParams  :: [Param]
                   , astFunBody    :: ASTMeta }

  | ASTApply       { astApplyFun  :: ASTMeta
                   , astApplyArgs :: [ASTMeta] }

  | ASTAnonFun     { astType    :: Types
                   -- , astCasePattern :: [ASTMeta]
                   , astCaseBranchs ::
                       [( [ASTMeta]        -- Pattern
                        , ASTMeta          -- expr to exec when matched
                        , Maybe ASTMeta )] -- guard
                   }

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

type NameExpr = ExprMeta

data Expr
  = ExprInt    { exprInt  :: Integer }
  | ExprStr    { exprStr  :: Text}
  | ExprDouble { exprDub  :: Double }
  | ExprType   { exprType :: Types }
  | ExprList   { exprList :: [ExprMeta] }
  | ExprBool   { exprBool :: Bool }
  | ExprVar    { exprType :: Types
               , exprVar  :: Name }

  | ExprSeq         { exprSeq :: [ExprMeta] }

  | ExprAssign      { exprAssignName :: NameExpr
                    , exprAssignVar  :: ExprMeta }

  | ExprFun         { exprType      :: Types
                    , exprFunParams :: [Param]
                    , exprFunBody   :: ExprMeta
                    -- , exprClosure   :: ExprEnv
                    }

  | ExprApply       { exprApplyFun  :: ExprMeta
                    , exprApplyArgs :: [ExprMeta] }

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

newtype Op = OpLit Text
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
