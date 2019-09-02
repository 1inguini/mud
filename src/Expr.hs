-- 式
module Expr where

import           Data.Char
import           Data.IORef
import           Data.List       (intercalate)
import           Data.Map        as Map hiding (foldr, map, take)
import           Data.Maybe
import           Debug.Trace
import           Text.Megaparsec

import           Env
import           RecList


type Name = String
type Param = String
type Type = String
type ASTEnv = GeneralEnv AST

-- data CodeMeta = CodeMeta { codeFileName :: FilePath
--                          , codePos      :: (Int, Int) }
--               deriving (Eq, Show)

-- emptyCode :: CodeMeta
-- emptyCode = CodeMeta { codeFileName = "interactive"
--                      , codePos      = (0,0) }

data ASTMeta = ASTMeta { srcPos :: SourcePos
                       , ast    :: AST}
               deriving (Eq, Show)

type NameAST = ASTMeta

data AST
  = ASTIntLit      { astInt  :: Integer }
  | ASTStrLit      { astStr  :: String}
  | ASTDoubleLit   { astDub  :: Double }
  | ASTTypeLit     { astType :: RecList Type }
  | ASTListLit     { astList :: [ASTMeta] }
  | ASTBoolLit     { astBool :: Bool }
  | ASTVar         { astVar  :: Name }

  | ASTBinOp       { astOp   :: Op
                   , astArg0 :: ASTMeta
                   , astArg1 :: ASTMeta }

  | ASTUnary       { astOp  :: Op
                   , astArg :: ASTMeta }

  | ASTSeq         { astSeq :: [ASTMeta] }

  | ASTAssign      { astAssignName :: NameAST
                   , astAssignVar  :: ASTMeta }

  | ASTFunDef      { astType       :: RecList Type
                   , astFunDefName :: NameAST
                   , astFunParams  :: [Param]
                   , astFunBody    :: ASTMeta }

  | ASTFunDefAnon  { astType      :: RecList Type
                   , astFunParams :: [Param]
                   , astFunBody   :: ASTMeta }

  | ASTFun         { astType      :: RecList Type
                   , astFunParams :: [Param]
                   , astFunBody   :: ASTMeta
                   -- , astClosure   :: ASTEnv
                   }

  | ASTApply       { astApplyFun  :: ASTMeta
                   , astApplyArgs :: [ASTMeta] }

  | ASTCase        { astType    :: RecList Type
                   -- , astCasePattern :: [ASTMeta]
                   , astCaseBranchs ::
                       [([ASTMeta], ASTMeta, Maybe ASTMeta)] }

  | ASTIf          { astIfCond :: ASTMeta
                   , astIfThen :: ASTMeta
                   , astIfElse :: ASTMeta }

  | ASTTypeSig     { astType       :: RecList Type
                   , astTypeSigVar :: ASTMeta}

  | ASTTypeDef     { astTypeDefName   :: NameAST
                   , astTypeDefFields :: [(String, RecList Type)]}

  | ASTStructType  { astFields :: [(String, RecList Type)] }

  | ASTStructValue { astStructValue :: Map Name ASTMeta }

  | ASTCall        { astType        :: RecList Type
                   , astCallFunName :: Name }
  deriving (Eq, Show)

type NameExpr = Expr

data Expr
  = ExprIntLit      { exprInt  :: Integer }
  | ExprStrLit      { exprStr  :: String}
  | ExprDoubleLit   { exprDub  :: Double }
  | ExprTypeLit     { exprType :: RecList Type }
  | ExprListLit     { exprList :: [Expr] }
  | ExprBoolLit     { exprBool :: Bool }
  | ExprVar         { exprVar  :: Name }

  | ExprSeq         { exprSeq :: [Expr] }

  | ExprAssign      { exprAssignName :: NameExpr
                    , exprAssignVar  :: Expr }

  | ExprFunDef      { exprType       :: RecList Type
                    , exprFunDefName :: NameExpr
                    , exprFunParams  :: [Param]
                    , exprFunBody    :: Expr }

  | ExprFunDefAnon  { exprType      :: RecList Type
                    , exprFunParams :: [Param]
                    , exprFunBody   :: Expr }

  | ExprFun         { exprType      :: RecList Type
                    , exprFunParams :: [Param]
                    , exprFunBody   :: Expr
                    -- , exprClosure   :: ExprEnv
                    }

  | ExprApply       { exprApplyFun  :: Expr
                    , exprApplyArgs :: [Expr] }

  | ExprCase        { exprCaseType    :: RecList Type
                    -- , exprCasePattern :: [Expr]
                    , exprCaseBranchs :: [([Expr],Expr,Maybe Expr)] }

  | ExprTypeSig     { exprType       :: RecList Type
                    , exprTypeSigVar :: Expr}

  | ExprTypeDef     { exprTypeDefName   :: NameExpr
                    , exprTypeDefFields :: [(String, RecList Type)]}

  | ExprStructType  { exprFields :: [(String, RecList Type)] }

  | ExprStructValue { exprStructValue :: Map Name Expr }

  | ExprCall        { exprType        :: RecList Type
                    , exprCallFunName :: Name }
  deriving (Eq, Show)

data Op = -- Dot
        -- |
  OpLit String
        deriving (Eq, Show)

-- instance Show Expr where
--   show (IntLit i1) = show i1
--   show (StrLit str) = str
--   show (DoubleLit f) = show f
--   show (Neg e) = "-" ++ show e
--   show (Var name _) = name
--   show (BinOp op _ e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
--   show (Seq exprs) = concatMap ((++ ";") . show) exprs
--   show (Fun types params body env) = "function : " ++ rArrow types
--   show (FunDef (Var name _) types params body) = "(Fun (" ++ name ++ ") " ++ show body ++ ")"
--   show (FunDefAnon types params body code) = "(anon fun : " ++ rArrow types ++ ", body: " ++ show body ++ ")"
--   show (Apply e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
--   show (TypeSig sig expr) = show expr ++ " : " ++ show sig
--   show (ListLit exprs _) = "[" ++ intercalate "," (map show exprs) ++ "]"
--   show (BoolLit b) = show b
--   show (If condEx thenEx elseEx code) = "if " ++ show condEx ++ " then " ++ show thenEx ++ " else " ++ show elseEx
--   show (Case exprs matches types) = "(Case " ++ show matches ++ ")"
--   show (TypeDef (Var name _) types) = "(TypeDef " ++ name ++ " " ++ show types ++ ")"
--   show (StructType types) = "(StructType " ++ show types ++ ")"
--   show (StructValue sv) = Map.foldrWithKey f (show (fromJust $ sv !? "type")) sv
--     where f k a result = if k == "type" then result else result ++ " " ++ k ++ ":" ++ show a
--   show (Call name _) = "(Call " ++ name ++ ")"
--   show (TypeLit types) = "(TypeLit " ++ rArrow types ++ ")"

-- instance Eq AST where
--   (IntLit i1) == (IntLit i2) = i1 == i2
--   (StrLit s1) == (StrLit s2) = s1 == s2
--   (ListLit l1 c1) == (ListLit l2 c2) = l1 == l2
--   (BoolLit b1) == (BoolLit b2) = b1 == b2
--   e1 == e2 = trace (show (e1,e2)) False

-- typeOf' :: Expr -> RecList String
-- typeOf' (IntLit i) = Elem "Int"
-- typeOf' (StrLit s) = Elem "String"
-- typeOf' (BoolLit b) = Elem "Bool"
-- typeOf' (DoubleLit b) = Elem "Double"
-- typeOf' (TypeSig sig _) = sig
-- typeOf' (Fun sig _ _ _) = sig
-- typeOf' (ListLit (e:es) _) = Elems [Elem "List", typeOf' e]
-- typeOf' (ListLit [] _) = Elems [Elem "List", Elem "a"]
-- typeOf' (StructValue s) = case s !? "type" of
--   Just (StrLit str) -> Elem str
--   Nothing           -> error "type not defined in struct value"

-- -- パターンマッチの元になる式と、マッチさせる対象のリストから、変数または関数とそれに対する式のリストの組を返す
-- -- 例: a 2 [e;es] に 10 2 [1,2,3] をマッチさせる場合、(["a", "e", "es"], [IntLit 10, IntLit 1, ListLit [2,3]]) を返す
-- paramsAndArgs :: [Expr] -> [Expr] -> ([String], [Expr])
-- paramsAndArgs [] [] = ([],[])
-- paramsAndArgs (Var v _:e1s) (e:e2s) = let rests = paramsAndArgs e1s e2s
--                                   in (v : fst rests, e : snd rests)
-- paramsAndArgs (ListLit [Var h _,Var t _] c1 : e1s) (ListLit (e2:e2') c2 : e2s) =
--   let rests = paramsAndArgs e1s e2s
--   in (h : t : fst rests, e2 : ListLit e2' c2 : snd rests)
-- paramsAndArgs (e1:e1s) (e2:e2s) = paramsAndArgs e1s e2s

