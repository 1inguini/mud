{-# LANGUAGE OverloadedStrings #-}
-- 脱糖
module Desugar where

import           Control.Arrow                  (arr, first, second, (&&&),
                                                 (***), (>>>))
import qualified Control.Monad.Combinators.Expr as Comb
import           Data.List                      (partition)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map (fromList, toDescList)
import           Data.Text                      (Text)
import           Data.Void
import           Text.Megaparsec                hiding (sepBy, sepBy1)


import           Expr

-- a + (b c) * (d e f) + g + h + i == j k == l == m
-- (((a + (b c)) *) (((((((d e f) +) g) +) h) +) i) ==) (((j k) ==) ((l ==) m))

-- (a == c)
-- ((a ++) ++) ++


type Parser = Parsec Void OpASTList


type OpTable = [[Comb.Operator Parser ASTMeta]]


defOpMaps :: OpMaps
defOpMaps = Map.fromList <$>
  [ tup False <$> ["."]
  , tup False <$> ["*", "/"]
  , tup False <$> ["+", "-"]
  , tup False <$> [ "<=", ">=", "<", ">" ]
  , tup True <$> ["=="]
  , tup False <$> ["&&"]
  , tup False <$> ["||"]
  ]
  where tup = flip (,)


opMaps2OpTables :: OpMaps -> OpTable
opMaps2OpTables opMaps = (tup2ComBOp `concatMap`) <$> (Map.toDescList <$> opMaps)
  where
    tup2ComBOp :: (Text, Bool) -> [Comb.Operator Parser ASTMeta]
    tup2ComBOp (opTxt, isRightAssoc) =
      [ -- Comb.Postfix (genPostfix4OpTable opTxt)
      -- ,
        (if isRightAssoc then Comb.InfixR else Comb.InfixL)
        $ genInfix4OpTable opTxt]


genPostfix4OpTable :: Text -> Parser (ASTMeta -> ASTMeta)
genPostfix4OpTable txt = do
  _ <- single $ Left $ OpLit txt
  pure $ \astMeta@ASTMeta { astSrcPos = pos } ->
           ASTMeta { astSrcPos = pos
                   , ast       = ASTPostfix
                                 { astArg = astMeta
                                 , astOp  = OpLit txt } }


genInfix4OpTable :: Text -> Parser (ASTMeta -> ASTMeta -> ASTMeta)
genInfix4OpTable txt = do
  _ <- single $ Left $ OpLit txt
  pure $ \arg0@ASTMeta { astSrcPos = pos0 } arg1 ->
           ASTMeta { astSrcPos = pos0
                   , ast       = ASTApply
                     { astApplyFun = ASTMeta
                                     { astSrcPos = pos0
                                     , ast       = ASTPostfix
                                                   { astArg = arg0
                                                   , astOp  = OpLit txt } }
                     , astApplyArg = arg1 } }

desugar' ASTSeq { astSeq = asts } =
  undefined
  where
    funDefs = filter isOpDef asts
    isOpDef ASTMeta { ast = ASTOpDef {} } = True
    isOpDef _                             = False


desugar :: ASTMeta -> ExprMeta
desugar ASTMeta { astSrcPos = pos, ast = metaAst } = desugar' metaAst
  where
    meta expr = ExprMeta { exprSrcPos = pos, expr = expr }


    desugar' ASTInt { astInt = int } = meta
      ExprInt { exprInt = int }

    desugar' ASTDouble { astDub = dub } = meta
      ExprDouble { exprDub = dub }

    desugar' ASTStr { astStr = str } = meta
      ExprStr { exprStr = str }

    desugar' ASTBool { astBool = bool } = meta
      ExprBool { exprBool = bool }

    desugar' ASTVar { astVar = name } = meta
      ExprVar { exprVar = name }

    desugar' ASTList { astList = ls } = meta
      ExprList { exprList = desugar <$> ls }

    desugar' ASTPrefix { astOp = OpLit op, astArg = astMeta } = meta
      ExprApply { exprApplyFun = meta ExprVar { exprVar = "prefix\"" <> op <> "\"" }
                , exprApplyArg = desugar astMeta }

    desugar' ASTPostfix { astArg = astMeta, astOp = OpLit op } = meta
      ExprApply { exprApplyFun = meta ExprVar { exprVar = "postfix\"" <> op <> "\"" }
                , exprApplyArg = desugar astMeta }

    desugar' ASTSeq { astSeq = asts } = meta
      ExprSeq { exprSeqFunDefs  = desugar <$> funDefs
              , exprSeqTypeDefs = desugar <$> typeDefs
              , exprSeqAnonFuns = desugar <$> anonFuns
              , exprSeqExprs = desugar <$> exprs }
      where
        (funDefs, (typeDefs, (anonFuns, exprs))) =
          (second . second) (partition isAnonFun)
          $ second (partition isTypeDef)
          $ partition isFunDef asts
        isFunDef ASTMeta { ast = ASTFunDef {} } = True
        isFunDef _                              = False
        isTypeDef ASTMeta { ast = ASTTypeDef {} } = True
        isTypeDef _                               = False
        isAnonFun ASTMeta { ast = ASTAnonFun {} } = True
        isAnonFun _                               = False

    -- desugar' ASTAssign { astAssignName = name, astAssignVar = var } = meta
    --   ExprAssign { exprAssignName = name, exprAssignVar = var }
