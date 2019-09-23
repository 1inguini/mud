{-# LANGUAGE OverloadedStrings #-}
-- 脱糖
module Desugar where

import           Control.Arrow (arr, first, second, (&&&), (***), (>>>))
import           Data.List     (partition)
import           Data.Void
import           Expr

-- a + (b c) * (d e f) + g + h + i
-- ((a + (b c)) *) (((((((d e f) +) g) +) h) +) i)


-- desugar :: ASTMeta -> ExprMeta
-- desugar ASTMeta { astSrcPos = pos, ast = metaAst } = desugar' metaAst
--   where
--     meta expr = ExprMeta { exprSrcPos = pos, expr = expr }


--     desugar' ASTInt { astInt = int } = meta
--       ExprInt { exprInt = int }

--     desugar' ASTDouble { astDub = dub } = meta
--       ExprDouble { exprDub = dub }

--     desugar' ASTStr { astStr = str } = meta
--       ExprStr { exprStr = str }

--     desugar' ASTBool { astBool = bool } = meta
--       ExprBool { exprBool = bool }

--     desugar' ASTVar { astVar = name } = meta
--       ExprVar { exprVar = name }

--     desugar' ASTList { astList = ls } = meta
--       ExprList { exprList = desugar <$> ls }

--     desugar' ASTPrefix { astOp = OpLit op, astArg = astMeta } = meta
--       ExprApply { exprApplyFun = meta ExprVar { exprVar = "prefix\"" <> op <> "\"" }
--                 , exprApplyArg = desugar astMeta }

--     desugar' ASTPostfix { astArg = astMeta, astOp = OpLit op } = meta
--       ExprApply { exprApplyFun = meta ExprVar { exprVar = "postfix\"" <> op <> "\"" }
--                 , exprApplyArg = desugar astMeta }

--     desugar' ASTSeq { astSeq = asts } = meta
--       ExprSeq { exprSeqFunDefs  = desugar <$> funDefs
--               , exprSeqTypeDefs = desugar <$> typeDefs
--               , exprSeqAnonFuns = desugar <$> anonFuns
--               , exprSeqExprs    = desugar <$> exprs }
--       where
--         (funDefs, (typeDefs, (anonFuns, exprs))) =
--           (second . second) (partition isAnonFun)
--           $ second (partition isTypeDef)
--           $ partition isFunDef asts
--         -- (typeDefs, notType) = partition isTypeDef notFun
--         -- (anonFuns, exprs)   = partition isAnonFun notType
--         isFunDef ASTMeta { ast = ASTFunDef {} } = True
--         isFunDef _                              = False
--         isTypeDef ASTMeta { ast = ASTTypeDef {} } = True
--         isTypeDef _                               = False
--         isAnonFun ASTMeta { ast = ASTAnonFun {} } = True
--         isAnonFun _                               = False

--     -- desugar' ASTAssign { astAssignName = name, astAssignVar = var } = meta
--     --   ExprAssign { exprAssignName = name, exprAssignVar = var }

