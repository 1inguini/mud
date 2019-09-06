{-# LANGUAGE OverloadedStrings #-}
-- 字句解析と構文解析
module Parse where

import           Control.Arrow                  ((&&&), (***), (>>>))
import           Control.Monad                  (join, void)
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Maybe
import           Data.Text                      (Text, cons, pack)
import           Data.Void
import           Debug.Trace
import           Safe
import           Text.Megaparsec
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug

import           Expr
import           RecList

type Parser = Parsec Void Text

-- space consumer。空白やコメントをスキップする。改行はスキップしない。
spaceConsumer :: Parser ()
spaceConsumer = L.space spaceOrTab1 lineCmnt blockCmnt
  where
    spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> isSpace c && c /= '\n')
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- 整数を読む
integer :: Parser Integer
integer = lexeme L.decimal

-- 浮動小数点数を読む
double :: Parser Double
double = lexeme L.float

-- 演算子を読む
operator :: Parser Text
operator = lexeme $ takeWhile1P (Just "symbol") (`elem` opChars)


opChars, endChars, sepChars :: [Char]
opChars = "+-*/><"
endChars = "!?_'"
sepChars = "\n;"


-- 予約語のリスト
reservedWords :: [Text] -- list of reserved words
reservedWords = ["fun","if","then","else","type"]


-- 識別子を読む
identifier :: Parser Text
identifier = satisfy isLower >>= identifier'


-- 識別子を読む
constrIdent :: Parser Text
constrIdent = satisfy isUpper >>= identifier'


identifier' :: Char -> Parser Text
identifier' firstLetter = lexeme $ check =<< do
  middleLetters <- takeWhileP (Just "alphaNum") isAlphaNum
  lastLetters   <- takeWhileP (Just "endChar") (`elem` endChars)
  pure $ cons firstLetter middleLetters <> lastLetters
    where
      check x = if x `elem` reservedWords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else pure x


-- 与えられた文字列を読む。後ろの空白（改行を含む）をスキップする。
symbol, word :: Text -> Parser Text
symbol = L.symbol spaceConsumer
  -- lexeme $ chunk w <* notFollowedBy (satisfy isPrint)

word txt = lexeme $ chunk txt <* notFollowedBy (satisfy isAlphaNum)

var :: Parser Text -> Parser ASTMeta
var p = meta $ do
  txt <- p
  pure $ ASTVar { astVar = txt }


meta :: Parser AST -> Parser ASTMeta
meta parseAst = do
  astSrcPos <- getSourcePos
  ast <- parseAst
  pure ASTMeta { astSrcPos = astSrcPos, ast = ast }

-- 型を一般的な形にする。例：a -> b であれば t0 -> t1
makeGeneralType :: Int -> Types
makeGeneralType n = Elems $ (\x -> Elem ("t" <> tShow x)) <$> [0..n]

tShow :: Show a => a -> Text
tShow = pack . show


-- カッコで挟まれる表現を読む
parens, braces, angles, brackets, dubquotes, quotes :: Parser a -> Parser a
parens    = between (symbol "(" <* many lineSep) (many lineSep *> symbol ")")
braces    = between (symbol "{" <* many lineSep) (many lineSep *> symbol "}")
angles    = between (symbol "<" <* many lineSep) (many lineSep *> symbol ">")
brackets  = between (symbol "[" <* many lineSep) (many lineSep *> symbol "]")
dubquotes = between (symbol "\"") (symbol "\"")
quotes    = between (symbol "'") (symbol "'")

-- 文の区切り文字を読む
lineSep :: Parser ()
lineSep = void $ lexeme $ satisfy (`elem` sepChars)

-- プログラムのトップレベルを読む
toplevels :: Parser ASTMeta
toplevels = dbg "toplevels" $
  meta $ do
  _    <- many lineSep
  tops <- (meta (ASTSeq {astSeq = [] } <$ eof)
            <|> toplevel)
          `sepBy1` some lineSep
  pure ASTSeq {astSeq = tops}

toplevel :: Parser ASTMeta
toplevel = dbg "toplevel" $
  choice [ funDef
         , typeDef
         , exprAST ]

-- 演算子とその処理。リストの先頭のほうが優先順位が高い。
ops :: [[Operator Parser ASTMeta]]
ops = [ Prefix . genUnary4OpTable <$> ["-"]
      , InfixR . genBinOp4OpTable <$> ["++", "**"]
      , [ InfixL (genBinOp4OpTable "." <* notFollowedBy integer) ]
      , InfixL . genBinOp4OpTable <$> ["*", "/"]
      , InfixL . genBinOp4OpTable <$> ["+", "-"]
      , InfixL . genBinOp4OpTable <$> [ "<="
                                      , "=>"
                                      , "<"
                                      , ">" ]
      , InfixR . genBinOp4OpTable <$> ["=="]
      , InfixL . genBinOp4OpTable <$> ["&&"]
      , InfixL . genBinOp4OpTable <$> ["||"]
      , InfixR . genBinOp4OpTable <$> ["="]
      ]

genUnary4OpTable txt = do
  meta     <- getSourcePos
  astUnary <- ASTUnary (OpLit txt) <$ symbol txt
  pure $ \astMeta ->
           ASTMeta { astSrcPos = meta
                   , ast       = astUnary astMeta }


genBinOp4OpTable txt = do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp (OpLit txt) <$ symbol txt
  pure $ \arg0 arg1 ->
           ASTMeta { astSrcPos = meta
                   , ast      = astBinOp arg0 arg1 }


-- 式を読む
exprAST :: Parser ASTMeta
exprAST =  dbg "exprAST" $
   makeExprParser term ops


-- 項を読む。項は演算子の引数になるもの。
term :: Parser ASTMeta
term = dbg "term" $
  choice [ try anonFun
         , apply
         , try ptn
         , braces seqAST
         , parens exprAST ]


-- パターンマッチの左辺値になるもの
ptn :: Parser ASTMeta
ptn = dbg "ptn" $
  choice [ list
         , meta $ ASTDouble <$> try double
         , meta $ ASTInt <$> integer
         , var identifier
         , var constrIdent
         , try $ parens (ptn <|> applyConstr) ]


-- 型定義を読む
typeDef :: Parser ASTMeta
typeDef =  dbg "typeDef" $
  meta $ do
  name  <- word "type" *> var constrIdent <* symbol "="
  types <- braces (memberWithType `sepBy1` symbol ",")
  pure ASTTypeDef { astTypeDefName   = name
                  , astTypeDefFields = types }
    where
      -- 型定義中の、構造体のメンバーとその型を読む
      memberWithType :: Parser (Text, RecList Type)
      memberWithType =  dbg "memberWithType" $
        do{ member <- constrIdent
          ; types  <- typeSig
          ; pure (member, types) }


-- パターンマッチを含む関数定義を読む
funDef :: Parser ASTMeta
funDef =  dbg "funDef" $
  meta $ do
  nameAST <- word "fun" *> try (var identifier) <|> var operator
  types'  <- optional typeSig
  _       <- symbol "="
  caseAST@ASTMeta
    { ast = ASTAnonFun
            { astType        = types
            , astCaseBranchs = matches }
    }     <- braces (caseAST True types')
             <|> caseAST False types'
  pure ASTFunDef { astType       = types
                 , astFunDefName = nameAST
                 , astFunParams  = paramList $ paramNum matches
                 , astFunBody    = caseAST }

-- 匿名関数を読む
anonFun :: Parser ASTMeta
anonFun =  dbg "anonFun" $
  meta $ do
  ASTMeta
    { ast = fun@ASTAnonFun { astType = types }
    }  <- parens (caseAST True Nothing)
          <|> caseAST False Nothing
  sig' <- optional typeSig
  -- 型を省略した場合はもっとも一般的な型にしちゃう
  let sig = fromMaybe types sig'
  pure $ fun { astType = sig }


-- パターンマッチ式を読む
caseAST :: Bool -> Maybe (RecList Type) -> Parser ASTMeta
caseAST isMany maybeType =  dbg "caseAST" $
  meta $ do
  matches <- if isMany
             then matchAST `endBy1` some lineSep
             else (:[]) <$> matchAST
  -- 型を省略した場合はもっとも一般的な型にしちゃう
  let types = fromMaybe (makeGeneralType (paramNum matches)) maybeType
  pure ASTAnonFun { astType        = types
                  , astCaseBranchs = matches }
    where
      -- パターンマッチ式を読む
      matchAST :: Parser ([ASTMeta], ASTMeta, Maybe ASTMeta)
      matchAST =  dbg "matchAST" $
        do{ conds <- some ptn
          ; guard <- optional ( symbol "|" *> exprAST <* symbol "|")
          ; body  <- symbol "->" *> exprAST
          ; pure (conds, body, guard) }

paramNum arms = maybe 1 (length . fst3) $ headMay arms
paramList n = zipWith (<>) (replicate n "x") (tShow <$> take n [1..])
fst3 (a,_,_) = a


-- 型注釈つきの式を読む


-- 文字列のリテラルを読む

-- リストのリテラルを読む

-- 複式（改行もしくは;で区切られて連続する式）を読む
seqAST :: Parser ASTMeta
seqAST =  dbg "seqAST" $
  meta $ do
  asts <- braces (var identifier `sepBy` lineSep)
  pure ASTSeq { astSeq = asts }

-- 式を読む。後ろの改行の連続をスキップする

-- 改行を読む。; も改行扱いとする。

-- 型定義を読む

-- 型定義中の、構造体のメンバーとその型を読む

-- 関数定義を読む

-- パターンマッチを含む関数定義を読む


-- if式を読む

-- 関数適用を読む
apply :: Parser ASTMeta
apply =  dbg "apply" $
  meta $ do
  caller <- anonFun <|> var constrIdent <|> var identifier
  args   <- some exprAST
  pure ASTApply { astApplyFun  = caller
                , astApplyArgs = args }


-- パターンマッチの左辺になる関数の適用を読む
applyConstr :: Parser ASTMeta
applyConstr =  dbg "apply" $
  meta $ do
  caller <- var constrIdent
  args   <- many ptn
  pure ASTApply { astApplyFun  = caller
                , astApplyArgs = args }

-- 型注釈を読む

-- 型を表す項を読む。Int, a, [Double], (Int-> String) など。

-- リスト型を読む
list :: Parser ASTMeta
list = dbg "list" $
  meta $ do
  ls <- brackets $ exprAST `sepBy` choice (symbol <$> [",", ";"])
  pure ASTList { astList = ls }


-- 型注釈を読む
typeSig :: Parser Types
typeSig =  dbg "typeList" $
  Elems <$> (symbol ":" *> typeTerm `sepBy` symbol "->")
  where
    -- 型を表す項を読む。Int, a, [Double], (Int->String) など。
    typeTerm :: Parser Types
    typeTerm =  dbg "typeTerm" $
      choice [ Elem <$> (constrIdent <|> identifier)
             , listTerm
             , parens typeSig ]
    -- リスト型を読む
    listTerm :: Parser Types
    listTerm =  dbg "listTerm" $
      do
        term <- brackets constrIdent
        pure $ Elems [ Elem "List", Elem term ]
