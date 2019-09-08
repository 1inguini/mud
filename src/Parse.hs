{-# LANGUAGE OverloadedStrings #-}
-- 字句解析と構文解析
module Parse where

import           Control.Arrow                  ((&&&), (***), (>>>))
import           Control.Monad                  (join, void)
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.List                      (find)
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
    blockCmnt = L.skipBlockCommentNested "/*" "*/"

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
sepChars = ";\n"


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
  middleLetters <- takeWhileP (Just "alphaNum and _") (\c -> isAlphaNum c || c == '_')
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

word txt = between skipSep skipSep $ lexeme $
           chunk txt <* notFollowedBy (satisfy isAlphaNum)

var :: Parser Text -> Parser ASTMeta
var p = -- dbg "var" $
  meta $ do
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
parens    = between (symbol "(" <* skipSep) (symbol ")")
braces    = between (symbol "{" <* skipSep) (symbol "}")
angles    = between (symbol "<" <* skipSep) (symbol ">")
brackets  = between (symbol "[" <* skipSep) (symbol "]")
dubquotes = between (symbol "\"") (symbol "\"")
quotes    = between (symbol "'") (symbol "'")

-- 文の区切り文字を読む
lineSep :: Parser ()
lineSep = skipSome $ lexeme $ satisfy (`elem` sepChars)

-- 文の区切り文字を読み飛ばす
skipSep :: Parser ()
skipSep = skipMany $ lexeme $ satisfy (`elem` sepChars)


-- programParser :: FilePath -> Parser ASTMeta
-- programParser filepath = do
--   updateParserState $ \st@State { statePosState = pos } ->
--     st { statePosState = pos { pstateSourcePos = initialPos filepath} }
--   toplevels


-- プログラムのトップレベルを読む
toplevels :: Parser ASTMeta
toplevels = -- dbg "toplevels" $
  meta $ do
  spaceConsumer
  skipSep
  tops <- toplevel `sepEndBy` lineSep
  pure ASTSeq { astSeq = tops }


toplevel :: Parser ASTMeta
toplevel = -- dbg "toplevel" $
  -- exprAST
  choice [ funDef
         , typeDef
         , exprAST ]


-- 項を読む。項は演算子の引数になるもの。
term :: Parser ASTMeta
term = -- dbg "term" $
  choice [ try anonFun
         , try ifAST
         , try ptn
         , braces seqAST
         , parens $ choice
           [ try astWithTypeSig
           , try anonFuns
           , exprAST ] ]


-- パターンマッチの左辺値になるもの
ptn :: Parser ASTMeta
ptn = -- dbg "ptn" $
  choice [ list
         , str
         , meta $ ASTDouble <$> try double
         , meta $ ASTInt <$> integer
         , var identifier
         , var constrIdent
         , try $ parens ptn ]


-- 演算子とその処理。リストの先頭のほうが優先順位が高い。
ops :: [[Operator Parser ASTMeta]]
ops = [ Prefix . genUnary4OpTable <$> ["-"]
      ,[InfixL apply]
      , InfixR . genBinOp4OpTable <$> ["++", "**"]
      , [InfixL (genBinOp4OpTable "." <* notFollowedBy integer)]
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


genUnary4OpTable :: Text -> Parser (ASTMeta -> ASTMeta)
genUnary4OpTable txt = do
  meta     <- getSourcePos
  astUnary <- ASTUnary (OpLit txt) <$ symbol txt
  pure $ \astMeta ->
           ASTMeta { astSrcPos = meta
                   , ast       = astUnary astMeta }


genBinOp4OpTable :: Text -> Parser (ASTMeta -> ASTMeta -> ASTMeta)
genBinOp4OpTable txt = do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp (OpLit txt) <$ symbol txt <* skipSep
  pure $ \arg0 arg1 ->
           ASTMeta { astSrcPos = meta
                   , ast      = astBinOp arg0 arg1 }


-- 関数適用を読む
apply :: Parser (ASTMeta -> ASTMeta -> ASTMeta)
apply = do
  meta   <- getSourcePos
  -- caller <- -- anonFun <|>
  --   var constrIdent <|> var identifier
  pure $ \caller arg ->
           ASTMeta { astSrcPos = meta
                   , ast = ASTApply { astApplyFun = caller
                                    , astApplyArg = arg } }


-- -- 型注釈つきの無名関数を読む
-- anonWithTypeSig :: Parser ASTMeta
-- anonWithTypeSig = -- -- dbg "astWithTypeSig" $
--   meta $ do
--   ast' <- anonFun
--   sig  <- typeSig
--   pure $ ASTTypeSig { astType = sig, astTypeSigVar = ast'}


-- 式を読む
exprAST :: Parser ASTMeta
exprAST =  -- dbg "exprAST" $
   makeExprParser term ops


-- 型定義を読む
typeDef :: Parser ASTMeta
typeDef =  -- dbg "typeDef" $
  meta $ do
  name  <- word "type" *> var constrIdent <* symbol "="
  types <- braces (memberWithType `sepBy1` symbol ",")
  pure ASTTypeDef { astTypeDefName   = name
                  , astTypeDefFields = types }
    where
      -- 型定義中の、構造体のメンバーとその型を読む
      memberWithType :: Parser (Text, RecList Type)
      memberWithType =  -- dbg "memberWithType" $
        do{ member <- identifier
          ; types  <- typeSig
          ; pure (member, types) }


-- 匿名関数を読む
anonFun :: Parser ASTMeta
anonFun =  -- dbg "anonFun" $
  meta $ do
  -- パターンマッチ式を読む
  conds <- some ptn
  guard <- optional (symbol "|" *> exprAST <* symbol "|")
  body  <- symbol "->" *> exprAST
  pure ASTAnonFun { astPattern = conds
                  , astBody    = body
                  , astGuard   = guard}


-- パターンマッチを含む関数定義を読む
funDef :: Parser ASTMeta
funDef =  -- dbg "funDef" $
  meta $ do
  nameAST   <- word "fun" *> (var identifier <|> var operator)
  maybeType <- optional typeSig
  _         <- symbol "="
  body      <- braces (anonFun -- <|> funDef
                        `sepEndBy` lineSep)
               <|> (:[]) <$> anonFun
  let types = fromMaybe (makeGeneralType (paramNum body)) maybeType
  pure ASTFunDef { astType       = types
                 , astFunDefName = nameAST
                 , astFunParams  = paramList $ paramNum body
                 , astFunBody    = body }

paramNum :: [ASTMeta] -> Int
paramNum arms =
  maybe 0 (length . astPattern) $ find isAnonFun $ ast <$> arms
  where
    isAnonFun ASTAnonFun {} = True
    isAnonFun _             = False
paramList :: Int -> [Text]
paramList n =
  zipWith (<>) (replicate n "x") (tShow <$> take n [1..])


anonFuns :: Parser ASTMeta
anonFuns = -- dbg "anonFuns" $
  do
  srcPos <- getSourcePos
  anons  <- anonFuns'
  maybe anons
    (\sig ->
       ASTMeta { astSrcPos = srcPos
               , ast = ASTTypeSig { astType       = sig
                                  , astTypeSigVar = anons }})
    <$> optional typeSig
  where
    anonFuns' = meta $ do
      anons <- anonFun `sepEndBy` lineSep
      pure ASTSeq { astSeq = anons }


-- 複式（改行もしくは;で区切られて連続する式）を読む
seqAST :: Parser ASTMeta
seqAST =  -- dbg "seqAST" $
  meta $ do
  asts <- toplevel `sepEndBy` lineSep
  pure ASTSeq { astSeq = asts }


-- if式を読む
ifAST :: Parser ASTMeta
ifAST =  -- dbg "ifAST" $
  meta $ do
  condAST <- word "if" *> exprAST
  thenAST <- word "then" *> exprAST
  elseAST <- word "else" *> exprAST
  pure ASTIf { astIfCond = condAST
             , astIfThen = thenAST
             , astIfElse = elseAST }


-- -- パターンマッチの左辺になる関数の適用を読む
-- applyConstr :: Parser ASTMeta
-- applyConstr = -- -- dbg "apply" $
--   meta $ do
--   caller <- var constrIdent
--   args   <- ptn
--   pure ASTApply { astApplyFun  = caller
--                 , astApplyArg = args }


-- リストのリテラルを読む
list :: Parser ASTMeta
list = -- dbg "list" $
  meta $ do
  ls <- brackets $ exprAST `sepBy` choice (symbol <$> [",", ";"])
  pure ASTList { astList = ls }


-- 文字列のリテラルを読む
str :: Parser ASTMeta
str = -- dbg "str" $
  meta $ do
  beginChar <- single '"' <|> single '\''
  string    <- takeWhileP (Just ("string between" <> [beginChar])) (/= beginChar)
  _         <- single beginChar <* spaceConsumer
  pure ASTStr { astStr = string }

-- 型注釈つきの式を読む
astWithTypeSig :: Parser ASTMeta
astWithTypeSig =  -- dbg "astWithTypeSig" $
  meta $ do
  ast  <- exprAST
  sig  <- typeSig
  pure ASTTypeSig { astType       = sig
                  , astTypeSigVar = ast }


-- 型注釈を読む
typeSig :: Parser Types
typeSig =  -- dbg "typeList" $
  symbol ":" *> types
  where
    types = Elems <$> (typeTerm `sepBy` symbol "->")
    -- 型を表す項を読む。Int, a, [Double], (Int->String) など。
    typeTerm :: Parser Types
    typeTerm =  -- dbg "typeTerm" $
      choice [ Elem <$> (constrIdent <|> identifier)
             , listTerm
             , parens types ]
    -- リスト型を読む
    listTerm :: Parser Types
    listTerm =  -- dbg "listTerm" $
      do
        term <- brackets (constrIdent <|> identifier)
        pure $ Elems [ Elem "List", Elem term ]
