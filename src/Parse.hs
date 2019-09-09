{-# LANGUAGE OverloadedStrings #-}
-- 字句解析と構文解析
module Parse where

import           Control.Arrow                  ((&&&), (***), (>>>))
import           Control.Monad                  (join, void)
import qualified Control.Monad.Combinators.Expr as Comb
import           Data.Char
import           Data.List                      (find)
import           Data.Map                       (Map, fromList, toDescList)
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
type OpTable = [[Comb.Operator Parser ASTMeta]]

spaceConsumer, lineCmnt, blockCmnt  :: Parser ()

-- space consumer。空白やコメントをスキップする。改行はスキップしない。
spaceConsumer = L.space spaceOrTab1 lineCmnt blockCmnt
  where
    spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> isSpace c && c /= '\n')

-- 行コメントを読み飛ばす
lineCmnt  = L.skipLineComment "#"

-- ブロックコメントを読み飛ばす
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
var p = dbg "var" $
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
parens, braces, angles, brackets :: Parser a -> Parser a
parens    = between (symbol "(" *> skipSep) (skipSep <* symbol ")")
braces    = between (symbol "{" *> skipSep) (skipSep <* symbol "}")
brackets  = between (symbol "[" *> skipSep) (skipSep <* symbol "]")
angles    = between (symbol "<") (symbol ">")

-- 文の区切り文字を読む
lineSep :: Parser ()
lineSep = skipSome $ lexeme $ satisfy (`elem` sepChars)

-- 文の区切り文字を読み飛ばす
skipSep :: Parser ()
skipSep = L.space spaceOrLineSep lineCmnt blockCmnt
  where
    spaceOrLineSep = void $
      takeWhile1P (Just "line seperator") (\c -> isSpace c || c `elem` sepChars)


-- プログラムのトップレベルを読む
toplevels :: Parser ASTMeta
toplevels = dbg "toplevels" $
  meta $ do
  skipSep
  tops <- toplevel -- (opMaps2OpTables defOpMaps)
    `sepEndBy` lineSep
  pure ASTSeq { astSeq = tops }


-- toplevel :: OpTable -> Parser ASTMeta
toplevel = dbg "toplevel" $
  -- exprAST
  choice [ opDef
         -- , funDef
         , typeDef
         , exprAST ]


-- 項を読む。項は演算子の引数になるもの。
-- term :: OpTable -> Parser ASTMeta
term = dbg "term" $
  choice [ try anonFun
         , try ifAST
         , try ptn
         , braces seqAST
         , parens $ choice
           [ -- try astWithTypeSig
           -- ,
           try anonFuns
           , exprAST ] ]


-- パターンマッチの左辺値になるもの
ptn :: Parser ASTMeta
ptn = dbg "ptn" $
  choice [ list
         , str
         , meta $ ASTDouble <$> try double
         , meta $ ASTInt <$> integer
         , var identifier
         , var constrIdent
         , try $ parens ptn ]


genOpTable :: Parser OpTable
genOpTable = undefined

-- 演算子とその結合規則。リストの先頭のほうが優先順位が高い。
defOpMaps :: [Map Text Assoc]
defOpMaps = fromList <$>
  [ [("-", Prefix)
    -- ("°", Postfix)
    ]
  -- 関数適用はここ
  -- "."演算子はここ
  , flip (,) InfixL <$> ["*", "/"]
  , flip (,) InfixL <$> ["+", "-"]
  , flip (,) InfixL <$> [ "<=", "=>", "<", ">" ]
  , flip (,) InfixR <$> ["=="]
  , flip (,) InfixL <$> ["&&"]
  , flip (,) InfixL <$> ["||"]
  -- 型注釈はここ
  -- "="演算子(変数定義)はここ
  ]


opMaps2OpTables :: [Map Text Assoc] -> OpTable
opMaps2OpTables opMaps = (tup2ComBOp <$>) <$> (toDescList <$> opMaps)
  where
    tup2ComBOp :: (Text, Assoc) -> Comb.Operator Parser ASTMeta
    tup2ComBOp (opTxt, InfixR) = Comb.InfixR (genBinOp4OpTable opTxt)
    tup2ComBOp (opTxt, InfixL) = Comb.InfixL (genBinOp4OpTable opTxt)
    tup2ComBOp (opTxt, Prefix) = Comb.Prefix (genPrefix4OpTable opTxt)


exprAST :: Parser ASTMeta
exprAST =
  Comb.makeExprParser (term) newOpMaps
  where
    insertNonOp :: OpTable -> OpTable
    insertNonOp (prefix:infixs) =
      prefix:[Comb.InfixL apply]:infixs ++ opsTail
    insertNonOp [] = [Comb.InfixL apply]:opsTail
    opsTail = [ [Comb.Postfix astWithTypeSig]
              , [Comb.InfixR $ genBinOp4OpTable "="] ]
    newOpMaps = insertNonOp $ opMaps2OpTables defOpMaps


-- exprAST :: OpTable -> Parser ASTMeta
-- exprAST opMaps =
--   Comb.makeExprParser (term newOpMaps) newOpMaps
--   where
--     insertNonOp :: OpTable -> OpTable
--     insertNonOp (prefix:infixs) =
--       prefix:[Comb.InfixL apply]:infixs ++ opsTail
--     insertNonOp [] = [Comb.InfixL apply]:opsTail
--     opsTail = [ [Comb.Postfix astWithTypeSig]
--               , [Comb.InfixR $ genBinOp4OpTable "="] ]
--     newOpMaps = insertNonOp opMaps



-- -- 演算子とその処理。リストの先頭のほうが優先順位が高い。
-- ops :: OpTable
-- ops = [ [Comb.InfixL apply]
--       , Comb.Prefix . genPrefix4OpTable <$> ["-"]
--       , [Comb.InfixL (genBinOp4OpTable "." <* notFollowedBy integer)]
--       -- , InfixR . genBinOp4OpTable <$> ["++", "**"]
--       , Comb.InfixL . genBinOp4OpTable <$> ["*", "/"]
--       , Comb.InfixL . genBinOp4OpTable <$> ["+", "-"]
--       , Comb.InfixL . genBinOp4OpTable <$> [ "<="
--                                       , "=>"
--                                       , "<"
--                                       , ">" ]
--       , Comb.InfixR . genBinOp4OpTable <$> ["=="]
--       , Comb.InfixL . genBinOp4OpTable <$> ["&&"]
--       , Comb.InfixL . genBinOp4OpTable <$> ["||"]
--       , [Comb.Postfix astWithTypeSig]
--       , Comb.InfixR . genBinOp4OpTable <$> ["="] ]


genPrefix4OpTable :: Text -> Parser (ASTMeta -> ASTMeta)
genPrefix4OpTable txt = do
  meta     <- getSourcePos
  astUnary <- ASTUnary (OpLit txt) <$ chunk txt
  pure $ \astMeta ->
           ASTMeta { astSrcPos = meta
                   , ast       = astUnary astMeta }

-- genPostfix4OpTable :: Text -> Parser (ASTMeta -> ASTMeta)
-- genPostfix4OpTable txt = do
--   meta     <- getSourcePos
--   astUnary <- ASTUnary (OpLit txt) <$ chunk txt
--   pure $ \astMeta ->
--            ASTMeta { astSrcPos = meta
--                    , ast       = astUnary astMeta }


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
  meta <- getSourcePos
  pure $ \caller arg ->
           ASTMeta { astSrcPos = meta
                   , ast = ASTApply { astApplyFun = caller
                                    , astApplyArg = arg } }

-- 型注釈つきの式を読む
astWithTypeSig :: Parser (ASTMeta -> ASTMeta)
astWithTypeSig = do
  meta <- getSourcePos
  sig  <- typeSig
  pure $ \ast ->
    ASTMeta { astSrcPos = meta
            , ast = ASTTypeSig { astType       = sig
                               , astTypeSigVar = ast } }


-- -- 式を読む
-- exprAST :: Parser ASTMeta
-- exprAST =  -- dbg "exprAST" $
--    Comb.makeExprParser term ops


-- 型定義を読む
typeDef :: Parser ASTMeta
typeDef =  dbg "typeDef" $
  meta $ do
  name  <- word "type" *> var constrIdent <* symbol "="
  types <- braces (memberWithType `sepEndBy` (symbol "," <* skipSep))
  pure ASTTypeDef { astTypeDefName   = name
                  , astTypeDefFields = types }
    where
      -- 型定義中の、構造体のメンバーとその型を読む
      memberWithType :: Parser (Text, RecList Type)
      memberWithType =  dbg "memberWithType" $
        do{ member <- identifier
          ; types  <- typeSig
          ; pure (member, types) }



-- パターンマッチを含む関数定義を読む
funDef :: Parser ASTMeta
funDef =  dbg "funDef" $
  meta $ do
  nameAST   <- word "fun" *> var identifier
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


-- 演算子の定義を読む
opDef :: Parser ASTMeta
opDef =  dbg "opDef" $
  meta $ do
  nameAST@ASTMeta
    { ast = ASTVar { astVar = opName }
    }       <- word "fun" *> var operator
  let
    assocR = ((,) True)
      <$> (symbol opName *>
            choice [ try $ StrongerThan . OpLit <$> (symbol ">" *> operator)
                   , try $ EqualTo . OpLit <$> (symbol "==" *> operator)
                   , WeakerThan . OpLit <$> (symbol "<" *> operator) ])
    assocL = ((,) False)
      <$> (choice [ try $ StrongerThan . OpLit <$> (operator *> symbol ">")
                  , try $ EqualTo . OpLit <$> (operator *> symbol "==")
                  , WeakerThan . OpLit <$> (operator *> symbol "<") ]
            <* symbol opName)
  maybeAssc <- optional (symbol "|" *> (try assocL <|> assocR) <* symbol "|")
  maybeType <- optional typeSig
  _         <- symbol "="
  body      <- braces (anonFun `sepEndBy` lineSep)
               <|> (:[]) <$> anonFun
  let types = fromMaybe (makeGeneralType (paramNum body)) maybeType
  pure ASTOpDef { astType      = types
                , astOpAssoc   = maybeAssc
                , astOpDefName = nameAST
                , astOpParams  = paramList $ paramNum body
                , astOpBody    = body }
    where
      assocStr = choice [ StrongerThan . OpLit <$> (symbol ">" *> operator)
                        , EqualTo . OpLit <$> (symbol "==" *> operator)
                        , WeakerThan . OpLit <$> (symbol "<" *> operator) ]



-- 匿名関数を読む
anonFun :: Parser ASTMeta
anonFun =  dbg "anonFun" $
  meta $ do
  -- パターンマッチ式を読む
  conds <- some ptn
  guard <- optional (symbol "|" *> exprAST <* symbol "|")
  body  <- symbol "->" *> exprAST
  pure ASTAnonFun { astPattern = conds
                  , astBody    = body
                  , astGuard   = guard}

anonFuns :: Parser ASTMeta
anonFuns = dbg "anonFuns" $
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
      pure ASTAnons { astAnons = anons }


-- 複式（改行もしくは;で区切られて連続する式）を読む
seqAST :: Parser ASTMeta
seqAST =  dbg "seqAST" $
  undefined
  -- meta $ do
  -- asts <- toplevel `sepEndBy` lineSep
  -- pure ASTSeq { astSeq = asts }



-- if式を読む
ifAST :: Parser ASTMeta
ifAST =  dbg "ifAST" $
  meta $ do
  condAST <- word "if" *> exprAST
  thenAST <- word "then" *> exprAST
  elseAST <- word "else" *> exprAST
  pure ASTIf { astIfCond = condAST
             , astIfThen = thenAST
             , astIfElse = elseAST }


-- リストのリテラルを読む
list :: Parser ASTMeta
list = dbg "list" $
  meta $ do
  ls <- brackets $ (exprAST <* C.space) `sepBy` seperator
  pure ASTList { astList = ls }
  where
    seperator = L.lexeme (L.space C.space1 lineCmnt blockCmnt)
                $ oneOf (",;" :: [Char])


-- 文字列のリテラルを読む
str :: Parser ASTMeta
str = dbg "str" $
  meta $ do
  beginChar <- single '"' <|> single '\''
  string    <- takeWhileP (Just ("string between" <> [beginChar])) (/= beginChar)
  _         <- single beginChar <* spaceConsumer
  pure ASTStr { astStr = string }


-- 型注釈を読む
typeSig :: Parser Types
typeSig =  dbg "typeList" $
  symbol ":" *> types
  where
    types = Elems <$> (typeTerm `sepBy` symbol "->")
    -- 型を表す項を読む。Int, a, [Double], (Int->String) など。
    typeTerm :: Parser Types
    typeTerm =  dbg "typeTerm" $
      choice [ Elem <$> (constrIdent <|> identifier)
             , listTerm
             , parens types ]
    -- リスト型を読む
    listTerm :: Parser Types
    listTerm =  dbg "listTerm" $
      do
        term <- brackets (constrIdent <|> identifier)
        pure $ Elems [ Elem "List", Elem term ]
