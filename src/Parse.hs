{-# LANGUAGE OverloadedStrings #-}
-- 字句解析と構文解析
module Parse where

import           Control.Arrow                  (arr, (&&&), (***), (>>>))
import           Control.Monad                  (join, void)
import qualified Control.Monad.Combinators.Expr as Comb
import           Data.Char
import           Data.List                      (find)
import           Data.Map                       ()
import qualified Data.Map                       as Map
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

spaceConsumer, spaceConsumer1, lineCmnt, blockCmnt :: Parser ()

-- space consumer。空白やコメントをスキップする。改行はスキップしない。
spaceConsumer = skipMany $ singleSpace spaceOrTab1 lineCmnt blockCmnt

-- space consumer。一つ以上の空白やコメントをスキップする。改行はスキップしない。
spaceConsumer1 = skipSome $ singleSpace spaceOrTab1 lineCmnt blockCmnt

spaceOrTab1 = void $ takeWhile1P (Just "white space") isSeparator -- (\c -> isSpace c && c /= '\n')

-- 行コメントを読み飛ばす
lineCmnt  = L.skipLineComment "#"

-- ブロックコメントを読み飛ばす
blockCmnt = L.skipBlockComment "/*" "*/"

singleSpace :: Parser a -> Parser a -> Parser a -> Parser ()
singleSpace spc lnCmnt blkCmnt = () <$ choice
  [hidden spc, hidden lnCmnt, hidden blkCmnt]

betweens p = between p p

lexeme, isolated, lexemeSep, isolatedSep, isolatedNewline :: Parser a -> Parser a
lexeme      = betweens spaceConsumer
isolated    = betweens spaceConsumer1
lexemeSep   = betweens skipSep
isolatedSep = betweens lineSep
isolatedNewline = betweens C.space1


-- 文の区切り文字を読む
lineSep :: Parser ()
lineSep = skipSome $ lexeme $ satisfy (`elem` sepChars)

-- 文の区切り文字を読み飛ばす
skipSep :: Parser ()
skipSep = skipMany $ singleSpace spaceOrLineSep1 lineCmnt blockCmnt

spaceOrLineSep1 = void $
  takeWhile1P (Just "line seperator") (isSpace &||& (`elem` sepChars))


-- 整数を読む
integer :: Parser Integer
integer = -- lexeme
  L.decimal

-- 浮動小数点数を読む
double :: Parser Double
double = -- lexeme
  L.float

opChars, endChars, sepChars :: [Char]
opChars  = "!#$%&*+./<=>?@\\^|-~:"
endChars = "!?_'"
sepChars = ";\n"

isOpChar, isIdentChar :: Char -> Bool
isOpChar = isSymbol &||& (`elem` opChars)
isIdentChar =
  ('_' ==)
  &||& isDigit
  &||& (isLetter
  &&&& (not . (isSpace
               &&&& isOpChar
               &&&& (`elem` sepChars))))

(&&&&) x y = (x &&& y) >>> arr (uncurry (&&))
(&||&) x y = (x &&& y) >>> arr (uncurry (||))



-- 予約語のリスト
reservedWords, reservedOps, reserveds :: [Text] -- list of reserved words
reservedWords = ["fun","if","then","else","type"]
reservedOps = [":", "=", "->"]
reserveds = reservedWords <> reservedOps


-- 演算子を読む
opIdent :: Parser Text
opIdent = try $ takeWhile1P (Just "symbol") isOpChar >>= check
  where
    check x = if x `elem` reservedOps
      then fail $ "keyword " <> show x <> " cannot be an opIdent"
      else pure x


-- 識別子を読む
identifier :: Parser Text
identifier = satisfy isLower >>= identifier'


-- 識別子を読む
constrIdent :: Parser Text
constrIdent = satisfy isUpper >>= identifier'


identifier' :: Char -> Parser Text
identifier' firstLetter = -- lexeme $
  check =<< do
  middleLetters <- takeWhileP (Just "alphaNum and _") isIdentChar
  lastLetters   <- takeWhileP (Just "endChar") (`elem` endChars)
  pure $ cons firstLetter middleLetters <> lastLetters
    where
      check x = if x `elem` reservedWords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else pure x


-- 与えられた文字列を読む。後ろの空白（改行を含む）をスキップする。
symbol, word :: Text -> Parser Text

symbol txt = lexeme $ chunk txt
-- symbol = L.symbol spaceConsumer
  -- lexeme $ chunk w <* notFollowedBy (satisfy isPrint)

word txt = lexemeSep $ chunk txt <* notFollowedBy (satisfy isIdentChar)


var :: Parser Text -> Parser ASTMeta
var p = dbg "var" $
  meta $ do
  txt <- p
  pure $ ASTVar { astVar = txt }


meta :: Parser AST -> Parser ASTMeta
meta parseAst = do
  astSrcPos <- getSourcePos
  ast       <- parseAst
  pure ASTMeta { astSrcPos = astSrcPos, ast = ast }

-- 型を一般的な形にする。例：a -> b であれば t0 -> t1
makeGeneralType :: Int -> Types
makeGeneralType n = Elems $ (\x -> Elem ("t" <> tShow x)) <$> [0..n]

tShow :: Show a => a -> Text
tShow = pack . show


-- カッコで挟まれる表現を読む
parens, braces, angles, guards, brackets :: Parser a -> Parser a
parens    = between (chunk "(") (chunk ")") . lexemeSep
braces    = between (chunk "{") (chunk "}") . lexemeSep
brackets  = between (chunk "[") (chunk "]") . lexemeSep
guards    = betweens (chunk "|") . lexemeSep
angles    = between (chunk "<") (chunk ">")


-- プログラムのトップレベルを読む
toplevels :: Parser ASTMeta
toplevels = dbg "toplevels" $
  seqAST

-- 複式（改行もしくは;で区切られて連続する式）を読む
seqAST :: Parser ASTMeta
seqAST =  dbg "seqAST" $
  meta $ do
  asts <- lexemeSep $ some $ toplevel <* lineSep
  pure ASTSeq { astSeq = asts }


toplevel :: Parser ASTMeta
toplevel = dbg "toplevel" $
  choice [ typeDef
         , funDef
         , exprAST ]


-- 項を読む。項は演算子の引数になるもの。
term :: Parser ASTMeta
term = dbg "term" $
       choice [ try anonFun
              , try ptn
              , try ifAST
              , braces seqAST
              , parens $ choice
                [
                  -- try astWithTypeSig
                  -- , try anonFuns
                  -- ,
                  exprAST
                ]
              ]


-- パターンマッチの左辺値になるもの
ptn :: Parser ASTMeta
ptn = dbg "ptn" $
  choice [ list
         , str
         , meta $ ASTDouble <$> try double
         , meta $ ASTInt <$> integer
         , var identifier
         , var constrIdent
         , try $ parens ptn
         ]

-- -- 演算子とその結合規則。リストの先頭のほうが優先順位が高い。
-- defOpMaps :: OpMaps
-- defOpMaps = Map.fromList <$>
--   [ [("-", Prefix)
--     -- ("°", Postfix)
--     ]
--   -- 関数適用はここ
--   -- "."演算子はここ
--   -- , flip (,) InfixL <$> ["*", "/"]
--   -- , flip (,) InfixL <$> ["+", "-"]
--   -- , flip (,) InfixL <$> [ "<=", ">=", "<", ">" ]
--   -- , flip (,) InfixR <$> ["=="]
--   -- , flip (,) InfixL <$> ["&&"]
--   -- , flip (,) InfixL <$> ["||"]
--   -- 型注釈はここ
--   -- "="演算子(変数定義)はここ
--   ]

-- 演算子とその結合規則。リストの先頭のほうが優先順位が高い。
weakerThanApply =
  [--  Comb.Prefix . genPrefix4OpTable <$> ["-"]
  -- , [Comb.InfixL dot]
  -- , [Comb.InfixL apply]
  -- ,
    [Comb.InfixL defLAssocOp]
  , [Comb.Postfix astWithTypeSig]
  , [Comb.InfixR assign]
  ]

strongerThanApply =
  [ Comb.Prefix . genPrefix4OpTable <$> ["-"]
  , [Comb.InfixL dot]
  ]

genPrefix4OpTable :: Text -> Parser (ASTMeta -> ASTMeta)
genPrefix4OpTable txt = try $ do
  meta     <- getSourcePos
  astUnary <- try $ ASTUnary . OpLit <$> chunk txt <* notFollowedBy opIdent
  pure $ \astMeta ->
           ASTMeta { astSrcPos = meta
                   , ast       = astUnary astMeta }

dot = try $ do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp Dot <$ chunk "."
              <* notFollowedBy (() <$ opIdent <|> () <$ integer)
  pure $ \arg0 arg1 ->
           ASTMeta { astSrcPos = meta
                   , ast       = astBinOp arg0 arg1 }

-- -- 関数適用を読む
-- apply :: Parser (ASTMeta -> ASTMeta -> ASTMeta)
-- apply = try $ do
--   meta <- getSourcePos
--   _    <- spaceConsumer1
--   pure $ \caller arg ->
--            ASTMeta { astSrcPos = meta
--                    , ast = ASTApply { astApplyFun = caller
--                                     , astApplyArg = arg } }

defLAssocOp :: Parser (ASTMeta -> ASTMeta -> ASTMeta)
defLAssocOp = try $ do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp . OpLit <$> isolatedNewline opIdent
  pure $ \arg0 arg1 ->
           ASTMeta { astSrcPos = meta
                   , ast       = astBinOp arg0 arg1 }

assign :: Parser (ASTMeta -> ASTMeta -> ASTMeta)
assign = try $ do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp . OpLit <$> symbol "="
  pure $ \arg0 arg1 ->
           ASTMeta { astSrcPos = meta
                   , ast       = astBinOp arg0 arg1 }

-- genBinOp4OpTable :: Text -> Parser (ASTMeta -> ASTMeta -> ASTMeta)
-- genBinOp4OpTable txt = try $ do
--   meta     <- getSourcePos
--   astBinOp <- ASTBinOp . OpLit <$> (chunk txt <* notFollowedBy opIdent)
--               <* skipSep
--   pure $ \arg0 arg1 ->
--            ASTMeta { astSrcPos = meta
--                    , ast       = astBinOp arg0 arg1 }


-- 関数適用を読む
apply :: Parser ASTMeta -> Parser ASTMeta
apply arg = dbg "apply" $
  try $ arg >>= apply'
  where
    apply' :: ASTMeta -> Parser ASTMeta
    apply' caller = dbg "apply'" $
      option caller
      (try $ do
          _    <- spaceConsumer1
          meta <- getSourcePos
          arg  <- arg
          apply' ASTMeta
            { astSrcPos = meta
            , ast       = ASTApply
                          { astApplyFun = caller
                          , astApplyArg = arg } })



-- 型注釈つきの式を読む
astWithTypeSig :: Parser (ASTMeta -> ASTMeta)
astWithTypeSig = try $ do
  meta <- getSourcePos
  sig  <- typeSig
  pure $ \ast ->
    ASTMeta { astSrcPos = meta
            , ast = ASTTypeSig { astType       = sig
                               , astTypeSigVar = ast } }


-- 式を読む
exprAST :: Parser ASTMeta
exprAST =  dbg "exprAST" $
  -- Comb.makeExprParser term ops
  Comb.makeExprParser (apply args) weakerThanApply
  where
    args = dbg "args" $
           Comb.makeExprParser term strongerThanApply


-- 型定義を読む
typeDef :: Parser ASTMeta
typeDef =  dbg "typeDef" $
  meta $ do
  name  <- word "type" *> var constrIdent <* symbol "="
  types <- braces (memberWithType `sepEndBy` (chunk "," <* skipSep))
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
  nameAST   <- word "fun" *> (var identifier <|> var (opIdent <* spaceConsumer))
  maybeType <- optional $ try typeSig
  _         <- symbol "="
  body      <- braces seqAST <|> exprAST
  let types = fromMaybe (makeGeneralType (paramNum body)) maybeType
  pure ASTFunDef { astType       = types
                 , astFunDefName = nameAST
                 , astFunParams  = paramList $ paramNum body
                 , astFunBody    = body }

paramNum :: ASTMeta -> Int
paramNum seq
  | isSeq     (ast seq) = maybe 0 (length . astPattern)
                          $ find isAnonFun $ ast <$> astSeq (ast seq)
  | isAnonFun (ast seq) = length $ astPattern (ast seq)
  | otherwise           = 0
  where
    isAnonFun ASTAnonFun {} = True
    isAnonFun _             = False
    isSeq ASTSeq {} = True
    isSeq _         = False

paramList :: Int -> [Text]
paramList n =
  zipWith (<>) (replicate n "x") (tShow <$> take n [1..])



-- 匿名関数を読む
anonFun :: Parser ASTMeta
anonFun = dbg "anonFun" $
  do
    srcPos <- getSourcePos

    -- パターンマッチ式を読む
    conds <- some ptn
    guard <- optional (guards exprAST)
    body  <- symbol "->" *> exprAST
    let fun = ASTMeta { astSrcPos = srcPos
                        , ast     =  ASTAnonFun
                                     { astPattern = conds
                                     , astBody    = body
                                     , astGuard   = guard } }
    maybe fun
      (\sig ->
          ASTMeta { astSrcPos = srcPos
                  , ast       = ASTTypeSig
                                { astType       = sig
                                , astTypeSigVar = fun } })
      <$> optional typeSig

-- anonFuns :: Parser ASTMeta
-- anonFuns = -- dbg "anonFuns" $
--   do
--   srcPos <- getSourcePos
--   anons  <- anonFuns'
--   maybe anons
--     (\sig ->
--        ASTMeta { astSrcPos = srcPos
--                , ast = ASTTypeSig { astType       = sig
--                                   , astTypeSigVar = anons }})
--     <$> optional typeSig
--   where
--     anonFuns' = meta $ do
--       anons <- anonFun `sepEndBy` lineSep
--       pure ASTSeq { astSeq = anons }



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
  ls <- brackets (exprAST `sepBy` seperator)
  pure ASTList { astList = ls }
  where
    seperator = betweens (L.space C.space1 lineCmnt blockCmnt)
                $ oneOf (",;" :: [Char])

-- 文字列のリテラルを読む
str :: Parser ASTMeta
str = dbg "str" $
  meta $ do
  beginChar <- single '"' <|> single '\''
  string    <- takeWhileP (Just ("string between" <> [beginChar])) (/= beginChar)
  _         <- single beginChar
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
      lexeme $ choice
      [ Elem <$> (constrIdent <|> identifier)
      , listTerm
      , parens types ]
    -- リスト型を読む
    listTerm :: Parser Types
    listTerm =  dbg "listTerm" $
      do
        term <- brackets (constrIdent <|> identifier)
        pure $ Elems [ Elem "List", Elem term ]
