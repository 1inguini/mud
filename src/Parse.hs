{-# LANGUAGE OverloadedStrings #-}
-- 字句解析と構文解析
module Parse where

import           Control.Arrow                  ((&&&), (***), (>>>))
import           Control.Monad                  (join, void)
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Maybe
import           Data.Text                      as T
import           Data.Void
import           Debug.Trace
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
  middleLetters <- takeWhileP (Just "") isAlphaNum
  lastLetters   <- takeWhileP (Just "") (`elem` endChars)
  pure $ cons firstLetter middleLetters <> lastLetters
    where
      check x = if x `elem` reservedWords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else pure x


-- 与えられた文字列を読む。後ろの空白（改行を含む）をスキップする。
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer
  -- lexeme $ chunk w <* notFollowedBy (satisfy isPrint)


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
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
dubquotes = between (symbol "\"") (symbol "\"")
quotes    = between (symbol "'") (symbol "'")



lineSep :: Parser ()
lineSep = void $ lexeme $ satisfy (`elem` sepChars)

toplevels :: Parser ASTMeta
toplevels = dbg "toplevels" $
  meta $ do
  _ <- many lineSep
  tops <- toplevel `sepBy1` some lineSep
  _ <- many lineSep <* dbg "eof" eof
  pure ASTSeq {astSeq = tops}


toplevel :: Parser ASTMeta
toplevel =
  meta (ASTSeq {astSeq = [] } <$ eof) <|> exprAst

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
exprAst :: Parser ASTMeta
exprAst =  dbg "exprAst" $
   makeExprParser term ops

-- 項を読む。項は演算子の引数になるもの。
term :: Parser ASTMeta
term = dbg "term" $
  choice [ var identifier
         , var constrIdent
         , parens term]


-- 匿名関数を読む
  -- 型を省略した場合はもっとも一般的な型にしちゃう

-- 型を一般的な形にする。例：a -> b であれば t0 -> t1

-- 型注釈つきの式を読む

-- 型注釈つきの項を読む

-- 文字列のリテラルを読む

-- リストのリテラルを読む

-- 複式（改行で区切られて連続する式）を読む

-- 式を読む。後ろの改行の連続をスキップする

-- 改行を読む。; も改行扱いとする。

-- プログラムのトップレベルを読む

-- 型定義を読む

-- 型定義中の、構造体のメンバーとその型を読む

-- 関数定義を読む

-- パターンマッチを含む関数定義を読む

-- パターンマッチ式を読む


-- if式を読む

-- 関数適用を読む

-- 型注釈を読む

-- 型を表す項を読む。Int, a, [Double], (Int-> String) など。

-- リスト型を読む
