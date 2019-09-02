-- 字句解析と構文解析
module Parse where

import           Control.Arrow                  ((&&&), (***), (>>>))
import           Control.Monad                  (join, void)
import           Control.Monad.Combinators.Expr
import           Data.Maybe
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Expr
import           RecList

type Parser = Parsec Void String

-- space consumer 。空白やコメントをスキップする。改行はスキップしない。
sc :: Parser ()
sc = L.space spaceOrTab1 lineCmnt blockCmnt
  where
    spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> c == ' ' || c == '\t')
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- 改行を含む空白やコメントをスキップする
scn :: Parser ()
scn = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- 整数を読む
integer :: Parser Integer
integer = lexeme L.decimal

-- 浮動小数点数を読む
double :: Parser Double
double = lexeme L.float

-- 演算子を読む
operator :: Parser String
operator = lexeme $ some (oneOf "+-*/><")

-- 予約語を読む
rword :: String -> Parser ()
rword w = (lexeme . try) (space >> string w *> notFollowedBy alphaNumChar)

-- 予約語のリスト
reservedWords :: [String] -- list of reserved words
reservedWords = ["fun","if","then","else","type"]

-- 識別子を読む
identifier :: Parser String
identifier = (lexeme . try) (identifier' >>= check)
  where
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else pure x

identifier' :: Parser String
identifier' = do
  firstLetter <- letterChar
  middleLetters <- many (alphaNumChar <|> single '_')
  lastLetters <- many (oneOf "!?_'")
  pure $ firstLetter:middleLetters ++ lastLetters


var :: Parser String -> Parser ASTMeta
var p = do
  srcPos <- getSourcePos
  str <- p
  pure ASTMeta { srcPos = srcPos
               , ast    = ASTVar { astVar = str } }

-- 与えられた文字列を読む。後ろの空白（改行を含む）をスキップする。
symbol :: String -> Parser String
symbol = L.symbol scn

-- 与えられた文字列を読む。後ろの空白（改行を含まない）をスキップする。
symboln :: String -> Parser String
symboln = L.symbol sc

-- カッコで挟まれる表現を読む
parens, braces, angles, brackets, dubquotes, quotes :: Parser a -> Parser a
parens    = between (symbol "(") (symboln ")")
braces    = between (symbol "{") (symboln "}")
angles    = between (symbol "<") (symboln ">")
brackets  = between (symbol "[") (symboln "]")
dubquotes = between (symbol "\"") (symboln "\"")
quotes    = between (symbol "'") (symboln "'")

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

genUnary4OpTable str = do
  meta     <- getSourcePos
  astUnary <- ASTUnary (OpLit str) <$ symbol str
  pure (\astMeta ->
          ASTMeta { srcPos = meta
                  , ast      = astUnary astMeta })

genBinOp4OpTable str = do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp (OpLit str) <$ symbol str
  pure (\arg0 arg1 ->
           ASTMeta { srcPos = meta
                   , ast      = astBinOp arg0 arg1 })


-- 式を読む
exprAst :: Parser ASTMeta
exprAst = makeExprParser term ops

-- 項を読む。項は演算子の引数になるもの。
term :: Parser ASTMeta
term = choice
       [ ast2astMeta $ try anonFun
       , try apply
       , arg
       , try ifAST
       , try funDefCase
       , fundef
       , typeDef ]

-- 関数の引数になりうるものを読む
arg :: Parser ASTMeta
arg = choice
      [ ast2astMeta $ ASTDoubleLit <$> try double
      , ast2astMeta $ ASTIntLit <$> integer
      , ast2astMeta strLit
      , ast2astMeta $ ASTVar <$> identifier
      , listLit
      , ast2astMeta $ try (parens argWithTypeSig)
      , parens exprAst
      , seqAST ]

ast2astMeta parseAst = do
  srcPos <- getSourcePos
  ast <- parseAst
  pure ASTMeta { srcPos = srcPos, ast = ast }

-- 匿名関数を読む
anonFun :: Parser AST
anonFun = do
  params <- some identifier
  code   <- symbol "->" *> getSourcePos
  body   <- exprAst
  sig'   <- optional (symbol ":" *> typeList)
  -- 型を省略した場合はもっとも一般的な型にしちゃう
  let sig = fromMaybe (makeGeneralType (length params)) sig'
  pure $ ASTFunDefAnon { astType      = sig
                       , astFunParams = params
                       , astFunBody   = body }

-- 型を一般的な形にする。例：a -> b であれば t0 -> t1
makeGeneralType :: Int -> RecList Type
makeGeneralType n = Elems (map (\x -> Elem ("t" ++ show x)) [0..n])

-- 型注釈つきの式を読む
astWithTypeSig :: Parser AST
astWithTypeSig = do
  ast' <- exprAst
  symbol ":"
  sig <- typeList
  pure $ ASTTypeSig { astType = sig, astTypeSigVar = ast'}

-- 型注釈つきの項を読む
argWithTypeSig :: Parser AST
argWithTypeSig = do
  arg' <- arg
  symbol ":"
  sig <- typeList
  pure $ ASTTypeSig { astType = sig, astTypeSigVar = arg'}

-- 文字列のリテラルを読む
strLit :: Parser AST
strLit = do
  beginChar <- single '"' <|> single '\''
  str <- many $ noneOf [beginChar]
  _   <- symboln [beginChar]
  pure $ ASTStrLit { astStr = str }

-- リストのリテラルを読む
listLit :: Parser ASTMeta
listLit = do
  srcPos <- getSourcePos
  asts   <- brackets $ exprAst `sepBy` (symbol "," <|> symbol ";")
  pure ASTMeta { srcPos = srcPos
               , ast = ASTListLit { astList = asts } }

-- -- カッコ式（カッコの中に式が一つだけある式）を読む
-- parenAST :: Parser ASTMeta
-- parenAST = do
--   symbol "("
--   e <- ast
--   symboln ")"
--   pure $ Seq [e]

-- 複式（改行で区切られて連続する式）を読む
seqAST :: Parser ASTMeta
seqAST = do
  srcPos <- getSourcePos
  asts   <- braces $ many newLine *> many astNewLine
  pure ASTMeta { srcPos = srcPos
               , ast = ASTSeq { astSeq = asts } }

-- 式を読む。後ろの改行の連続をスキップする
astNewLine :: Parser ASTMeta
astNewLine = exprAst <* many newLine

-- 改行を読む。; も改行扱いとする。
newLine :: Parser String
newLine = symbol "\n" <|> symbol ";"

-- プログラムのトップレベルを読む
topLevel :: Parser ASTMeta
topLevel = do
  srcPos <- getSourcePos
  sc
  asts <- many newLine *> some astNewLine
  pure ASTMeta { srcPos = srcPos
               , ast    = ASTSeq { astSeq = asts }}

-- 型定義を読む
typeDef :: Parser ASTMeta
typeDef = do
  srcPos <- getSourcePos
  rword "type"
  name <- var identifier <* symbol "="
  types <- braces (memberWithType `sepBy1` symbol ",") <* many newLine
  pure ASTMeta { srcPos = srcPos
               , ast = ASTTypeDef { astTypeDefName   = name
                                  , astTypeDefFields = types}}

-- 型定義中の、構造体のメンバーとその型を読む
memberWithType :: Parser (String, RecList Type)
memberWithType = do
  member <- identifier <* symbol ":"
  types  <- typeList
  pure (member, types)

-- 関数定義を読む
fundef :: Parser ASTMeta
fundef = do
  srcPos <- getSourcePos
  rword "fun"
  nameAST <- try (var identifier) <|> var operator
  types'  <- optional (symbol ":" *> typeList) <* symbol "="
  params  <- some identifier <* symbol "->"
  body    <- exprAst
  -- 型を省略した場合はもっとも一般的な型にしちゃう
  let types = fromMaybe (makeGeneralType $ length params) types'
  pure ASTMeta { srcPos = srcPos
               , ast = ASTFunDef { astType       = types
                                 , astFunDefName = nameAST
                                 , astFunParams  = params
                                 , astFunBody    = body }}

-- パターンマッチを含む関数定義を読む
funDefCase :: Parser ASTMeta
funDefCase = do
  srcPos  <- getSourcePos
  rword "fun"
  nameAST <- try (var identifier) <|> var operator
  types'  <- optional (symbol ":" *> typeList) <* symbol "="
  caseAST@ASTMeta
    { ast = ASTCase
            { astType        = types
            , astCaseBranchs = matches }
    }     <- braces $ many newLine *> caseAST types'
  pure $ ASTMeta { srcPos = srcPos
                 , ast = ASTFunDef
                         { astType       = types
                         , astFunDefName = nameAST
                         , astFunParams  = paramList $ paramNum matches
                         , astFunBody    = caseAST
                         } }


paramNum = length . fst3 . head
paramList n = zipWith (++) (replicate n "x") (map show (take n [1..]))
fst3 (a,_,_) = a

caseAST :: Maybe (RecList Type) -> Parser ASTMeta
caseAST maybeType = do
  srcPos  <- getSourcePos
  matches <- some matchAST
  -- 型を省略した場合はもっとも一般的な型にしちゃう
  let types = fromMaybe (makeGeneralType (paramNum matches)) maybeType
  pure ASTMeta { srcPos = srcPos
               , ast    = ASTCase { astType        = types
                                  , astCaseBranchs = matches }}

-- パターンマッチ式を読む
matchAST :: Parser ([ASTMeta], ASTMeta, Maybe ASTMeta)
matchAST = do
  conds <- some arg
  guard <- optional ( symbol "|" *> exprAst <* symbol "|") <* symbol "->"
  body <- exprAst <* many newLine
  pure (conds, body, guard)


-- if式を読む
ifAST :: Parser ASTMeta
ifAST = do
  srcPos <- getSourcePos
  rword "if"
  condAST <- exprAst
  rword "then"
  thenAST <- exprAst
  rword "else"
  elseAST <- exprAst
  pure $ ASTMeta { srcPos = srcPos
                 , ast = ASTIf { astIfCond = condAST
                               , astIfThen = thenAST
                               , astIfElse = elseAST} }

-- 関数適用を読む
apply :: Parser ASTMeta
apply = do
  srcPos <- getSourcePos
  caller <- parens exprAst <|> var identifier
  args   <- some arg
  pure ASTMeta { srcPos = srcPos
               , ast = ASTApply caller args }

-- 型注釈を読む
typeList :: Parser (RecList String)
typeList = do
  term1 <- typeTerm
  terms <- many $ symbol "->" *> typeTerm
  pure $ Elems (term1 : terms)

-- 型を表す項を読む。Int, a, [Double], (Int->String) など。
typeTerm :: Parser (RecList String)
typeTerm = try listTerm
  <|> (Elem <$> identifier)
  <|> parens typeList

-- リスト型を読む
listTerm :: Parser (RecList String)
listTerm = do
  term <- brackets identifier
  pure $ Elems [ Elem "List", Elem term ]

-- -- 現在パース中のコード位置を取得する
-- getCodeMeta :: Parser CodeMeta
-- getCodeMeta = do
--   pos <- getSourcePos
--   pure $ CodeMeta { codeFileName = sourceName pos
--                   , codePos      =
--                     ((sourceLine &&& sourceColumn)
--                       >>> (unPos *** unPos))
--                     pos }
