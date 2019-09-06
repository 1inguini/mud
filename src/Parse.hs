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
import           Text.Megaparsec.Debug

import           Expr
import           RecList

type Parser = Parsec Void String

-- -- space consumer 。空白やコメントをスキップする。改行はスキップしない。
-- sc :: Parser ()
-- sc = L.space spaceOrTab1 lineCmnt blockCmnt
--   where
--     spaceOrTab1 = void $ takeWhile1P (Just "white space") (\c -> c == ' ' || c == '\t')
--     lineCmnt  = L.skipLineComment "#"
--     blockCmnt = L.skipBlockComment "/*" "*/"

-- 改行を含む空白やコメントをスキップする
scn :: Parser ()
scn = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- -- 与えられた文字列を読む。後ろの空白（改行を含まない）をスキップする。
-- symboln :: String -> Parser String
-- symboln = L.symbol sc

-- 与えられた文字列を読む。後ろの空白（改行を含む）をスキップする。
symbol :: String -> Parser String
symbol = L.symbol scn

-- 整数を読む
integer :: Parser Integer
integer = lexeme L.decimal

-- 浮動小数点数を読む
double :: Parser Double
double = lexeme L.float

-- 演算子を読む
operator :: Parser String
operator = lexeme $ some (oneOf opChars)

opChars :: [Char]
opChars = "+-*/><"

-- 予約語を読む
rword :: String -> Parser ()
rword w = lexeme (chunk w *> notFollowedBy alphaNumChar)

-- 予約語のリスト
reservedWords :: [String] -- list of reserved words
reservedWords = ["fun","if","then","else","type"]

-- 識別子を読む
identifier :: Parser String
identifier = lowerChar >>= identifier'


-- 識別子を読む
constrIdent :: Parser String
constrIdent = upperChar >>= identifier'


identifier' :: Char -> Parser String
identifier' firstLetter = lexeme $ check =<< do
  middleLetters <- many (alphaNumChar <|> single '_')
  lastLetters   <- many (oneOf "!?_'")
  pure $ firstLetter:middleLetters ++ lastLetters
    where
      check x = if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else pure x


var :: Parser String -> Parser ASTMeta
var p = meta $ do
  str <- p
  pure ASTVar { astVar = str }

-- カッコで挟まれる表現を読む
parens, braces, angles, brackets, dubquotes, quotes :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
dubquotes = between (symbol "\"") (symbol "\"")
quotes    = between (symbol "'") (symbol "'")

lineSep = some $ symbol ";"

-- 型を一般的な形にする。例：a -> b であれば t0 -> t1
makeGeneralType :: Int -> RecList Type
makeGeneralType n = Elems (map (\x -> Elem ("t" ++ show x)) [0..n])

meta :: Parser AST -> Parser ASTMeta
meta parseAst = do
  astSrcPos <- getSourcePos
  ast <- parseAst
  pure ASTMeta { astSrcPos = astSrcPos, ast = ast }

-- プログラムのトップレベルを読む
topLevels :: Parser ASTMeta
topLevels =  dbg "topLevels" $
  meta $ do
  asts <- some topLevel
  pure ASTSeq { astSeq = asts }

topLevel =  dbg "topLevel" $
  scn *>
  choice [ try typeDef
         , try funDef
         , exprAst <* lineSep
         ]

-- 型定義を読む
typeDef :: Parser ASTMeta
typeDef =  dbg "typeDef" $
  meta $ do
  rword "type"
  name  <- var constrIdent <* symbol "="
  types <- braces (memberWithType `sepBy1` symbol ",")
  pure ASTTypeDef { astTypeDefName   = name
                  , astTypeDefFields = types}
    where
      -- 型定義中の、構造体のメンバーとその型を読む
      memberWithType :: Parser (String, RecList Type)
      memberWithType =  dbg "memberWithType" $
        do{
          ; member <- identifier <* symbol ":"
          ; types  <- typeList
          ; pure (member, types) }

-- パターンマッチを含む関数定義を読む
funDef :: Parser ASTMeta
funDef =  dbg "funDef" $
  meta $ do
  rword "fun"
  nameAST <- try (var identifier) <|> var operator
  types'  <- optional (symbol ":" *> typeList) <* symbol "="
  caseAST@ASTMeta
    { ast = ASTAnonFun
            { astType        = types
            , astCaseBranchs = matches }
    }     <- braces (caseAST True types')
             <|> caseAST False types' <* lineSep
  pure ASTFunDef { astType       = types
                 , astFunDefName = nameAST
                 , astFunParams  = paramList $ paramNum matches
                 , astFunBody    = caseAST }


caseAST :: Bool -> Maybe (RecList Type) -> Parser ASTMeta
caseAST isMany maybeType =  dbg "caseAST" $
  meta $ do
  matches <- if isMany
             then matchAST `endBy1` lineSep
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
          ; guard <- optional ( symbol "|" *> exprAst <* symbol "|")
          ; body  <- symbol "->" *> exprAst
          ; pure (conds, body, guard) }


paramNum = length . fst3 . head
paramList n = zipWith (++) (replicate n "x") (map show (take n [1..]))
fst3 (a,_,_) = a

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
          ASTMeta { astSrcPos = meta
                  , ast       = astUnary astMeta })


genBinOp4OpTable str = do
  meta     <- getSourcePos
  astBinOp <- ASTBinOp (OpLit str) <$ symbol str
  pure (\arg0 arg1 ->
           ASTMeta { astSrcPos = meta
                   , ast       = astBinOp arg0 arg1 })


-- 式を読む
exprAst :: Parser ASTMeta
exprAst =  dbg "exprAst" $
   makeExprParser term ops

-- 項を読む。項は演算子の引数になるもの。
term :: Parser ASTMeta
term =  dbg "term" $
  choice [ try $ parens (try astWithTypeSig <|> exprAst)
         , seqAST
         , try ifAST
         , ptn ]

-- -- パターンマッチになりうるものを読む
ptn :: Parser ASTMeta
ptn =  dbg "ptn" $
  choice [ meta $ ASTDouble <$> try double
         , meta $ ASTInt <$> integer
         , try apply
         -- , var identifier
         -- , var constrIdent
         , listLit
         , strLit
         ]

-- 匿名関数を読む
anonFun :: Parser ASTMeta
anonFun =  dbg "anonFun" $
  meta $ parens $ do
  ASTMeta
    { ast = fun@ASTAnonFun { astType = types }
    }  <- parens (caseAST True Nothing)
          <|> caseAST False Nothing
  sig' <- optional (symbol ":" *> typeList)
  -- 型を省略した場合はもっとも一般的な型にしちゃう
  let sig = fromMaybe types sig'
  pure $ fun { astType = sig }


-- 型注釈つきの式を読む
astWithTypeSig :: Parser ASTMeta
astWithTypeSig =  dbg "astWithTypeSig" $
  meta $ do
  ast' <- exprAst
  sig  <- symbol ":" *> typeList
  pure $ ASTTypeSig { astType = sig, astTypeSigVar = ast'}

-- -- 型注釈つきの項を読む
-- argWithTypeSig :: Parser ASTMeta
-- argWithTypeSig =  dbg "argWithTypeSig" $
--   meta $ do
--   arg' <- term
--   sig  <- symbol ":" *> typeList
--   pure $ ASTTypeSig { astType = sig, astTypeSigVar = arg'}

-- 文字列のリテラルを読む
strLit :: Parser ASTMeta
strLit =  dbg "strLit" $
  meta $ do
  beginChar <- single '"' <|> single '\''
  str <- many $ noneOf [beginChar]
  _   <- symbol [beginChar]
  pure $ ASTStr { astStr = str }

-- リストのリテラルを読む
listLit :: Parser ASTMeta
listLit =  dbg "listLit" $
  meta $ do
  asts <- brackets $ exprAst `sepBy` (symbol "," <|> symbol ";")
  pure ASTList { astList = asts }

-- 複式（";"で区切られて連続する式）を読む
seqAST :: Parser ASTMeta
seqAST =  dbg "seqAST" $
  meta $ do
  asts <- braces (exprAst `endBy1` lineSep)
  pure ASTSeq { astSeq = asts }

-- -- -- 式を読む。後ろの改行の連続をスキップする
-- -- astNewLine :: Parser ASTMeta
-- -- astNewLine = exprAst <* some newLine

-- -- -- 改行を読む。; も改行扱いとする。
-- -- newLine :: Parser String
-- -- newLine = symbol "\n" <|> symbol ";"


-- if式を読む
ifAST :: Parser ASTMeta
ifAST =  dbg "ifAST" $
  meta $ do
  condAST <- rword "if" *> exprAst <* rword "then"
  -- rword "then"
  thenAST <- exprAst <* rword "else"
  -- rword "else"
  elseAST <- exprAst
  pure ASTIf { astIfCond = condAST
             , astIfThen = thenAST
             , astIfElse = elseAST }

-- 関数適用を読む
apply :: Parser ASTMeta
apply =  dbg "apply" $
  meta $ do
  caller <- anonFun <|> var constrIdent <|> var identifier
  args   <- many term
  pure ASTApply { astApplyFun  = caller
                , astApplyArgs = args}

-- 型注釈を読む
typeList :: Parser (RecList Type)
typeList =  dbg "typeList" $
  do
  term1 <- typeTerm
  terms <- many $ symbol "->" *> typeTerm
  pure $ Elems (term1 : terms)

-- 型を表す項を読む。Int, a, [Double], (Int->String) など。
typeTerm :: Parser (RecList Type)
typeTerm =  dbg "typeTerm" $
  try listTerm
  <|> (Elem <$> constrIdent)
  <|> parens typeList

-- リスト型を読む
listTerm :: Parser (RecList Type)
listTerm =  dbg "listTerm" $
  do
  term <- brackets constrIdent
  pure $ Elems [ Elem "List", Elem term ]
