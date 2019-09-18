-- 環境
module Env where

import           Control.Monad.State
import           Data.IORef
import           Data.Map            as Map
import           Debug.Trace
import           RecList
import           TypeUtil

type GeneralEnv a = -- IORef
  (Map String [(RecList String, a)])

-- 与えられた名前の変数を探す
lookupVar :: (Show a) => String -> State (GeneralEnv a) (Maybe a)
lookupVar name = lookupVar' name False

-- 与えられた名前の変数を探す。ただし変数として名前がなくても関数として一つだけあるならそれを返す
lookupVarLoose :: (Show a) => String -> State (GeneralEnv a) (Maybe a)
lookupVarLoose name = lookupVar' name True

-- 与えられた名前の変数を探す。上2つを統合した関数。
lookupVar' :: (Show a) => String -> Bool -> State (GeneralEnv a) (Maybe a)
lookupVar' name loose = do
  env' <- get
  pure $ do
    vars <- env' !? name
    case vars of
      -- 同じ名前の定義の先頭に変数があればそれを参照する
      (Elem "_", expr):es -> Just expr
      -- 変数として名前がなくても関数として1つだけ名前があるならそれを参照する
      [(Elems _, expr)]   -> if loose then Just expr else Nothing
      -- そうでなければなし
      _                   -> Nothing

-- 関数を環境に登録する
-- 同名かつ同型の関数がない場合のみ登録できる
insertFun :: (Show a) => String -> RecList String -> a
          -> State (GeneralEnv a) (Either String ())
insertFun name types expr = do
  fe <- funExists name types
  if not fe
  then do
    insertFun' name types expr
    pure $ Right ()
  else
    pure $ Left ("function '" ++ name ++ " : " ++ argSig types ++ "' already exists")

-- 同名の関数がある場合は、具体型の関数は先頭に、多相型の関数は末尾に追加する
insertFun' :: String -> RecList String -> a -> State (GeneralEnv a) ()
insertFun' name types expr = do
  env <- get
  funs' <- case env !? name of
    Nothing   -> pure []
    Just funs -> pure funs
  let generalizedTypes = generalizeTypes types
  put (Map.insert name (if types == generalizedTypes
                         then (generalizedTypes, expr):funs'
                         else funs' ++ [(generalizedTypes, expr)])
        env)

-- 与えられた名前と引数の型を持つ関数が存在するか？
funExists :: (Show a) => String -> RecList String -> State (GeneralEnv a) Bool
funExists name types = do
  fun <- lookupFun name types True
  case fun of
    Nothing -> pure False
    Just _  -> pure True


-- 与えられた名前と引数の型を持つ関数を探す
lookupFun :: (Show a) => String -> RecList String -> Bool
          -> State (GeneralEnv a) (Maybe a)
lookupFun name types strict = do
  --trace ("lookupFun: name=" ++ name) $ pure True
  env <- get
  pure $ do
    funs <- env !? name
    if hasVariable types
      then lastMatch (generalizeTypesWith "x" types) funs strict
      else firstMatch (generalizeTypesWith "x" types) funs strict

-- 環境を先頭から（具体型を持つほうから）探して、最初にマッチした関数を返す
firstMatch :: (Show a) => RecList String -> [(RecList String, a)] -> Bool -> Maybe a
firstMatch types [] strict = Nothing
firstMatch types ((types', expr):es) strict =
  if strict
    then if types' == types then Just expr else firstMatch types es strict
    else case unify (rInit types') types mempty of
      Nothing  -> firstMatch types es strict
      Just env -> Just expr

-- 環境を後ろから（抽象型を持つほうから）探して、最初にマッチした関数を返す
lastMatch :: (Show a) => RecList String -> [(RecList String, a)] -> Bool -> Maybe a
lastMatch types funs strict = firstMatch types (reverse funs) strict

-- -- 変数を環境に登録する
-- -- 同名の変数がない場合のみ登録できる
-- -- 同名の関数はあってもいい
-- insertVar :: (Show a) => String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
-- insertVar name expr env = do
--   e <- varExists name env
--   if not e
--     then insertVarForce name expr env
--     else pure $ Left ("variable '" ++ name ++ "' already exists")

-- -- 変数を環境に強制的に登録する
-- insertVarForce :: String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
-- insertVarForce name expr env = do
--   env' <- readIORef env
--   funs' <- case env' !? name of
--     Nothing   -> pure []
--     Just funs -> pure funs
--   writeIORef env (Map.insert name ((Elem "_", expr) : funs') env')
--   pure $ Right env

-- -- 与えられた名前を持つ変数が存在するか？
-- varExists :: (Show a) => String -> GeneralEnv a -> IO Bool
-- varExists name env = do
--   exists <- lookupVar name env
--   case exists of
--     Nothing   -> pure False
--     otherwise -> pure True

-- -- 与えられた名前の変数、または関数が存在するか？
-- anyExists :: String -> GeneralEnv a -> IO Bool
-- anyExists name env = do
--   env' <- readIORef env
--   case env' !? name of
--     Nothing   -> pure False
--     Just vars -> pure True

-- -- 環境の中身を表示する
-- showEnv :: (Show a) => GeneralEnv a -> IO ()
-- showEnv env = do
--   env' <- readIORef env
--   print env'
