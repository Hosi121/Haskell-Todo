{-# LANGUAGE DeriveGeneric #-}

module Todo where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Data.Text (Text)

-- Todoの型定義
type TodoId = Int

data Todo = Todo
  { todoId :: TodoId
  , title  :: Text
  , done   :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

-- 新規Todo作成用のリクエストボディ
data NewTodo = NewTodo
  { newTitle :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON NewTodo
instance FromJSON NewTodo

-- Todo更新用のリクエストボディ
data UpdateTodo = UpdateTodo
  { updateTitle :: Maybe Text
  , updateDone  :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateTodo
instance FromJSON UpdateTodo

-- シンプルなTodoデータのリスト（実際にはDBで管理）
todoList :: [Todo]
todoList = [ Todo 1 "Learn Haskell" False
           , Todo 2 "Build a REST API" True
           ]

-- Todoリストを取得
todos :: Handler [Todo]
todos = return todoList

-- 特定のTodoを取得
getTodo :: TodoId -> Handler Todo
getTodo tid = case filter (\t -> todoId t == tid) todoList of
    (t:_) -> return t
    _     -> throwError err404 { errBody = "Todo not found" }

-- 新規Todoを作成
createTodo :: NewTodo -> Handler Todo
createTodo newTodo = do
    let newId = length todoList + 1
    let todo = Todo newId (newTitle newTodo) False
    return todo

-- Todoを更新
updateTodo :: TodoId -> UpdateTodo -> Handler Todo
updateTodo tid update = case filter (\t -> todoId t == tid) todoList of
    (t:_) -> return t { title = maybe (title t) id (updateTitle update)
                      , done = maybe (done t) id (updateDone update)
                      }
    _     -> throwError err404 { errBody = "Todo not found" }

-- Todoを削除
deleteTodo :: TodoId -> Handler ()
deleteTodo tid = return ()  -- 実際には削除処理が必要

