{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant
import Todo (Todo, TodoId, NewTodo, UpdateTodo)

-- Todo用APIの型定義
type TodoAPI =
       "todos" :> Get '[JSON] [Todo]                       -- Todoの一覧を取得
  :<|> "todos" :> Capture "id" TodoId :> Get '[JSON] Todo  -- 特定のTodoを取得
  :<|> "todos" :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo  -- 新規Todoを作成
  :<|> "todos" :> Capture "id" TodoId :> ReqBody '[JSON] UpdateTodo :> Put '[JSON] Todo  -- Todoを更新
  :<|> "todos" :> Capture "id" TodoId :> Delete '[JSON] ()  -- Todoを削除

