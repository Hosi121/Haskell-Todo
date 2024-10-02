module Server where

import Servant
import Api
import Todo (todos, getTodo, createTodo, updateTodo, deleteTodo)

-- サーバーのエンドポイントとロジックを結びつける
server :: Server TodoAPI
server = todos
    :<|> getTodo
    :<|> createTodo
    :<|> updateTodo
    :<|> deleteTodo

-- アプリケーションを返す
app :: Application
app = serve (Proxy :: Proxy TodoAPI) server

