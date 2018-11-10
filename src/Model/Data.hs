module Model.Data where

data Position = Position { x :: Int, y :: Int } deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)