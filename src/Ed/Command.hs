module Ed.Command where

data Cmd
  = Insert
  | Delete
  | Append
  | Write (Maybe FilePath)
  deriving (Show,Eq)
