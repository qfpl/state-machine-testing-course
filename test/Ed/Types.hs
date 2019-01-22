{-# LANGUAGE KindSignatures #-}
module Ed.Types where

import           Hedgehog       (HTraversable (..))

import           System.IO      (Handle)
import qualified System.Process as P

import           Data.Text      (Text)

newtype Buffer (v :: * -> *) =
  Buffer [Text]
  deriving (Show, Eq)

data Cmd_PrintAll (v :: * -> *) =
  Cmd_PrintAll
  deriving (Show, Eq)

instance HTraversable Cmd_PrintAll where
  htraverse _ _ = pure Cmd_PrintAll

newtype Cmd_Append (v :: * -> *) =
  Cmd_Append Text
  deriving (Show, Eq)

instance HTraversable Cmd_Append where
  htraverse _ (Cmd_Append t) = pure (Cmd_Append t)

data EdProc = EdProc
  { _edIn  :: Handle
  , _edOut :: Handle
  , _edPh  :: P.ProcessHandle
  }
