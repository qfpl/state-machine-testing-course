{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class     (MonadIO, liftIO)

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (Handle, hClose, hFlush, hGetLine,
                                             hPutStrLn)
import qualified System.Process.Typed       as TP

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

newtype Buffer (v :: * -> *) =
  Buffer ByteString
  deriving (Show, Eq)

data Cmd_PrintAll (v :: * -> *) =
  Cmd_PrintAll
  deriving (Show, Eq)

instance HTraversable Cmd_PrintAll where
  htraverse _ _ = pure Cmd_PrintAll

newtype Cmd_Append (v :: * -> *) =
  Cmd_Append ByteString
  deriving (Show, Eq)

instance HTraversable Cmd_Append where
  htraverse _ (Cmd_Append t) = pure (Cmd_Append t)

cAppendText
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => TP.Process Handle Handle ()
  -> Command n m Buffer
cAppendText edProc =
  let
    gen _ = Just $ Cmd_Append . BS.pack <$> Gen.string (Range.linear 0 100) Gen.ascii

    execute (Cmd_Append t) = evalIO $ do
      let ec = edCmd edProc
      ec "a"
      ec t
      ec "."
      ec ",p"
      BS.pack <$> hGetLine (TP.getStdout edProc)
  in
    Command gen execute
    [ Update $ \(Buffer b) (Cmd_Append i) _out ->
        Buffer (BS.unlines [b,i])

    , Ensure $ \(Buffer old) (Buffer new) (Cmd_Append t) out -> do
        new === out
    ]

cPrintAll
  :: ( MonadIO m
     , MonadTest m
     , MonadGen n
     )
  => TP.Process Handle Handle ()
  -> Command n m Buffer
cPrintAll edProc =
  let
    gen _ = Just $ Gen.constant Cmd_PrintAll

    execute _ = evalIO $ do
      edCmd edProc ",p"
      BS.pack <$> hGetLine (TP.getStdout edProc)
  in
    Command gen execute
    [ Ensure $ \(Buffer old) (Buffer new) _ _   -> old === new
    , Ensure $ \(Buffer old) _            _ out -> old === out
    ]

edProcConfig =
  TP.setStdin TP.createPipe
  $ TP.setStdout TP.createPipe
  $ TP.setStderr TP.closed "ed"

edCmd p c =
  BS.hPutStrLn (TP.getStdin p) c
  >> hFlush (TP.getStdin p)

prop_ap :: TP.Process Handle Handle () -> Property
prop_ap edProc = withTests 5 . property $ do
  let
    cmds = ($ edProc) <$> [cAppendText, cPrintAll]
    initialState = Buffer mempty

  actions <- forAll $ Gen.sequential (Range.linear 1 10) initialState cmds
  executeSequential initialState actions

main :: IO ()
main = do
  TP.withProcess edProcConfig $ \edProc -> do
    b <- checkSequential $ Group "ed (real deal)"
         [ ("property_a_p", prop_ap edProc)
         ]
    hClose (TP.getStdin edProc)

    if b then exitSuccess else exitFailure
