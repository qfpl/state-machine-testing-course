{-# LANGUAGE OverloadedStrings #-}
module Ed.Memory
    ( prop_ed_blackbox_memory
    , edCmd
    ) where

import           Control.Lens           (snoc)
import           Control.Monad.IO.Class (MonadIO)
import           System.IO              (hFlush, hGetLine)

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           Ed.Types

cAppendText
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => EdProc
  -> Command n m Buffer
cAppendText edProc =
  let
    gen _ = Just $ Cmd_Append <$> (Gen.text (Range.linear 1 100) Gen.alphaNum)

    execute (Cmd_Append t) = evalIO $ do
      let ec = edCmd edProc
      ec "a"
      ec t
      ec "."
  in
    Command gen execute
    [ Update $ \(Buffer b) (Cmd_Append i) _ ->
        Buffer (snoc b i)

    , Ensure $ \(Buffer old) (Buffer new) (Cmd_Append i) _ ->
        new === snoc old i
    ]

cPrintAll
  :: ( MonadIO m
     , MonadTest m
     , MonadGen n
     )
  => EdProc
  -> Command n m Buffer
cPrintAll edProc =
  let
    gen _ = Just $ Gen.constant Cmd_PrintAll

    execute _ = evalIO $ do
      edCmd edProc ",p"
      readPrintedLines edProc
  in
    Command gen execute
    [ Require $ \(Buffer cur) _ -> not $ null cur
    , Ensure $ \(Buffer old) (Buffer new) _ _out -> old === new
    ]

edCmd :: EdProc -> Text -> IO ()
edCmd p c = T.hPutStrLn (_edIn p) c >> hFlush (_edIn p)

readPrintedLines :: EdProc -> IO Text
readPrintedLines ed = T.pack <$> hGetLine (_edOut ed)

prop_ed_blackbox_memory :: EdProc -> Property
prop_ed_blackbox_memory edProc = property $ do
  let
    cmds = ($ edProc) <$> [cAppendText, cPrintAll]
    initialState = Buffer mempty

  actions <- forAll $ Gen.sequential (Range.linear 1 10) initialState cmds

  -- Reset the ed buffer
  evalIO $ do
    edCmd edProc "a"
    edCmd edProc "avoiding invalid address error :/"
    edCmd edProc "."
    edCmd edProc ",d"

  executeSequential initialState actions

