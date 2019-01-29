{-# LANGUAGE OverloadedStrings #-}
module Ed.Memory where

import           Control.Lens           (ix, snoc, (^?))
import           Control.Monad.IO.Class (MonadIO)

import           System.Timeout         (timeout)

import           Data.Bifoldable        (bifold)
import           Data.Bifunctor         (first, second)
import           Data.Foldable          (traverse_)

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Hedgehog

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           Ed.Types
import           Turtle.Format          (d, format, (%))

genLineNum :: MonadGen m => Buffer v -> m Word
genLineNum (Buffer b) = Gen.word (Range.linear 1 (fromIntegral $ length b))

genTextInp :: MonadGen m => m Text
genTextInp = Gen.text (Range.linear 1 100) Gen.alphaNum

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
      ec ",p"
      readPrintedLines edProc
  in
    Command gen execute
    [ Update $ \(Buffer b) (Cmd_Append i) _ ->
        Buffer (snoc b i)

    , Ensure $ \(Buffer old) (Buffer new) (Cmd_Append i) out -> do
        new === snoc old i
        T.unlines new === out
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
    , Ensure $ \(Buffer _old) (Buffer new) _ out ->
        out === T.unlines new
    ]

cDeleteLine
  :: ( MonadIO m
     , MonadTest m
     , MonadGen n
     )
  => EdProc
  -> Command n m Buffer
cDeleteLine edProc =
  let
    gen (Buffer b) = Just $ Cmd_DeleteLine
      <$> Gen.word (Range.linear 1 (fromIntegral $ length b))

    execute (Cmd_DeleteLine n) = evalIO $ do
      edCmd edProc $ format (d%"d") n
      readPrintedLines edProc
  in
    Command gen execute
    [ Require $ \(Buffer b) (Cmd_DeleteLine n) ->
        n >= 0 && n < fromIntegral (length b) && not (null b)

    , Update $ \(Buffer b) (Cmd_DeleteLine n) _ ->
        Buffer
        . bifold
        . first (take (fromIntegral n - 1))
        $ splitAt (fromIntegral n) b

    , Ensure $ \(Buffer old) (Buffer new) (Cmd_DeleteLine n) out -> do
        T.unlines new === out
        let n' = fromIntegral n
        new ^? ix n'       === old ^? ix (n' + 1)
        new ^? ix (n' - 1) === old ^? ix n'
    ]


cAppendAt
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => EdProc
  -> Command n m Buffer
cAppendAt edProc =
  let
    gen b = Just $
      Cmd_AppendAt <$> genLineNum b <*> genTextInp

    execute (Cmd_AppendAt ln i) = evalIO $
      traverse_ (edCmd edProc)
        [ format (d%"a") ln
        , i
        , "."
        ]
      >> readPrintedLines edProc
  in
    Command gen execute
    [ Require $ \(Buffer b) (Cmd_AppendAt ln _) ->
        ln >= 0 && ln < fromIntegral (length b) && not (null b)

    , Update $ \(Buffer b) (Cmd_AppendAt ln i) _ ->
        Buffer
        . bifold
        . second (i:)
        . splitAt (fromIntegral ln) $ b

    , Ensure $ \(Buffer _old) (Buffer new) (Cmd_AppendAt ln i) out -> do
        T.unlines new === out
        -- Can't get here without having perfomed a valid insert, so totes safe, right?...RIGHT!?
        new !! (fromIntegral ln) === i
    ]

edCmd :: EdProc -> Text -> IO ()
edCmd p c = T.hPutStrLn (_edIn p) c

readPrintedLines :: EdProc -> IO Text
readPrintedLines ed = go []
  where
    go acc = do
      ma <- timeout 2500 $ T.hGetLine (_edOut ed)
      case ma of
        Nothing -> pure . T.unlines . reverse $ acc
        Just a  -> go (a:acc)

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

