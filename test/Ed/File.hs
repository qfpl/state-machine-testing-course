{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ed.File (prop_ed_blackbox_file) where

import           Prelude           hiding (FilePath)

import           Control.Lens      (snoc)

import qualified Data.Text         as T
import qualified Data.Text.IO      as T

import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range

import           Ed.Types          (Buffer (..), Cmd_Append (..),
                                    Cmd_PrintAll (..))

import           Turtle

import           System.Posix.Temp (mkstemp)


resetEdFile :: FilePath -> IO ()
resetEdFile edFile = sh $
  testfile edFile >>= (`when` (rm edFile)) >> touch edFile

edCmdsOnFile :: [Text] -> FilePath -> IO ()
edCmdsOnFile cmds edFile = sh $
  procs "ed" ["-s", format fp edFile] . select . textToLines $ T.unlines cmds

readEdFile :: FilePath -> IO Text
readEdFile edFile = T.readFile (T.unpack $ format fp edFile)

cAppendText
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => FilePath
  -> Command n m Buffer
cAppendText edFile =
  let
    gen _ = Just $ Cmd_Append <$> (Gen.text (Range.linear 1 100) Gen.alphaNum)

    execute (Cmd_Append t) = evalIO $ do
      edCmdsOnFile ["a", t, ".", "w", "q"] edFile
      readEdFile edFile

  in
    Command gen execute
    [ Update $ \(Buffer b) (Cmd_Append i) _ -> Buffer (snoc b i)
    , Ensure $ \_ (Buffer new) _ out        -> T.unlines new === out
    ]

cPrintAll
  :: ( MonadIO m
     , MonadTest m
     , MonadGen n
     )
  => FilePath
  -> Command n m Buffer
cPrintAll edFile =
  let
    gen _ = Just $ Gen.constant Cmd_PrintAll

    execute _ = evalIO $
      readEdFile edFile

  in
    Command gen execute
    [ Require $ \(Buffer cur) _ -> not $ null cur
    , Ensure $ \(Buffer old) (Buffer new) _ _out -> old === new
    , Ensure $ \(Buffer _old) (Buffer new) _ out -> out === T.unlines new
    ]


prop_ed_blackbox_file :: Property
prop_ed_blackbox_file = property $ do
  -- Create our file to apply our ed commands to
  edFile <- evalIO $ decodeString . fst <$> mkstemp "/tmp/ed_test_file_"

  let
    -- Prepare our list of possible commands
    cmds = ($ edFile) <$> [cAppendText, cPrintAll]
    -- Initialise our 'state'
    initialState = Buffer mempty

  -- Generate a sequence of Commands to apply to our state machine
  actions <- forAll $ Gen.sequential (Range.linear 1 10) initialState cmds

  -- Reset the ed buffer
  evalIO $ resetEdFile edFile

  -- Run the tests!
  executeSequential initialState actions

  -- Clean up our temp files
  evalIO . sh $ rm edFile
