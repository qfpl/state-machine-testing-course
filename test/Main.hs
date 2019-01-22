{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hedgehog

import           System.Exit    (exitFailure, exitSuccess)
import           System.IO      (BufferMode (..), hSetBuffering)

import qualified System.Process as P

import qualified Ed.File        as EF
import qualified Ed.Memory      as EM
import           Ed.Types

main1 :: IO ()
main1 = do
  let p = (P.shell "ed") { P.std_in = P.CreatePipe, P.std_out = P.CreatePipe }
  b <- P.withCreateProcess p $ \(Just stdin) (Just stdout) _ ph -> do

    hSetBuffering stdout LineBuffering

    let edProc = EdProc stdin stdout ph

    EM.edCmd edProc "H"

    b <- checkSequential $ Group "ed (real deal)"
       [ ("property_a_p", EM.prop_ed_blackbox_memory edProc)
       ]

    EM.edCmd edProc "Q"

    pure b

  if b then exitSuccess else exitFailure

main2 :: IO Bool
main2 = checkSequential $ Group "Ed Black-Box State Machine Tests"
  [ ("Cmds: [(.)a,a,(.)d,p]", EF.prop_ed_blackbox_file)
  ]

main :: IO Bool
main = main2
