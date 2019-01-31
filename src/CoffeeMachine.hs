{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module CoffeeMachine
  ( Machine
  , MachineState(..)
  , coins
  , drinkSetting
  , mug

  , Drink(..)
  , _HotChocolate
  , _Coffee
  , _Tea

  , MilkSugar(..)
  , milk
  , sugar

  , MachineError(..)
  , Mug(..)

  , newMachine
  , reset
  , peek

  , insertCoins
  , refund

  , addMug
  , takeMug

  , coffee
  , hotChocolate
  , tea

  , addMilk
  , addSugar

  , dispense
  ) where

import Control.Lens (failing, use)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (runState)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Tuple (swap)

data MilkSugar = MilkSugar
  { _milk :: Int
  , _sugar :: Int
  }
  deriving Show

$(makeLenses ''MilkSugar)

data Drink
  = HotChocolate
  | Coffee MilkSugar
  | Tea MilkSugar
  deriving Show

$(makePrisms ''Drink)

drinkCost :: Drink -> Int
drinkCost HotChocolate = 3
drinkCost (Coffee (MilkSugar m s)) = 4 + m + s
drinkCost (Tea (MilkSugar m s)) = 2 + m + s

newtype Mug = Mug (Maybe Drink) deriving Show

data MachineState = MachineState
  { _coins :: Int
  , _drinkSetting :: Drink
  , _mug :: Maybe Mug
  } deriving Show

$(makeLenses ''MachineState)

newtype Machine = Machine (IORef MachineState)

newMachine :: MonadIO m => m Machine
newMachine = liftIO . fmap Machine $ newIORef initialState

reset :: MonadIO m => Machine -> m ()
reset = update (const initialState)

peek :: MonadIO m => Machine -> m MachineState
peek (Machine m) = liftIO $ readIORef m

initialState :: MachineState
initialState = MachineState
  { _coins = 0
  , _drinkSetting = HotChocolate
  , _mug = Nothing
  }

data MachineError
  = NotEnoughCoins
  | NoMug
  | MugFull
  | MugInTheWay
  deriving (Eq, Show)

insertCoins :: MonadIO m => Int -> Machine -> m ()
insertCoins c = update $ coins +~ c

refund :: MonadIO m => Machine -> m Int
refund (Machine ref) = liftIO $ atomicModifyIORef' ref (swap . (coins <<.~ 0))

addMug :: MonadIO m => Machine -> m (Either MachineError ())
addMug (Machine ref) = liftIO . atomicModifyIORef' ref $ \st ->
  let oldMug = st ^. mug
  in case oldMug of
       Nothing -> (st & mug ?~ Mug Nothing, Right ())
       Just{} -> (st, Left MugInTheWay)

takeMug :: MonadIO m => Machine -> m (Either MachineError Mug)
takeMug (Machine ref) = liftIO . atomicModifyIORef' ref $ \st ->
  let (mMug, st') = st & mug <<.~ Nothing
  in case mMug of
       Nothing -> (st, Left NoMug)
       Just m -> (st', Right m)

coffee :: MonadIO m => Machine -> m ()
coffee = update $ drinkSetting .~ Coffee (MilkSugar 0 0)

hotChocolate :: MonadIO m => Machine -> m ()
hotChocolate = update $ drinkSetting .~ HotChocolate

tea :: MonadIO m => Machine -> m ()
tea = update $ drinkSetting .~ Tea (MilkSugar 0 0)

addMilk :: MonadIO m => Machine -> m ()
addMilk = update $ drinkSetting . (_Coffee `failing` _Tea) . milk +~ 1

addSugar :: MonadIO m => Machine -> m ()
addSugar = update $ drinkSetting . (_Coffee `failing` _Tea) . sugar +~ 1

update :: MonadIO m => (MachineState -> MachineState) -> Machine -> m ()
update f (Machine ref) = liftIO $ atomicModifyIORef' ref ((,()) . f)

dispense :: MonadIO m => Machine -> m (Either MachineError ())
dispense (Machine ref) = liftIO . atomicModifyIORef' ref $
  swap . runState (runExceptT go)
  where
    go = do
      drink <- use drinkSetting

      let cost = drinkCost drink
      credit <- use coins
      when (credit < cost) $ throwError NotEnoughCoins

      mMug <- use mug >>= maybe (throwError NoMug) pure
      case mMug of
        Mug Just{} -> throwError MugFull
        Mug Nothing -> do
          coins -= cost
          mug .= Just (Mug (Just drink))
      pure ()
