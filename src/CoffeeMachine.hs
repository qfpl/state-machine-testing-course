{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module CoffeeMachine
  ( Machine
  , MachineState(..)
  , coins
  , drinkSetting
  , drinkCost
  , mug
  , preferences
  , nextToken

  , Drink(..)
  , _HotChocolate
  , _Coffee
  , _Tea

  , MilkSugar(..)
  , milk
  , sugar

  , MachineError(..)
  , Mug(..)

  , PreferenceToken

  , newMachine
  , reset
  , peek

  , insertCoins
  , refund
  , currentDrinkCost

  , addMug
  , takeMug

  , coffee
  , hotChocolate
  , tea

  , addMilk
  , addSugar

  , savePreferences
  , loadPreferences
  , badPreferenceToken

  , dispense
  ) where

import           Control.Lens (at, failing, use)
import           Control.Lens.Operators
import           Control.Lens.TH (makeLenses, makePrisms)
import           Control.Monad (when)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State (runState)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Tuple (swap)

data MilkSugar = MilkSugar
  { _milk :: Int
  , _sugar :: Int
  }
  deriving (Eq, Show)

$(makeLenses ''MilkSugar)

data Drink
  = HotChocolate
  | Coffee MilkSugar
  | Tea MilkSugar
  deriving (Eq, Show)

$(makePrisms ''Drink)

newtype PreferenceToken = PreferenceToken Int deriving (Eq, Ord, Show)

next :: PreferenceToken -> PreferenceToken
next (PreferenceToken p) = PreferenceToken $ p + 1

drinkCost :: Drink -> Int
drinkCost HotChocolate = 3
drinkCost (Coffee (MilkSugar m s)) = 4 + m + s
drinkCost (Tea (MilkSugar m s)) = 2 + m + s

newtype Mug = Mug (Maybe Drink) deriving (Eq, Show)

data MachineState = MachineState
  { _coins :: Int
  , _drinkSetting :: Drink
  , _mug :: Maybe Mug
  , _preferences :: Map PreferenceToken Drink
  , _nextToken :: PreferenceToken
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
  , _preferences = Map.empty
  , _nextToken = PreferenceToken 0
  }

data MachineError
  = NotEnoughCoins
  | NoMug
  | MugFull
  | MugInTheWay
  | InvalidPreferenceToken
  deriving (Eq, Show)

insertCoins :: MonadIO m => Int -> Machine -> m ()
insertCoins c = update $ coins +~ c

refund :: MonadIO m => Machine -> m Int
refund (Machine ref) = liftIO $ atomicModifyIORef' ref (swap . (coins <<.~ 0))

currentDrinkCost :: MonadIO m => Machine -> m Int
currentDrinkCost = fmap (drinkCost . _drinkSetting) . peek

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

savePreferences :: MonadIO m => Machine -> m PreferenceToken
savePreferences (Machine ref) = liftIO . atomicModifyIORef' ref $ \state ->
  let
    (token, state') = state & nextToken <<%~ next
    state'' = state' & preferences . at token ?~ (state' ^. drinkSetting)
  in
    (state'', token)

loadPreferences
  :: MonadIO m
  => Machine
  -> PreferenceToken
  -> m (Either MachineError ())
loadPreferences (Machine ref) token
  = liftIO . atomicModifyIORef' ref $ \state ->
  case state ^. preferences . at token of
    Nothing -> (state, Left InvalidPreferenceToken)
    Just pref -> (state & drinkSetting .~ pref, Right ())

-- | Return a token that will error when used. Useful for testing.
badPreferenceToken :: MonadIO m => Machine -> m PreferenceToken
badPreferenceToken (Machine ref) = liftIO . atomicModifyIORef' ref $ \state ->
  swap $ state & nextToken <<%~ next

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
