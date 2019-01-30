{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module CoffeeMachine where

import Control.Lens (failing, use)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (when)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, execStateT)

data MilkSugar = MilkSugar
  { _milk :: Int
  , _sugar :: Int
  }
  deriving Show

$(makeLenses ''MilkSugar)

data Drink
  = HotChocolate
  | Coffee MilkSugar
  | HipsterCoffee -- ^ Hipster barista disallows adding milk/sugar.
  | Tea MilkSugar
  deriving Show

$(makePrisms ''Drink)

drinkCost :: Drink -> Int
drinkCost HotChocolate = 2
drinkCost (Coffee (MilkSugar m s)) = 4 + m + s
drinkCost HipsterCoffee = 9
drinkCost (Tea (MilkSugar m s)) = 3 + m + s

newtype Mug = Mug (Maybe Drink) deriving Show

data MachineState = MachineState
  { _coins :: Int
  , _drinkSetting :: Drink
  , _mug :: Maybe Mug
  } deriving Show

$(makeLenses ''MachineState)

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
  deriving Show

insertCoins :: Int -> MachineState -> MachineState
insertCoins n = coins %~ (+n)

refund :: MachineState -> (Int, MachineState)
refund = coins <<.~ 0

addMug :: MachineState -> Either MachineError MachineState
addMug st = let oldMug = st ^. mug
            in case oldMug of
                 Nothing -> Right $ st & mug ?~ Mug Nothing
                 Just{} -> Left MugInTheWay

takeMug :: MachineState -> Either MachineError (Mug, MachineState)
takeMug st = let (oldMug, st') = st & mug <<.~ Nothing
             in maybe (Left NoMug) (Right . (,st')) oldMug

hotChocolate :: MachineState -> MachineState
hotChocolate = drinkSetting .~ HotChocolate

coffee :: MachineState -> MachineState
coffee = drinkSetting .~ Coffee (MilkSugar 0 0)

hipsterCoffee :: MachineState -> MachineState
hipsterCoffee = drinkSetting .~ HipsterCoffee

addMilk :: MachineState -> MachineState
addMilk = drinkSetting . (_Coffee `failing` _Tea) . milk +~ 1

addSugar :: MachineState -> MachineState
addSugar = drinkSetting . (_Coffee `failing` _Tea) . sugar +~ 1

tea :: MachineState -> MachineState
tea = drinkSetting .~ Tea (MilkSugar 0 0)

dispense :: MachineState -> Either MachineError MachineState
dispense = runExcept . execStateT go where
  go = do
    drink <- use drinkSetting

    let cost = drinkCost drink
    credit <- use coins
    when (credit < cost) $ throwError NotEnoughCoins

    m <- use mug >>= maybe (throwError NoMug) pure
    case m of
      Mug Just{} -> throwError MugFull
      Mug Nothing -> do
        coins -= cost
        mug .= Just (Mug (Just drink))
    pure ()
