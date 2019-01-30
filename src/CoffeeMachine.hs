{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoffeeMachine where

import Control.Lens (failing, use)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (when)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, evalStateT)

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

data MachineError
  = NotEnoughCoins
  | NoMug
  | MugFull
  | MugInTheWay
  deriving Show

newtype Machine a = Machine (StateT MachineState (Except MachineError) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError MachineError
           , MonadState MachineState
           )

runMachine :: Machine a -> Either MachineError a
runMachine (Machine m) = runExcept . evalStateT m $ MachineState
  { _coins = 0
  , _drinkSetting = HotChocolate
  , _mug = Nothing
  }

insertCoins :: Int -> Machine ()
insertCoins n = coins += n

refund :: Machine Int
refund = coins <<.= 0

addMug :: Machine ()
addMug = do
  oldMug <- use mug
  case oldMug of
    Nothing -> mug .= Just (Mug Nothing)
    Just{} -> throwError MugInTheWay

takeMug :: Machine Mug
takeMug = do
  oldMug <- mug <<.= Nothing
  maybe (throwError NoMug) pure oldMug

hotChocolate :: Machine ()
hotChocolate = drinkSetting .= HotChocolate

coffee :: Machine ()
coffee = drinkSetting .= Coffee (MilkSugar 0 0)

hipsterCoffee :: Machine ()
hipsterCoffee = drinkSetting .= HipsterCoffee

addMilk :: Machine ()
addMilk = drinkSetting . (_Coffee `failing` _Tea) . milk += 1

addSugar :: Machine ()
addSugar = drinkSetting . (_Coffee `failing` _Tea) . sugar += 1

tea :: Machine ()
tea = drinkSetting .= Tea (MilkSugar 0 0)

dispense :: Machine ()
dispense = do
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
