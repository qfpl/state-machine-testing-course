{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.IORef as R
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

data DrinkType = Coffee | HotChocolate | Tea
newtype Model (v :: * -> *) = Model DrinkType

data SetDrinkCoffee (v :: * -> *) = SetDrinkCoffee deriving Show
data SetDrinkHotChocolate (v :: * -> *) = SetDrinkHotChocolate deriving Show
data SetDrinkTea (v :: * -> *) = SetDrinkTea deriving Show

instance HTraversable SetDrinkCoffee where
  htraverse _ _ = pure SetDrinkCoffee

instance HTraversable SetDrinkHotChocolate where
  htraverse _ _ = pure SetDrinkHotChocolate

instance HTraversable SetDrinkTea where
  htraverse _ _ = pure SetDrinkTea

cSetDrinkCoffee
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cSetDrinkCoffee ref = Command gen exec
  [ Update $ \_ _ _ -> Model Coffee
  , Ensure $ \_ _ _ drink -> case drink of
      C.Coffee{} -> success
      _ -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkCoffee Symbolic))
    gen _ = Just $ pure SetDrinkCoffee

    exec :: SetDrinkCoffee Concrete -> m C.Drink
    exec _ = evalIO $ do
      R.modifyIORef ref C.coffee
      view C.drinkSetting <$> R.readIORef ref

cSetDrinkHotChocolate
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cSetDrinkHotChocolate ref = Command gen exec
  [ Update $ \_ _ _ -> Model HotChocolate
  , Ensure $ \_ _ _ drink -> case drink of
      C.HotChocolate -> success
      _ -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkHotChocolate Symbolic))
    gen _ = Just $ pure SetDrinkHotChocolate

    exec :: SetDrinkHotChocolate Concrete -> m C.Drink
    exec _ = evalIO $ do
      R.modifyIORef ref C.hotChocolate
      view C.drinkSetting <$> R.readIORef ref

cSetDrinkTea
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cSetDrinkTea ref = Command gen exec
  [ Update $ \_ _ _ -> Model Tea
  , Ensure $ \_ _ _ drink -> case drink of
      C.Tea{} -> success
      _ -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkTea Symbolic))
    gen _ = Just $ pure SetDrinkTea

    exec :: SetDrinkTea Concrete -> m C.Drink
    exec _ = evalIO $ do
      R.modifyIORef ref C.tea
      view C.drinkSetting <$> R.readIORef ref

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  r <- evalIO $ R.newIORef C.initialState

  let initialModel = Model HotChocolate
      commands = ($ r) <$>
        [ cSetDrinkCoffee
        , cSetDrinkHotChocolate
        , cSetDrinkTea
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ R.writeIORef r C.initialState
  executeSequential initialModel actions
