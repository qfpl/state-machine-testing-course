{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind (Type)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea
newtype Model (v :: Type -> Type) = Model DrinkType

data SetDrinkCoffee (v :: Type -> Type) = SetDrinkCoffee deriving Show
data SetDrinkHotChocolate (v :: Type -> Type) = SetDrinkHotChocolate deriving Show
data SetDrinkTea (v :: Type -> Type) = SetDrinkTea deriving Show

instance HTraversable SetDrinkCoffee where
  htraverse _ _ = pure SetDrinkCoffee

instance HTraversable SetDrinkHotChocolate where
  htraverse _ _ = pure SetDrinkHotChocolate

instance HTraversable SetDrinkTea where
  htraverse _ _ = pure SetDrinkTea

cSetDrinkCoffee
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkCoffee mach = Command gen exec
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
      C.coffee mach
      view C.drinkSetting <$> C.peek mach

cSetDrinkHotChocolate
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkHotChocolate mach = Command gen exec
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
      C.hotChocolate mach
      view C.drinkSetting <$> C.peek mach

cSetDrinkTea
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkTea mach = Command gen exec
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
      C.tea mach
      view C.drinkSetting <$> C.peek mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate
      commands = ($ mach) <$>
        [ cSetDrinkCoffee
        , cSetDrinkHotChocolate
        , cSetDrinkTea
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ C.reset mach
  executeSequential initialModel actions
