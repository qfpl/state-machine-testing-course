{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType
  = Coffee
  | HotChocolate
  | Tea
  deriving (Enum, Bounded, Eq, Show)

data Model (v :: Type -> Type) = Model DrinkType Bool

data AddMug (v :: Type -> Type) = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show

-- Replace these with one SetDrinkType data type whose constructor
-- takes the type of drink to select as an argument. Don't forget to
-- add a HTraversable instance.

data SetDrinkCoffee (v :: Type -> Type) = SetDrinkCoffee deriving Show
data SetDrinkHotChocolate (v :: Type -> Type) = SetDrinkHotChocolate deriving Show
data SetDrinkTea (v :: Type -> Type) = SetDrinkTea deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

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
  [ Update $ \(Model _ hasMug) _ _ -> Model Coffee hasMug
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
  [ Update $ \(Model _ hasMug) _ _ -> Model HotChocolate hasMug
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
  [ Update $ \(Model _ hasMug) _ _ -> Model Tea hasMug
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

-- Replace the three command definitions above with one cSetDrinkType
-- command definition below, which will the SetDrinkType data type you
-- defined above.

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkType = undefined

-- Fill in these command definitions to take and replace the mug in
-- the machine. Use the TakeMug and AddMug types you defined
-- previously.
--
-- You should only return a generator when it makes sense: taking a
-- mug when it's in the machine, or adding a mug when the machine is
-- empty. You will also need `Require` callbacks that enforce this.
--
-- You should also write an `Ensure` callback that verifies that your
-- action worked.

cTakeMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMug = undefined

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug = undefined

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False
      commands = ($ mach) <$>
        [ cSetDrinkCoffee
        , cSetDrinkHotChocolate
        , cSetDrinkTea
        -- You will need to replace the above commands, or your
        -- cSetDrinkType will never run.
        , cTakeMug
        , cAddMug
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ C.reset mach
  executeSequential initialModel actions
