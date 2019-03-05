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

data DrinkType = Coffee | HotChocolate | Tea

data Model (v :: Type -> Type) = Model DrinkType Bool

data AddMug (v :: Type -> Type) = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show
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

cTakeMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMug mach = Command gen exec
  [ Require $ \(Model _ hasMug) _ -> hasMug
  , Update $ \(Model dt _) _ _ -> Model dt False
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen (Model _ hasMug) | hasMug    = (pure . pure) TakeMug
                         | otherwise = Nothing

    exec :: TakeMug Concrete -> m C.Mug
    exec _ = evalIO (C.takeMug mach) >>= evalEither

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug mach = Command gen exec
  [ Require $ \(Model _ hasMug) _ -> not hasMug
  , Update $ \(Model dt _) _ _ -> Model dt True
  , Ensure $ \_ _ _ -> assert . isJust
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen (Model _ hasMug) | not hasMug = (pure . pure) AddMug
                         | otherwise  = Nothing

    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      evalIO (C.addMug mach) >>= evalEither
      evalIO $ view C.mug <$> C.peek mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False
      commands = ($ mach) <$>
        [ cSetDrinkCoffee
        , cSetDrinkHotChocolate
        , cSetDrinkTea
        , cTakeMug
        , cAddMug
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ C.reset mach
  executeSequential initialModel actions
