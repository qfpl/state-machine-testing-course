{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function ((&))
import           Data.Kind (Type)
import           Data.Maybe (isJust, isNothing)
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
newtype SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkType mach = Command gen exec
  [ Update $ \(Model _oldDrinkType hasMug) (SetDrinkType d) _execResult
      -> Model d hasMug
  , Ensure $ \_oldModel (Model d _) _input drink -> case (d, drink) of
      (Coffee, C.Coffee{})           -> success
      (HotChocolate, C.HotChocolate) -> success
      (Tea, C.Tea{})                 -> success
      _                              -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen _ = pure $ SetDrinkType <$> Gen.enumBounded

    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = evalIO $ do
      mach & case d of
        Coffee       -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea          -> C.tea
      view C.drinkSetting <$> C.peek mach

cTakeMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMug mach = Command gen exec
  [ Require $ \(Model _ hasMug) _input -> hasMug
  , Update $ \(Model drink _) _input _execResult -> Model drink False
  , Ensure $ \_oldM _newM _input -> assert . isNothing
  ]
  where
    gen :: Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen (Model _ hasMug)
      | hasMug = Nothing
      | otherwise = Just $ pure TakeMug

    exec :: TakeMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      _ <- evalIO (C.takeMug mach) >>= evalEither
      evalIO $ view C.mug <$> C.peek mach

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug mach = Command gen exec
  [ Require $ \(Model _ hasMug) _input -> not hasMug
  , Update $ \(Model drink _) _input _execResult -> Model drink True
  , Ensure $ \_oldM _newM _input -> assert . isJust
  ]
  where
    gen :: Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen (Model _ hasMug)
      | hasMug = Just $ pure AddMug
      | otherwise = Nothing

    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      evalIO (C.addMug mach) >>= evalEither
      evalIO $ view C.mug <$> C.peek mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False
      commands = ($ mach) <$>
        [ cSetDrinkType
        , cTakeMug
        , cAddMug
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  C.reset mach
  executeSequential initialModel actions
