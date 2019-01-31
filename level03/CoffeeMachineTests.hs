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
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show)
newtype Model (v :: * -> *) = Model DrinkType

newtype SetDrinkType (v :: * -> *) = SetDrinkType DrinkType deriving Show

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cSetDrinkType ref = Command gen exec
  [ Update $ \_ (SetDrinkType d) _ -> Model d
  , Ensure $ \_ (Model d) _ drink -> case (d, drink) of
      (Coffee, C.Coffee{}) -> success
      (HotChocolate, C.HotChocolate) -> success
      (Tea, C.Tea{}) -> success
      _ -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen _ = Just $ SetDrinkType <$> Gen.enumBounded

    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = evalIO $ do
      R.modifyIORef ref $ case d of
        Coffee -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea -> C.tea
      view C.drinkSetting <$> R.readIORef ref

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  r <- evalIO $ R.newIORef C.initialState

  let initialModel = Model HotChocolate
      commands = ($ r) <$>
        [ cSetDrinkType
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ R.writeIORef r C.initialState
  executeSequential initialModel actions
