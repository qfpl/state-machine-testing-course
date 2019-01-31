{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function ((&))
import           Data.Kind (Type)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show)
newtype Model (v :: Type -> Type) = Model DrinkType

newtype SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkType mach = Command gen exec
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
      mach & case d of
        Coffee -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea -> C.tea
      view C.drinkSetting <$> C.peek mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate
      commands = ($ mach) <$>
        [ cSetDrinkType
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ C.reset mach
  executeSequential initialModel actions
