{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (makeLenses, to, view, (.~), (^.))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function ((&))
import           Data.Kind (Type)
import           Data.Maybe (isJust, isNothing)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show, Eq)

-- This type and its associated fold might be useful.
data DrinkAdditive = Milk | Sugar deriving (Bounded, Enum, Show)

drinkAdditive :: a -> a -> DrinkAdditive -> a
drinkAdditive m _ Milk  = m
drinkAdditive _ s Sugar = s

data Model (v :: Type -> Type) = Model
  { _modelDrinkType :: DrinkType
  , _modelHasMug    :: Bool
  , _modelMilk      :: Int
  , _modelSugar     :: Int
  , _modelCoins     :: Int
  }
$(makeLenses ''Model)

newtype SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

data AddMug (v :: Type -> Type) = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show

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
  [ Update $ \m (SetDrinkType d) _ -> m
      & modelDrinkType .~ d
      & modelMilk      .~ 0
      & modelSugar     .~ 0

  , Ensure $ \_ newM _ drink -> case (_modelDrinkType newM, drink) of
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
  [ Require $ \m _ -> m ^. modelHasMug
  , Update $ \m _ _ -> m & modelHasMug .~ False
  , Ensure $ \_ _ _ -> assert . isNothing
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m | _modelHasMug m = (pure . pure) TakeMug
          | otherwise = Nothing

    exec :: TakeMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      _ <- evalIO (C.takeMug mach) >>= evalEither
      evalIO $ view C.mug <$> C.peek mach

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug mach = Command gen exec
  [ Require $ \m _ -> m ^. modelHasMug . to not
  , Update $ \m _ _ -> m & modelHasMug .~ True
  , Ensure $ \_ _ _ -> assert . isJust
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen m | not $ _modelHasMug m = (pure . pure) AddMug
          | otherwise  = Nothing

    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      evalIO (C.addMug mach) >>= evalEither
      evalIO $ view C.mug <$> C.peek mach


stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False 0 0 0
      commands = ($ mach) <$>
        [ cSetDrinkType
        , cAddMug
        , cTakeMug
        -- , cAddMilkSugar
        -- , cInsertCoins
        -- , cRefundCoins
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ C.reset mach
  executeSequential initialModel actions
