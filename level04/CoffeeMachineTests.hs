{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (failing, makeLenses, to, view)
import           Control.Lens.Operators
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

newtype InsertCoins (v :: Type -> Type) = InsertCoins Int deriving Show
data RefundCoins (v :: Type -> Type) = RefundCoins deriving Show

newtype AddMilkSugar (v :: Type -> Type) = AddMilkSugar DrinkAdditive
  deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

instance HTraversable InsertCoins where
  htraverse _ (InsertCoins i) = pure $ InsertCoins i

instance HTraversable RefundCoins where
  htraverse _ _ = pure RefundCoins

instance HTraversable AddMilkSugar where
  htraverse _ (AddMilkSugar additive) = pure $ AddMilkSugar additive

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
    exec (SetDrinkType d) = do
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
      _ <- C.takeMug mach >>= evalEither
      view C.mug <$> C.peek mach

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
      C.addMug mach >>= evalEither
      view C.mug <$> C.peek mach

cAddMilkSugar
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugar mach = Command gen exec
  [ Require $ \m _ -> m ^. modelDrinkType /= HotChocolate
  , Update $ \m (AddMilkSugar additive) _ ->
      m & drinkAdditive modelMilk modelSugar additive +~ 1
  , Ensure $ \_ newM _ setting ->
      let
        setMilk = setting ^?! (C._Coffee `failing` C._Tea) . C.milk
        setSugar = setting ^?! (C._Coffee `failing` C._Tea) . C.sugar
      in do
        newM ^. modelMilk === setMilk
        newM ^. modelSugar === setSugar
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (AddMilkSugar Symbolic))
    gen m = case m ^. modelDrinkType of
      HotChocolate -> Nothing
      _ -> Just $ AddMilkSugar <$> Gen.enumBounded

    exec :: AddMilkSugar Concrete -> m C.Drink
    exec (AddMilkSugar additive) = do
      drinkAdditive C.addMilk C.addSugar additive mach
      view C.drinkSetting <$> C.peek mach

cInsertCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cInsertCoins mach = Command gen exec
  [ Update $ \m (InsertCoins coins) _ -> m & modelCoins +~ coins
  , Ensure $ \_ newM _ currentCoins -> newM ^. modelCoins === currentCoins
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (InsertCoins Symbolic))
    gen _ = Just $ InsertCoins <$> Gen.int (Range.linear 0 100)

    exec :: InsertCoins Concrete -> m Int
    exec (InsertCoins coins) = do
      C.insertCoins coins mach
      view C.coins <$> C.peek mach

cRefundCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cRefundCoins mach = Command gen exec
  [ Update $ \m _ _ -> m & modelCoins .~ 0

  , Ensure $ \oldM _ _ refundCoins -> oldM ^. modelCoins === refundCoins
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (RefundCoins Symbolic))
    gen _ = Just $ pure RefundCoins

    exec :: RefundCoins Concrete -> m Int
    exec _ = C.refund mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False 0 0 0
      commands = ($ mach) <$>
        [ cSetDrinkType
        , cAddMug
        , cTakeMug
        , cAddMilkSugar
        , cInsertCoins
        , cRefundCoins
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  C.reset mach
  executeSequential initialModel actions
