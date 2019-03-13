{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (makeLenses, to, view, (^?), (.~), (^.), (+~))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function ((&))
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show, Eq)
data DrinkAdditive = Milk | Sugar deriving (Bounded, Enum, Show)

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

newtype AddMilkSugar (v :: Type -> Type) = AddMilkSugar DrinkAdditive deriving Show
newtype InsertCoins (v :: Type -> Type)  = InsertCoins Int deriving Show
data RefundCoins (v :: Type -> Type)     = RefundCoins deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

instance HTraversable AddMilkSugar where
  htraverse _ (AddMilkSugar d) = pure $ AddMilkSugar d

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

instance HTraversable RefundCoins where
  htraverse _ _ = pure RefundCoins

instance HTraversable InsertCoins where
  htraverse _ (InsertCoins n) = pure (InsertCoins n)

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
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m | _modelHasMug m = (pure . pure) TakeMug
          | otherwise = Nothing

    exec :: TakeMug Concrete -> m C.Mug
    exec _ = evalIO (C.takeMug mach) >>= evalEither

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

milkOrSugar :: DrinkAdditive -> a -> a -> a
milkOrSugar Milk m  _ = m
milkOrSugar Sugar _ s = s

cAddMilkSugar
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugar mach = Command gen exec
  [ Require $ \m _ -> HotChocolate /= _modelDrinkType m

  , Update $ \m (AddMilkSugar additive) _ -> case additive of
      Milk -> m & modelMilk +~ 1
      Sugar -> m & modelSugar +~ 1

  , Ensure $ \oldM newM (AddMilkSugar additive) mug' ->
      let (additiveL, sL) = milkOrSugar additive (modelMilk, C.milk) (modelSugar, C.sugar)

          drinkAdditiveQty = case _modelDrinkType newM of
            Coffee -> mug' ^? C._Coffee . sL
            Tea    -> mug' ^? C._Tea . sL
            _      -> Nothing

      in do
        (newM ^. additiveL) - (oldM ^. additiveL) === 1
        Just (newM ^. additiveL) === drinkAdditiveQty
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (AddMilkSugar Symbolic))
    gen m | HotChocolate /= _modelDrinkType m = pure (AddMilkSugar <$> Gen.enumBounded)
          | otherwise = Nothing

    exec :: (MonadTest m, MonadIO m) => AddMilkSugar Concrete -> m C.Drink
    exec (AddMilkSugar additive) = evalIO $ do
      milkOrSugar additive C.addMilk C.addSugar mach
      view C.drinkSetting <$> C.peek mach

cInsertCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cInsertCoins mach = Command gen exec
  [ Update $ \m (InsertCoins coins) _ -> m & modelCoins +~ coins

  , Ensure $ \oldM newM (InsertCoins coins) currentCoins -> do
      newM ^. modelCoins         === currentCoins
      oldM ^. modelCoins + coins === currentCoins
      newM ^. modelCoins - coins === oldM ^. modelCoins
  ]
  where
    gen :: Model Symbolic -> Maybe (g (InsertCoins Symbolic))
    gen _ = Just $ InsertCoins <$> Gen.int (Range.linear 0 100)

    exec :: InsertCoins Concrete -> m Int
    exec (InsertCoins coins) = evalIO $ do
      C.insertCoins coins mach
      view C.coins <$> C.peek mach

cRefundCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cRefundCoins mach = Command gen exec
  [ Update $ \m _ _ -> m & modelCoins .~ 0

  , Ensure $ \oldM newM _ refundCoins -> do
      oldM ^. modelCoins - refundCoins === 0
      newM ^. modelCoins + refundCoins === oldM ^. modelCoins
  ]
  where
    gen :: Model Symbolic -> Maybe (g (RefundCoins Symbolic))
    gen _ = Just $ pure RefundCoins

    exec :: RefundCoins Concrete -> m Int
    exec _ = evalIO (C.refund mach)

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
  evalIO $ C.reset mach
  executeSequential initialModel actions
