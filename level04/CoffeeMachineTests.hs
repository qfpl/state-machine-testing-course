{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CoffeeMachineTests (stateMachineTests) where

import           Data.Kind              (Type)
import qualified CoffeeMachine          as C
import           Control.Lens           (failing, makeLenses, to, view)
import           Control.Lens.Extras    (is)
import           Control.Lens.Operators
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function          ((&))
import           Data.Maybe             (isJust)
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty             (TestTree)
import           Test.Tasty.Hedgehog    (testProperty)

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

data AddMug (v :: Type -> Type)  = AddMug deriving Show
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
    & modelMilk .~ 0
    & modelSugar .~ 0

  , Ensure $ \_ m _ drink -> case (m ^. modelDrinkType, drink) of
      (Coffee, C.Coffee{})           -> success
      (HotChocolate, C.HotChocolate) -> success
      (Tea, C.Tea{})                 -> success
      _                              -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen _ = Just $ SetDrinkType <$> Gen.enumBounded

    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = do
      mach & case d of
        Coffee       -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea          -> C.tea
      view C.drinkSetting <$> C.peek mach

genAddMilkSugarCommand
  :: MonadGen g
  => (DrinkType -> Bool)
  -> Model Symbolic
  -> Maybe (g (AddMilkSugar Symbolic))
genAddMilkSugarCommand isDrinkType m
  | isDrinkType (m ^. modelDrinkType) = Just (AddMilkSugar <$> Gen.enumBounded)
  | otherwise                         = Nothing

milkOrSugarExec
  :: ( MonadTest m
     , MonadIO m
     )
  => C.Machine
  -> AddMilkSugar Concrete
  -> m C.Drink
milkOrSugarExec mach (AddMilkSugar additive) = do
  drinkAdditive C.addMilk C.addSugar additive mach
  view C.drinkSetting <$> C.peek mach

cAddMilkSugarHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugarHappy ref = Command (genAddMilkSugarCommand (/= HotChocolate)) (milkOrSugarExec ref)
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

cAddMilkSugarSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugarSad mach = Command (genAddMilkSugarCommand (== HotChocolate)) (milkOrSugarExec mach)
  [ Require $ \m _ -> m ^. modelDrinkType == HotChocolate

  , Ensure $ \_ _ _ drink ->
      assert $ is C._HotChocolate drink
  ]

cTakeMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMugHappy mach = Command gen exec
  [ Require $ \m _ -> m ^. modelHasMug
  , Update $ \m _ _ -> m & modelHasMug .~ False
  ]
  where
    gen :: Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m
      | m ^. modelHasMug = Just $ pure TakeMug
      | otherwise = Nothing

    exec :: TakeMug Concrete -> m C.Mug
    exec _ = C.takeMug mach >>= evalEither

cTakeMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMugSad mach = Command gen exec
  [ Require $ \m _ -> m ^. modelHasMug . to not
  , Ensure $ \_ _ _ e -> either (=== C.NoMug) (const failure) e
  ]
  where
    gen :: Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m
      | m ^. modelHasMug = Nothing
      | otherwise = Just $ pure TakeMug

    exec :: TakeMug Concrete -> m (Either C.MachineError C.Mug)
    exec _ = C.takeMug mach

cAddMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMugHappy mach = Command gen exec
  [ Require $ \m _ -> m ^. modelHasMug . to not
  , Update $ \m _ _ -> m & modelHasMug .~ True
  , Ensure $ \_ _ _ -> assert . isJust
  ]
  where
    gen :: Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen m
      | m ^. modelHasMug = Nothing
      | otherwise = Just $ pure AddMug

    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      C.addMug mach >>= evalEither
      view C.mug <$> C.peek mach

cAddMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMugSad mach = Command gen exec
  [ Require $ \m _ -> m ^. modelHasMug
  , Ensure $ \ _ _ _ res -> either (=== C.MugInTheWay) (const failure) res
  ]
  where
    gen :: Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen m
      | m ^. modelHasMug = Just $ pure AddMug
      | otherwise = Nothing

    exec :: AddMug Concrete -> m (Either C.MachineError ())
    exec _ = C.addMug mach

cInsertCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cInsertCoins mach = Command gen exec
  [ Update $ \m (InsertCoins coins) _ -> m & modelCoins +~ coins

  , Ensure $ \_ newM _ currentCoins -> newM ^. modelCoins === currentCoins
  ]
  where
    gen :: Model Symbolic -> Maybe (g (InsertCoins Symbolic))
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
    gen :: Model Symbolic -> Maybe (g (RefundCoins Symbolic))
    gen _ = Just $ pure RefundCoins

    exec :: RefundCoins Concrete -> m Int
    exec _ = C.refund mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False 0 0 0
      commands = ($ mach) <$>
        [ cSetDrinkType
        , cAddMugHappy
        , cAddMugSad
        , cTakeMugHappy
        , cTakeMugSad
        , cAddMilkSugarHappy
        , cAddMilkSugarSad
        , cInsertCoins
        , cRefundCoins
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  C.reset mach
  executeSequential initialModel actions
