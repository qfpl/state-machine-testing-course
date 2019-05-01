{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine          as C
import           Control.Lens           (failing, makeLenses, view, _Just)
import           Control.Lens.Extras    (is)
import           Control.Lens.Operators
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function          ((&))
import           Data.Kind              (Type)
import           Data.Maybe             (isJust)
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty             (TestTree)
import           Test.Tasty.Hedgehog    (testProperty)

data DrinkType
  = Coffee
  | HotChocolate
  | Tea
  deriving (Bounded, Enum, Show, Eq)

data DrinkAdditive
  = Milk
  | Sugar
  deriving (Bounded, Enum, Show)

drinkAdditive :: a -> a -> DrinkAdditive -> a
drinkAdditive m _ Milk  = m
drinkAdditive _ s Sugar = s

data MugStatus
  = Empty
  | Full
  deriving (Eq, Show)

-- You'll want to add this to the 'Model', probably as a 'Maybe
-- Credit', so that 'Nothing' means "it's unknown whether we have
-- enough credit".
data Credit
  = TooLittle
  | Enough
  deriving (Eq, Show)

data Model (v :: Type -> Type) = Model
  { _modelDrinkType        :: DrinkType
  , _modelMug              :: Maybe MugStatus
  , _modelMilk             :: Int
  , _modelSugar            :: Int
  , _modelCoins            :: Int
  , _modelDrinkCost        :: Maybe Int
  , _modelHasDispensed     :: Bool
  }
$(makeLenses ''Model)

hasMug :: Model v -> Bool
hasMug = isJust . _modelMug

doesntHaveMug :: Model v -> Bool
doesntHaveMug = not . hasMug

data DispenseDrink (v :: Type -> Type) = DispenseDrink deriving Show
instance HTraversable DispenseDrink where htraverse _ _ = pure DispenseDrink

data CheckCredit (v :: Type -> Type) = CheckCredit deriving Show
instance HTraversable CheckCredit where htraverse _ _ = pure CheckCredit

data RefundCoins (v :: Type -> Type) = RefundCoins deriving Show
instance HTraversable RefundCoins where htraverse _ _ = pure RefundCoins

newtype InsertCoins (v :: Type -> Type) = InsertCoins Int deriving Show

instance HTraversable InsertCoins where
  htraverse _ (InsertCoins n) = pure (InsertCoins n)

data AddMug (v :: Type -> Type) = AddMug deriving Show
instance HTraversable AddMug where htraverse _ _ = pure AddMug

data TakeMug (v :: Type -> Type) = TakeMug deriving Show
instance HTraversable TakeMug where htraverse _ _ = pure TakeMug

newtype AddMilkSugar (v :: Type -> Type) = AddMilkSugar DrinkAdditive deriving Show
newtype SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

instance HTraversable AddMilkSugar where
  htraverse _ (AddMilkSugar d) = pure $ AddMilkSugar d

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

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
    exec (SetDrinkType d) = evalIO $ do
      mach & case d of
        Coffee       -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea          -> C.tea
      view C.drinkSetting <$> C.peek mach

milkOrSugarExec
  :: ( MonadTest m
     , MonadIO m
     )
  => C.Machine
  -> AddMilkSugar Concrete
  -> m C.Drink
milkOrSugarExec mach (AddMilkSugar additive) = do
  evalIO $ drinkAdditive C.addMilk C.addSugar additive mach
  view C.drinkSetting <$> C.peek mach

genAddMilkSugarCommand
  :: MonadGen g
  => (DrinkType -> Bool)
  -> Model Symbolic
  -> Maybe (g (AddMilkSugar Symbolic))
genAddMilkSugarCommand isDrinkType m
  | isDrinkType (m ^. modelDrinkType) = pure $ AddMilkSugar <$> Gen.enumBounded
  | otherwise                         = Nothing

cAddMilkSugarSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugarSad mach = Command (genAddMilkSugarCommand (== HotChocolate)) (milkOrSugarExec mach)
  [ Require $ \m _ ->
      m ^. modelDrinkType == HotChocolate

  , Ensure $ \_ _ _ drink ->
      assert $ is C._HotChocolate drink
  ]

cAddMilkSugarHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugarHappy ref = Command (genAddMilkSugarCommand (/= HotChocolate)) (milkOrSugarExec ref)
  [ Require $ \m _ ->
      m ^. modelDrinkType /= HotChocolate

  , Update $ \m (AddMilkSugar additive) _ -> m
      & drinkAdditive modelMilk modelSugar additive +~ 1

  , Ensure $ \_ newM _ setting ->
      let
        setMilk = setting ^?! (C._Coffee `failing` C._Tea) . C.milk
        setSugar = setting ^?! (C._Coffee `failing` C._Tea) . C.sugar
      in do
        newM ^. modelMilk === setMilk
        newM ^. modelSugar === setSugar
  ]

genAddMug :: MonadGen g => Model Symbolic -> Maybe (g (AddMug Symbolic))
genAddMug m | doesntHaveMug m = pure $ pure AddMug
            | otherwise       = Nothing

genTakeMug :: MonadGen g => Model Symbolic -> Maybe (g (TakeMug Symbolic))
genTakeMug m | hasMug m  = pure $ pure TakeMug
             | otherwise = Nothing

cTakeMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMugSad mach = Command genTakeMug exec
  [ Require $ \m _ -> doesntHaveMug m
  , Ensure $ \_ _ _ e -> either (=== C.NoMug) (const failure) e
  ]
  where
    exec :: TakeMug Concrete -> m (Either C.MachineError C.Mug)
    exec _ = evalIO $ C.takeMug mach

cTakeMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMugHappy mach = Command genTakeMug exec
  [ Require $ \m _ -> hasMug m
  , Update $ \m _ _ -> m & modelMug .~ Nothing
  ]
  where
    exec :: TakeMug Concrete -> m ()
    exec _ = void $ evalIO (C.takeMug mach) >>= evalEither

cAddMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMugHappy mach = Command genAddMug exec
  [ Require $ \m _ -> doesntHaveMug m
  , Update $ \m _ _ -> m & modelMug ?~ Empty
  ]
  where
    exec :: AddMug Concrete -> m ()
    exec _ = evalIO (C.addMug mach) >>= evalEither

cAddMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMugSad mach = Command genAddMug exec
  [ Require $ \m _ -> hasMug m
  , Ensure $ \ _ _ _ res -> either (=== C.MugInTheWay) (const failure) res
  ]
  where
    exec :: AddMug Concrete -> m (Either C.MachineError ())
    exec _ = evalIO $ C.addMug mach

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
    exec (InsertCoins coins) = evalIO $ do
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
    exec _ = evalIO (C.refund mach)

-- TODO
--
-- In this command you'll want to set the drink cost and the
-- credit-checked value, with appropriate callbacks so this command can
-- only happen once in a generated sequence.
cCheckCredit
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cCheckCredit = undefined

cDispenseHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cDispenseHappy mach = Command gen exec
  [ Require $ \m _ -> okayToDispense m

  , Update $ \m _ _ -> m
    & modelCoins -~ (m ^?! modelDrinkCost . _Just)
    & modelMug . _Just .~ Full
    & modelHasDispensed .~ True

  , Ensure $ \_ newM _ (credit, contents) -> do
      -- Check that we have calculated the right amount of money in the machine
      newM ^. modelCoins === credit

      -- Check that the drink we got was the drink we configured
      let
        checkAdditives (C.MilkSugar milk sugar)
          = (newM ^. modelMilk === milk)
          *> (newM ^. modelSugar === sugar)
      case contents of
        C.HotChocolate -> newM ^. modelDrinkType === HotChocolate
        C.Coffee additives ->
          (newM ^. modelDrinkType === Coffee) *> checkAdditives additives
        C.Tea additives ->
          (newM ^. modelDrinkType === Tea) *> checkAdditives additives
  ]
  where
    okayToDispense :: Model Symbolic -> Bool
    okayToDispense m = hasMug m
      && not (m ^. modelHasDispensed)
      && isJust (m ^. modelDrinkCost)
   -- && and there is enough credit

    gen :: Model Symbolic -> Maybe (g (DispenseDrink Symbolic))
    gen m = if okayToDispense m then Just $ pure DispenseDrink else Nothing

    exec :: DispenseDrink Concrete -> m (Int, C.Drink)
    exec _ = do
      evalIO (C.dispense mach) >>= evalEither
      state <- evalIO (C.peek mach)
      let
        credit = state ^. C.coins
        Just (C.Mug (Just contents)) = state ^. C.mug
      pure (credit, contents)

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- evalIO C.newMachine

  let initialModel = Model HotChocolate Nothing 0 0 0 Nothing False
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
        , cCheckCredit
        , cDispenseHappy
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ C.reset mach
  executeSequential initialModel actions
