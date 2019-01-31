{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CoffeeMachineTests (stateMachineTests) where

import           Data.Kind              (Type)
import qualified CoffeeMachine          as C
import           Control.Lens           (makeLenses, to, view, (+~), (.~), (^.),
                                         (^?))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function          ((&))
import qualified Data.IORef             as R
import           Data.Maybe             (isJust)
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty             (TestTree)
import           Test.Tasty.Hedgehog    (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show, Eq)
data DrinkAdditive = Milk | Sugar deriving (Bounded, Enum, Show)

data Model (v :: Type -> Type) = Model
  { _modelDrinkType :: DrinkType
  , _modelHasMug    :: Bool
  , _modelMilk      :: Int
  , _modelSugar     :: Int
  }
$(makeLenses ''Model)

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
  => R.IORef C.MachineState
  -> Command g m Model
cSetDrinkType ref = Command gen exec
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
      R.modifyIORef ref $ case d of
        Coffee       -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea          -> C.tea
      view C.drinkSetting <$> R.readIORef ref

milkOrSugar :: DrinkAdditive -> a -> a ->  a
milkOrSugar Milk m  _ = m
milkOrSugar Sugar _ s = s

milkOrSugarExec
  :: ( MonadTest m
     , MonadIO m
     )
  => R.IORef C.MachineState
  -> AddMilkSugar Concrete
  -> m C.Drink
milkOrSugarExec ref (AddMilkSugar additive) = do
  evalIO $ R.modifyIORef ref (milkOrSugar additive C.addMilk C.addSugar)
  view C.drinkSetting <$> evalIO (R.readIORef ref)

genAddMilkSugarCommand
  :: MonadGen g
  => (DrinkType -> Bool)
  -> Model Symbolic
  -> Maybe (g (AddMilkSugar Symbolic))
genAddMilkSugarCommand isDrinkType m
  | isDrinkType (m ^. modelDrinkType) = Just (AddMilkSugar <$> Gen.enumBounded)
  | otherwise                         = Nothing

cAddMilkSugarSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cAddMilkSugarSad ref = Command (genAddMilkSugarCommand (== HotChocolate)) (milkOrSugarExec ref)
  [ Require $ \m _ ->
      not (m ^. modelHasMug) || m ^. modelDrinkType == HotChocolate

  , Ensure $ \_ _ _ drink ->
      drink ^? C._HotChocolate === Just ()
  ]

cAddMilkSugarHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cAddMilkSugarHappy ref = Command (genAddMilkSugarCommand (/= HotChocolate)) (milkOrSugarExec ref)
  [ Require $ \m _ ->
      m ^. modelHasMug && m ^. modelDrinkType /= HotChocolate

  , Update $ \m (AddMilkSugar additive) _ ->
      m & milkOrSugar additive modelMilk modelSugar +~ 1

  , Ensure $ \oldM newM (AddMilkSugar additive) mug' ->
      let (mL, sl) = milkOrSugar additive (modelMilk, C.milk) (modelSugar, C.sugar)

          drinkAdditiveQty = case newM ^. modelDrinkType of
            Coffee -> mug' ^? C._Coffee . sl
            Tea    -> mug' ^? C._Tea . sl
            _      -> Nothing

      in do
        (newM ^. mL) - (oldM ^. mL) === 1
        Just (newM ^. mL) === drinkAdditiveQty
  ]

genAddMug :: MonadGen g => Model Symbolic -> Maybe (g (AddMug Symbolic))
genAddMug m | m ^. modelHasMug . to not = Just $ pure AddMug
            | otherwise                 = Nothing

genTakeMug :: MonadGen g => Model Symbolic -> Maybe (g (TakeMug Symbolic))
genTakeMug m | m ^. modelHasMug = Just $ pure TakeMug
             | otherwise        = Nothing

cTakeMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cTakeMugSad ref = Command genTakeMug exec
  [ Require $ \m _ -> m ^. modelHasMug . to not
  , Ensure $ \_ _ _ e -> either (=== C.NoMug) (const failure) e
  ]
  where
    exec :: TakeMug Concrete -> m (Either C.MachineError (C.Mug, C.MachineState))
    exec _ = evalIO $ C.takeMug <$> R.readIORef ref

cTakeMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cTakeMugHappy ref = Command genTakeMug exec
  [ Require $ \m _ -> m ^. modelHasMug
  , Update $ \m _ _ -> m & modelHasMug .~ False
  ]
  where
    exec :: TakeMug Concrete -> m ()
    exec _ = do
      ms <- evalIO $ R.readIORef ref
      evalEither (C.takeMug ms) >>= evalIO . R.writeIORef ref . snd

cAddMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cAddMugHappy ref = Command genAddMug exec
  [ Require $ \m _ -> m ^. modelHasMug . to not
  , Update $ \m _ _ -> m & modelHasMug .~ True
  , Ensure $ \_ _ _ -> assert . isJust
  ]
  where
    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      ms <- evalIO $ R.readIORef ref
      ms' <- evalEither (C.addMug ms)
      evalIO $ R.writeIORef ref ms'
      pure $ view C.mug ms'

cAddMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cAddMugSad ref = Command genAddMug exec
  [ Require $ \m _ -> m ^. modelHasMug
  , Ensure $ \ _ _ _ res -> either (=== C.MugInTheWay) (const failure) res
  ]
  where
    exec :: AddMug Concrete -> m (Either C.MachineError C.MachineState)
    exec _ = evalIO $ C.addMug <$> R.readIORef ref

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  r <- evalIO $ R.newIORef C.initialState

  let initialModel = Model HotChocolate False 0 0
      commands = ($ r) <$>
        [ cSetDrinkType
        , cAddMugHappy
        , cAddMugSad
        , cTakeMugHappy
        , cTakeMugSad
        , cAddMilkSugarHappy
        , cAddMilkSugarSad
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ R.writeIORef r C.initialState
  executeSequential initialModel actions
