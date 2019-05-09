{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (Lens', at, failing, folded, ix, to, view)
import           Control.Lens.Extras (is)
import           Control.Lens.Operators
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function ((&))
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
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
  { _modelDrinkType           :: DrinkType
  , _modelHasMug              :: Bool
  , _modelMilk                :: Int
  , _modelSugar               :: Int
  , _modelSavedPreferences    :: Map (Var C.PreferenceToken v) C.Drink
  , _modelBadPreferenceTokens :: Set (Var C.PreferenceToken v)
  , _modelCoins               :: Int
  }

initialModel :: Model v
initialModel = Model HotChocolate False 0 0 Map.empty Set.empty 0

class HasCoins (s :: (Type -> Type) -> Type) where
  coins :: Lens' (s v) Int

instance HasCoins Model where
  coins f m = f (_modelCoins m) <&> \x -> m { _modelCoins = x }

class HasDrinkConfig (s :: (Type -> Type) -> Type) where
  drinkType :: Lens' (s v) DrinkType
  milk :: Lens' (s v) Int
  sugar :: Lens' (s v) Int

instance HasDrinkConfig Model where
  drinkType f m = f (_modelDrinkType m) <&> \x -> m { _modelDrinkType = x }
  milk f m = f (_modelMilk m) <&> \x -> m { _modelMilk = x }
  sugar f m = f (_modelSugar m) <&> \x -> m { _modelSugar = x }

class HasPreferences (s :: (Type -> Type) -> Type) where
  savedPreferences
    :: Lens' (s v) (Map (Var C.PreferenceToken v) C.Drink)
  badPreferenceTokens :: Lens' (s v) (Set (Var C.PreferenceToken v))

instance HasPreferences Model where
  savedPreferences f m =
    f (_modelSavedPreferences m) <&> \x -> m { _modelSavedPreferences = x }
  badPreferenceTokens f m =
    f (_modelBadPreferenceTokens m) <&> \x -> m { _modelBadPreferenceTokens = x }

class HasMug (s :: (Type -> Type) -> Type) where
  mug :: Lens' (s v) Bool

instance HasMug Model where
  mug f m = f (_modelHasMug m) <&> \x -> m { _modelHasMug = x }

newtype SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

data AddMug (v :: Type -> Type)  = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show

newtype AddMilkSugar (v :: Type -> Type) = AddMilkSugar DrinkAdditive deriving Show
newtype InsertCoins (v :: Type -> Type)  = InsertCoins Int deriving Show
data RefundCoins (v :: Type -> Type)     = RefundCoins deriving Show

data SavePreferences (v :: Type -> Type) = SavePreferences deriving Show
newtype LoadPreferences (v :: Type -> Type)
  = LoadPreferences (Var C.PreferenceToken v) deriving Show
data BadPreferenceToken (v :: Type -> Type) = BadPreferenceToken deriving Show

data Reset (v :: Type -> Type) = Reset deriving Show

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

instance HTraversable SavePreferences where
  htraverse _ _ = pure SavePreferences

instance HTraversable LoadPreferences where
  htraverse f (LoadPreferences token) = LoadPreferences <$> htraverse f token

instance HTraversable BadPreferenceToken where
  htraverse _ _ = pure BadPreferenceToken

instance HTraversable Reset where
  htraverse _ _ = pure Reset

cSetDrinkType
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasDrinkConfig s)
  => C.Machine
  -> Command g m s
cSetDrinkType mach = Command gen exec
  [ Update $ \m (SetDrinkType d) _ -> m
    & drinkType .~ d
    & milk .~ 0
    & sugar .~ 0

  , Ensure $ \_ m _ drink -> case (m ^. drinkType, drink) of
      (Coffee, C.Coffee{})           -> success
      (HotChocolate, C.HotChocolate) -> success
      (Tea, C.Tea{})                 -> success
      _                              -> failure
  ]
  where
    gen :: s Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen _ = Just $ SetDrinkType <$> Gen.enumBounded

    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = do
      mach & case d of
        Coffee       -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea          -> C.tea
      view C.drinkSetting <$> C.peek mach

genAddMilkSugarCommand
  :: (MonadGen g, HasDrinkConfig s)
  => (DrinkType -> Bool)
  -> s Symbolic
  -> Maybe (g (AddMilkSugar Symbolic))
genAddMilkSugarCommand isDrinkType m
  | isDrinkType (m ^. drinkType) = Just (AddMilkSugar <$> Gen.enumBounded)
  | otherwise                    = Nothing

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
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasDrinkConfig s)
  => C.Machine
  -> Command g m s
cAddMilkSugarHappy ref = Command (genAddMilkSugarCommand (/= HotChocolate)) (milkOrSugarExec ref)
  [ Require $ \m _ -> m ^. drinkType /= HotChocolate

  , Update $ \m (AddMilkSugar additive) _ ->
      m & drinkAdditive milk sugar additive +~ 1

  , Ensure $ \_ newM _ setting ->
      let
        setMilk = setting ^?! (C._Coffee `failing` C._Tea) . C.milk
        setSugar = setting ^?! (C._Coffee `failing` C._Tea) . C.sugar
      in do
        newM ^. milk === setMilk
        newM ^. sugar === setSugar
  ]

cAddMilkSugarSad
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasDrinkConfig s)
  => C.Machine
  -> Command g m s
cAddMilkSugarSad mach = Command (genAddMilkSugarCommand (== HotChocolate)) (milkOrSugarExec mach)
  [ Require $ \m _ -> m ^. drinkType == HotChocolate
  , Ensure $ \_ _ _ drink ->
      assert $ is C._HotChocolate drink
  ]

cTakeMugHappy
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasMug s)
  => C.Machine
  -> Command g m s
cTakeMugHappy mach = Command gen exec
  [ Require $ \m _ -> m ^. mug
  , Update $ \m _ _ -> m & mug .~ False
  ]
  where
    gen :: s Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m
      | m ^. mug = Just $ pure TakeMug
      | otherwise = Nothing

    exec :: TakeMug Concrete -> m C.Mug
    exec _ = C.takeMug mach >>= evalEither

cTakeMugSad
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasMug s)
  => C.Machine
  -> Command g m s
cTakeMugSad mach = Command gen exec
  [ Require $ \m _ -> m ^. mug . to not
  , Ensure $ \_ _ _ e -> either (=== C.NoMug) (const failure) e
  ]
  where
    gen :: s Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m
      | m ^. mug = Nothing
      | otherwise = Just $ pure TakeMug

    exec :: TakeMug Concrete -> m (Either C.MachineError C.Mug)
    exec _ = C.takeMug mach

cAddMugHappy
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasMug s)
  => C.Machine
  -> Command g m s
cAddMugHappy mach = Command gen exec
  [ Require $ \m _ -> m ^. mug . to not
  , Update $ \m _ _ -> m & mug .~ True
  , Ensure $ \_ _ _ -> assert . isJust
  ]
  where
    gen :: s Symbolic -> Maybe (g (AddMug Symbolic))
    gen m
      | m ^. mug = Nothing
      | otherwise = Just $ pure AddMug

    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      C.addMug mach >>= evalEither
      view C.mug <$> C.peek mach

cAddMugSad
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasMug s)
  => C.Machine
  -> Command g m s
cAddMugSad mach = Command gen exec
  [ Require $ \m _ -> m ^. mug
  , Ensure $ \ _ _ _ res -> either (=== C.MugInTheWay) (const failure) res
  ]
  where
    gen :: s Symbolic -> Maybe (g (AddMug Symbolic))
    gen m
      | m ^. mug = Just $ pure AddMug
      | otherwise = Nothing

    exec :: AddMug Concrete -> m (Either C.MachineError ())
    exec _ = C.addMug mach

cInsertCoins
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasCoins s)
  => C.Machine
  -> Command g m s
cInsertCoins mach = Command gen exec
  [ Update $ \m (InsertCoins c) _ -> m & coins +~ c

  , Ensure $ \_ newM _ currentCoins -> newM ^. coins === currentCoins
  ]
  where
    gen :: s Symbolic -> Maybe (g (InsertCoins Symbolic))
    gen _ = Just $ InsertCoins <$> Gen.int (Range.linear 0 100)

    exec :: InsertCoins Concrete -> m Int
    exec (InsertCoins c) = do
      C.insertCoins c mach
      view C.coins <$> C.peek mach

cRefundCoins
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasCoins s)
  => C.Machine
  -> Command g m s
cRefundCoins mach = Command gen exec
  [ Update $ \m _ _ -> m & coins .~ 0

  , Ensure $ \oldM _ _ refundCoins -> oldM ^. coins === refundCoins
  ]
  where
    gen :: s Symbolic -> Maybe (g (RefundCoins Symbolic))
    gen _ = Just $ pure RefundCoins

    exec :: RefundCoins Concrete -> m Int
    exec _ = C.refund mach

currentDrink :: HasDrinkConfig s => s v -> C.Drink
currentDrink model = case model ^. drinkType of
  Coffee -> C.Coffee $ C.MilkSugar (model ^. milk) (model ^. sugar)
  Tea -> C.Tea $ C.MilkSugar (model ^. milk) (model ^. sugar)
  HotChocolate -> C.HotChocolate

setDrink :: HasDrinkConfig s => C.Drink -> s v -> s v
setDrink (C.Coffee (C.MilkSugar m s))
  = (drinkType .~ Coffee)
  . (milk .~ m)
  . (sugar .~ s)
setDrink C.HotChocolate
  = (drinkType .~ HotChocolate)
  . (milk .~ 0)
  . (sugar .~ 0)
setDrink (C.Tea (C.MilkSugar m s))
  = (drinkType .~ Tea)
  . (milk .~ m)
  . (sugar .~ s)

cSavePreferences
  :: forall g m s.
     ( MonadGen g
     , MonadTest m, MonadIO m
     , HasDrinkConfig s, HasPreferences s
     )
  => C.Machine
  -> Command g m s
cSavePreferences mach = Command gen exec
  [ Update $ \model _ token ->
      model & savedPreferences . at token ?~ currentDrink model
  ]
  where
    gen :: s Symbolic -> Maybe (g (SavePreferences Symbolic))
    gen _ = Just $ pure SavePreferences

    exec :: SavePreferences Concrete -> m C.PreferenceToken
    exec _ = C.savePreferences mach

cBadPreferenceToken
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasPreferences s)
  => C.Machine
  -> Command g m s
cBadPreferenceToken mach = Command gen exec
  [ Update $ \model _ token ->
      model & badPreferenceTokens %~ Set.insert token
  ]
  where
    gen :: s Symbolic -> Maybe (g (BadPreferenceToken Symbolic))
    gen _ = Just $ pure BadPreferenceToken

    exec :: BadPreferenceToken Concrete -> m C.PreferenceToken
    exec _ = C.badPreferenceToken mach

cLoadPreferencesHappy
  :: forall g m s.
     ( MonadGen g
     , MonadTest m, MonadIO m
     , HasDrinkConfig s, HasPreferences s
     )
  => C.Machine
  -> Command g m s
cLoadPreferencesHappy mach = Command gen exec
  [ Require $ \model (LoadPreferences token) ->
      Map.member token $ model ^. savedPreferences
  , Update $ \model (LoadPreferences token) _ ->
      setDrink (model ^?! savedPreferences . ix token) model
  , Ensure $ \_ newM (LoadPreferences token) drink ->
      let
        modelDrink = newM ^?! savedPreferences . ix token
      in
        modelDrink === drink
  ]
  where
    gen :: s Symbolic -> Maybe (g (LoadPreferences Symbolic))
    gen model = case model ^. savedPreferences . to Map.keys of
      [] -> Nothing
      tokens -> Just $ LoadPreferences <$> Gen.element tokens

    exec :: LoadPreferences Concrete -> m C.Drink
    exec (LoadPreferences token) = do
      C.loadPreferences mach (concrete token) >>= evalEither
      view C.drinkSetting <$> C.peek mach

cLoadPreferencesSad
  :: forall g m s. (MonadGen g, MonadTest m, MonadIO m, HasPreferences s)
  => C.Machine
  -> Command g m s
cLoadPreferencesSad mach = Command gen exec
  [ Require $ \model (LoadPreferences token) ->
      Set.member token $ model ^. badPreferenceTokens
  , Ensure $ \_ _ _ -> either (=== C.InvalidPreferenceToken) (const failure) 
  ]
  where
    gen :: s Symbolic -> Maybe (g (LoadPreferences Symbolic))
    gen model = case model ^.. badPreferenceTokens . folded of
      [] -> Nothing
      tokens -> Just $ LoadPreferences <$> Gen.element tokens

    exec :: LoadPreferences Concrete -> m (Either C.MachineError ())
    exec (LoadPreferences token) = C.loadPreferences mach $ concrete token

cReset
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cReset mach = Command gen exec
  [ Update $ \_ _ _ -> initialModel
  ]
  where
    gen :: Model Symbolic -> Maybe (g (Reset Symbolic))
    gen _ = Just $ pure Reset

    exec :: Reset Concrete -> m ()
    exec _ = C.reset mach

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let commands = ($ mach) <$>
        [ cSetDrinkType
        , cAddMugHappy
        , cAddMugSad
        , cTakeMugHappy
        , cTakeMugSad
        , cAddMilkSugarHappy
        , cAddMilkSugarSad
        , cInsertCoins
        , cRefundCoins
        , cSavePreferences
        , cLoadPreferencesHappy
        , cLoadPreferencesSad
        , cBadPreferenceToken
        , cReset
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  C.reset mach
  executeSequential initialModel actions
