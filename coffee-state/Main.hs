{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified CoffeeMachine as C
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.IORef
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Exit

data DrinkType = T | C | HC
newtype CoffeeModel (v :: * -> *) = CoffeeModel DrinkType

data SetDrinkC (v :: * -> *) = SetDrinkC deriving Show
data SetDrinkHC (v :: * -> *) = SetDrinkHC deriving Show
data SetDrinkT (v :: * -> *) = SetDrinkT deriving Show
data AddMug (v :: * -> *) = AddMug deriving Show

instance HTraversable SetDrinkC where htraverse _ _ = pure SetDrinkC
instance HTraversable SetDrinkHC where htraverse _ _ = pure SetDrinkHC
instance HTraversable SetDrinkT where htraverse _ _ = pure SetDrinkT
instance HTraversable AddMug where htraverse _ _ = pure AddMug

cSetDrinkC
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => IORef C.MachineState
  -> Command g m CoffeeModel
cSetDrinkC ref = Command gen exec
  [ Update $ \_ _ _ -> CoffeeModel HC
  , Ensure $ \_ _ _ drink -> case drink of
      C.Coffee{} -> success
      _ -> failure
  ]
  where
    gen :: CoffeeModel Symbolic -> Maybe (g (SetDrinkC Symbolic))
    gen _ = Just $ pure SetDrinkC

    exec :: SetDrinkC Concrete -> m C.Drink
    exec _ = evalIO $ do
      modifyIORef ref C.coffee
      view C.drinkSetting <$> readIORef ref

cSetDrinkHC
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => IORef C.MachineState
  -> Command g m CoffeeModel
cSetDrinkHC ref = Command gen exec
  [ Update $ \_ _ _ -> CoffeeModel HC
  , Ensure $ \_ _ _ drink -> case drink of
      C.HotChocolate -> success
      _ -> failure
  ]
  where
    gen :: CoffeeModel Symbolic -> Maybe (g (SetDrinkHC Symbolic))
    gen _ = Just $ pure SetDrinkHC

    exec :: SetDrinkHC Concrete -> m C.Drink
    exec _ = evalIO $ do
      modifyIORef ref C.hotChocolate
      view C.drinkSetting <$> readIORef ref

cSetDrinkT
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => IORef C.MachineState
  -> Command g m CoffeeModel
cSetDrinkT ref = Command gen exec
  [ Update $ \_ _ _ -> CoffeeModel T
  , Ensure $ \_ _ _ drink -> case drink of
      C.Tea{} -> success
      _ -> failure
  ]
  where
    gen :: CoffeeModel Symbolic -> Maybe (g (SetDrinkT Symbolic))
    gen _ = Just $ pure SetDrinkT

    exec :: SetDrinkT Concrete -> m C.Drink
    exec _ = evalIO $ do
      modifyIORef ref C.tea
      view C.drinkSetting <$> readIORef ref

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => IORef C.MachineState
  -> Command g m CoffeeModel
cAddMug ref = Command gen exec []
  where
    gen :: CoffeeModel Symbolic -> Maybe (g (AddMug Symbolic))
    gen _ = Just $ pure AddMug

    exec :: AddMug Concrete -> m ()
    exec _ = do
      s <- evalIO $ readIORef ref
      evalEither (C.addMug s) >>= evalIO . writeIORef ref

main :: IO ()
main = do
  r <- newIORef C.initialState
  let commands = ($ r) <$>
        [ cSetDrinkC
        , cSetDrinkHC
        , cSetDrinkT

        , cAddMug
        ]
      initialModel = CoffeeModel T
  b <- checkSequential $ Group "coffee machine"
    [ ("state_coffee_machine", property $ do
          actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
          evalIO $ writeIORef r C.initialState
          executeSequential initialModel actions
      )
    ]
  if b then exitSuccess else exitFailure
