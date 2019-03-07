# Level03 - More Commands!

In this level we're going to practice what we've learned so far by expanding our
model a bit and implementing commands to test the following features of our
machine:

### Inserting Coins

We can insert coins into the machine using the following function:

```haskell
insertCoins :: MonadIO m => Int -> Machine -> m ()
```

### Refunding Coins

We can have our machine refund all the money inserted thus far with this
function:

```haskell
refund :: MonadIO m => Machine -> m Int
```

### Adding Milk or Sugar

We can add either milk or sugar with the following functions:

```haskell
addMilk :: MonadIO m => Machine -> m ()
--
addSugar :: MonadIO m => Machine -> m ()
```
