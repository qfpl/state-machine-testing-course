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
addSugar :: MonadIO m => Machine -> m ()
```

# Your Tasks

* Define `cInsertCoins` and `cRefundCoins` commands, that
  insert/remove coins and update the model.

* Define a `cAddMilkSugar` that adds either milk or sugar to the
  drink. Hot chocolate drinks cannot have milk or sugar added, so you
  will need to exclude this possibility when building the generator,
  as well as in a `Require` callback.

  If you struggle to implement the combined command, implement it as
  two commands first, and then abstract over the common parts.
