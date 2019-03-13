# Level 02 - Pre-conditions & Commands with arguments

We've built some simple `Command`s to ensure we can select different drink types
without our machine exploding. Yay. Note that because of how we've built these
`Command`s, any sequence of `Command`s is as valid as any other. Which is okay
only when we have actions that can always run, regardless of the current state
of the system.

What about when we have `Command`s that do not make sense to, run unless the
system has reached a particular state? If we do not manage this, we are re left
with a non-deterministic test suite because some `Command`s _might_ be generated
in an absurd order.

We solve this by using the `Require` callback, which allows Hedgehog to test
that a `Command` is valid in the current sequence. We will use the `takeMug`
function as an example.

Including a `Command` for this function without a `Require` callback would
result in our tests passing only when this `Command` was included after a call
to `addMug`, with no other `takeMug` being called in between.

By expanding our model with a value to track the status of the mug, we can
prevent the `takeMug` `Command` from being included unless our preconditions
have been satisified.

Doesn't this mean that our tests are only testing the "happy path"? Yes. We will
address this in a future level. Later on we'll also go into more detail about
how adding these smaller "state transitions" to your model can make things
easier.

The goals for this level are to implement `Commands` for the following
functions, including sensible `Require` callbacks:

### Add Mug

We add a mug using the `addMug` function:

```haskell
addMug :: MonadIO m => Machine -> m (Either MachineError ())
```

### Take Mug

Similarly, we may remove a mug with the `takeMug` function:

```haskell
takeMug :: MonadIO m => Machine -> m (Either MachineError Mug)
```

### Collapse the `DrinkType` commands to one

The drink commands we set up in the previous level involve a lot of
boilerplate. We are able to use generators to express the three options for
setting drink types as one `Command`. Collapse the following three `Command`s:

* `cSetDrinkHotChocolate`
* `cSetDrinkCoffee`
* `cSetDrinkTea`

To a single `Command`:

* `cSetDrinkType`
