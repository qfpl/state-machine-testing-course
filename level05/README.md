# Level 05 - Divergence and Phases

You may have noticed that in the process of building the model and various
commands, we're building another state machine! One that could almost be a
replica of the thing we're trying to test. This is a problem: our goal is to
prove that the thing we're testing operates correctly, not duplicate it.

So, how do we create _just enough_ of a model so we're able to write useful
tests, without creating a crummy duplicate of the thing we're testing? This is
where state machine testing becomes an art.

The goal: how do we _minimise_ the model so we can still write sensible
`Command`s? This often depends on the thing that you're testing, but there are
some general techniques.

One technique is to define `Command`(s) that do nothing but inspect the
current model and update specific values that will be used by `Require` or
`Ensure` callbacks in subsequent `Command`s.

This style of `Command` acts as a 'transition' within the state machine that
we're building. These 'transition' `Command`s serve to restrict the generated
tests to valid subsets of `Command`s.

Transition `Commands** collect complicated logic for transitions into a single
location and set values on the model that other `Command`s are able to check
more easily.

The goal for this level is to write a `Command** that dispenses a drink and
checks that this drink matches our expectations, based on our current model of
what the drink should be.

**NB:** We have deliberately provided limited guidance on how to complete this
exercise. Not because we're mean, but because we consider this to be a vital
skill for effectively applying state machine testing to large and complex
systems.

Some functions that will be useful:

### Current Credit

The function for checking the cost of the current drink configuration is:

```haskell
currentDrinkCost :: MonadIO m => Machine -> m Int
```

### Dispensing a drink

The function for dispensing a drink is:

```haskell
dispense :: MonadIO m => Machine -> m (Either MachineError ())
```
