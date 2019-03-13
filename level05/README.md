# Level 05 - Divergence and Phases

You may have noticed that in the process of building the model and various
commands, we're building another state machine! One that could almost be a
replica of the thing we're trying to test. This is a problem as our goal is to
prove that the thing we're testing operates correctly, not duplicate it.

So, how to create _just enough_ of a model so we're able to write useful tests,
without creating a crummy duplicate of the thing we're testing? This is where
state machine testing becomes a bit of an art.

The goal can be described as, how do we _minimise_ the model so we can still
write sensible `Command`s? This often depends on the thing that you're testing,
but there are some general techniques we can apply.

One such technique is to implement `Command`(s) that do nothing but inspect
the current model and update specific values that will be used by subsequent
`Command`s in `Require` or `Ensure` callbacks.

This style of `Command` would act as a 'transition' within the state machine
that we're building to manage our tests. These 'transition' style `Command`s
serve to guide the tests to valid subsets of `Command`s. 

You collect complicated logic for transitions in a single location and set
values on the model that other `Command`s are able to check more easily.

The goal for this level is to be able to write a `Command` that dispenses a
drink and checks that this drink matches our expectations, based on our current
model of what the drink should be.

To be able to write a `Command` that dispenses our drink we're going to need to know:

- how much our drink should cost
- whether we have a mug in place
- if we have inserted enough coins

A couple of functions that will be used in this level:

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
