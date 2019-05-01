# Level 05 - Phases and Transitions

You may have noticed that in the process of building the model and
various commands, we're building another state machine! This is a
problem: our goal is to prove that the thing we're testing operates
correctly, not duplicate it. How do we create _just enough_ of a model
so we're able to write useful tests, without creating a crummy
duplicate of the thing we're testing? This is where state machine
testing becomes an art.

The goal: construct a minimal model that can still support sensible
`Command`s. The exact details of this often depend on the thing that
you're testing.

One technique is to define `Command`s that do no work of their own,
but instead inspect the current model and update specific values used
by `Require` or `Ensure` callbacks in subsequent `Command`s.

This style of `Command` acts as a 'transition' within the state
machine that we're building. 'Transition `Command`s' serve to
restrict the generated tests to valid subsets of `Command`s, by
collecting the logic for transitions into a single location and
setting values on the model that other `Command`s can check more
easily.

Some functions that will be useful for this level's task:

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

# Your Task

Write a transition `Command` that checks whether the model has enough
credit to dispense a drink and stores that fact in the model. Once
that's working, update the other commands to use this extra
information instead of repeating those checks inline.

**NB:** We deliberately provide limited written guidance on how to
complete this exercise. Not because we're mean, but because we
consider this a vital skill for effectively applying state machine
testing to complex systems. Ask us if you get stuck and have struggled
for a few minutes.
