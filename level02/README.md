# Level 02 - Pre-conditions & Commands with arguments

We've built some simple `Command`s to ensure we can select different drink types
without our machine exploding. Because of how we've built these `Command`s, any
sequence of random actions is valid. This is only okay when our actions can run
regardless of the current state of the system.

What about `Command`s that only make sense when the system has reached a
particular state? If we cannot handle these, we are left with non-deterministic
tests because some actions _might_ be generated in an absurd order.

We solve this in two ways:

1. By returning `Nothing` from our generator if there is no sensible
   action given the current model.

2. By using a `Require` callback, which allows Hedgehog to test that a
   `Command` is valid in the current sequence. We will use the
   `takeMug` function as an example.

<details>
  <summary>Isn't this mindless repetition?</summary>

  No. Each test runs at different times, and for different
  reasons. Returning `Nothing` instead of a generator means that we do
  not even try to generate an action at all, instead of generating
  actions that immediately fail the `Require` callback.

  `Require` callbacks can see the exact command generated, and also
  run during shrinking, so that the command sequence being tested
  remains valid as hedgehog tries to find a minimal example.
</details>

Including a `Command` for this function without these checks would
result in our tests passing only when this `Command` was included
after a call to `addMug`, with no other `takeMug` being called in
between.

By expanding our model with a value to track the status of the mug, we can
prevent the `takeMug` `Command` from being included unless our preconditions
have been satisified.

Does this mean that our tests are only testing the "happy path"? Yes. We will
address this in a future level. Later on we'll also go into more detail about
how to factor out smaller "state transitions" to simplify your `Require`
callbacks.

# Your Tasks

## Collapse the `DrinkType` commands to one

The drink commands we set up in the previous level contain a lot of repeated
boilerplate. We are able to use generators to express the three options for
setting drink types as one `Command`. Collapse the following three data types:

* `SetDrinkCoffee`
* `SetDrinkHotChocolate`
* `SetDrinkTea`

Into a single data type:

* `SetDrinkType`

Next collapse the following three `Command`s:

* `cSetDrinkHotChocolate`
* `cSetDrinkCoffee`
* `cSetDrinkTea`

Into a single `Command`:

* `cSetDrinkType`

## New Commands: Add/Remove Mug

Implement `Commands` for the following functions, including sensible
`Require` callbacks:

### Add Mug

We add a mug to the `Machine` using the `addMug` function:

```haskell
addMug :: MonadIO m => Machine -> m (Either MachineError ())
```

### Take Mug

Similarly, we remove a mug from the `Machine` with the `takeMug`
function:

```haskell
takeMug :: MonadIO m => Machine -> m (Either MachineError Mug)
```
