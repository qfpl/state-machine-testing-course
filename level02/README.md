# Level 02 - Pre-conditions & Commands with arguments

We've built some simple `Command`s to ensure we can keep selecting different
drink types and our machine doesn't explode. Yay. Note that because of how we've
built these `Command`s, any sequence of `Command`s is as valid as any other.
Which is okay when particular actions may be run regardless of what state the
system is in.

What about when a `Command` does not make sense to run unless the system has
reached a particular state? If we do not manage this, the randomised aspect of
our tests is no longer beneficial. We're left with a non-deterministic test
suite because some `Command`s _might_ be included in an absurd order.

We solve this by using the `Require` callback to inform Hedgehog that this
`Command` is valid in the current sequence. We will use the `takeMug` function
as an example.

Including a `Command` for this function without the appropriate pre-condition
would result in our tests passing only when this `Command` was included after a
call to `addMug`, with no other `takeMug` being called in between.

However if we expanded our model with a value to track the status of the mug.
Then we can prevent the `takeMug` `Command` from being included unless our
criteria have been satisified.

Later on we'll go into more detail about how adding these smaller "state
transitions" to your model can make things easier.

The goals for this level are to implement the following commands:

### Add Mug

We add a mug using the oddly named function:

```haskell
addMug :: MonadIO m => Machine -> m (Either MachineError ())
```

### Take Mug

Similarly, we may remove a mug with the function:

```haskell
takeMug :: MonadIO m => Machine -> m (Either MachineError Mug)
```

### Collapse the `DrinkType` commands to one

We're able to express the individual `Command`s for setting the different types
of drink in a more condensed manner. Collapse the following three `Command`s:

* `cSetDrinkHotChocolate`
* `cSetDrinkCoffee`
* `cSetDrinkTea`

To a single `Command`:

* `cSetDrinkType`
