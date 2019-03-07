# Level 02 - Pre-conditions & Commands with arguments

So far they have all separate parts of our model. There are no dependencies so
Hedgehog is free to generate and shrink as it chooses, any sequence of commands
is as valid as any other.

But we need a way to ensure that some commands will only run when it makes sense
to do so. Otherwise the randomised aspect of our tests is no longer beneficial
and we're left with a non-deterministic test suite. All because some commands
have been included in an absurd order.

We will solve this by using the `Require` callback to inform hedgehog that a
given command is to be included. We will use the `takeMug` function as an
example.

Including a `Command` for this function without the appropriate pre-condition
would result in our tests passing only when this command was included after a
call to `addMug`, with no other `takeMug` being called in between.

However if we expanded our model to track the status the mug. Then we could
prevent the `takeMug` command from being included as a valid command unless our
criteria had been satisified. Later on we'll go into more detail about how
adding these smaller "state transitions" to your model can make things easier.

The goals for this level are to implement the following commands:

### Add Mug

We add a function using the oddly named function:

```haskell
addMug :: MonadIO m => Machine -> m (Either MachineError ())
```

### Take Mug

Similarly, we may remove a mug by calling the following function:

```haskell
takeMug :: MonadIO m => Machine -> m (Either MachineError Mug)
```

### Collapse the `DrinkType` commands to one

We're able to express the individual commands for setting the different types of
drink in a more condensed manner. Collapse the following three commands:

* `cSetDrinkHotChocolate`
* `cSetDrinkCoffee`
* `cSetDrinkTea`

To one single command:

* `cSetDrinkType`
