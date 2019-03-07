# Level 02 - Pre-conditions & Commands with arguments

So far they have all separate parts of our model. There are no dependencies so
Hedgehog is free to generate and shrink as it chooses, any sequence of commands
is as valid as any other.

But we need a way to ensure that some commands will only run when it makes sense
to do so. Otherwise the randomised aspect of our tests is no longer beneficial
and we're left with a non-deterministic test suite. All because some commands
have been included in an absurd order.

Let's use the `takeMug` function as an example.

If we were to include a `Command` for this function without an appropriate
pre-condition to check that we had, at somepoint, added a mug. Our tests would
pass only when this command happened to be included after a call to `addMug`,
with no other `takeMug` being called in between.

However if we expanded our model to track whether we had added a mug. Then we
could prevent the `takeMug` command from being a valid command unless our
criteria had been satisified. Later on we'll go into more detail about how
adding these smaller "state transitions" to your model can make things easier.

The goals for this level are to implement the following commands:

* Add Mug
* Take Mug
* Reimplement the `DrinkType` commands from Level01 in a single command
