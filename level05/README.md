# Level 05 - Divergence and Phases

You'll notice that in the process of building tests for our state machine,
we're building another state machine!

However, we must only create _'just enough'_ of a model so that Hedgehog has
the information it needs to generate sensible `Command` sequences. Whilst we
avoid creating a duplicate of the thing we're trying to test, or needlessly
exposing its inner workings.

The types aid us in achieving this as the inputs to `Require` or `commandGen`
are tagged as `Symbolic`. This prevent us from directly using output from the
thing we're testing in either the `Require` or `commandGen` sections of our
`Command`.

This is where state machine testing becomes a bit of an art. How will you
track the required knowledge about the thing you're testing so that you can
write sensible commands? Sometimes this may require implementing a `Command`
that examines and updates the model.

This style of `Command` would act as a 'transition'. Guiding the tests to
valid subsets of `Command`s. This allows you collect complicated logic for
transitions in a single location and set a single value on the model that
other `Command`s are able to check more easily.

In this level we're going to check the following steps:

* Inserting coins
* Refunding coins
* Dispensing a drink