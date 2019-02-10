# Level 05 - Divergence and Phases

You'll notice that in the process of building tests for our state machine, we're
building another state machine!

However, we must only create _'just enough'_ of a model so that Hedgehog has the
information it needs to generate sensible `Command` sequences. Whilst we avoid
creating a duplicate of the thing we're trying to test, or needlessly exposing
its inner workings.

The types aid us in achieving this as the inputs to `Require` or `commandGen`
are tagged as `Symbolic`. This prevent us from directly using output from the
thing we're testing in either the `Require` or `commandGen` sections of our
`Command`.
