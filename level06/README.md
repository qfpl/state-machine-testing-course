# Level 06 - Test-Driven Development

State machine testing works with test-driven development, so let's try
that.

These automatic drink machines often draw on a reservoir of water
that's refilled manually, so we'll need a few `Command`s:

* One to refill the reservoir,
* One to check its current level, and
* A "sad path" `Command` that fails to vend when there's no water.

For the actual implementation of the machine, let's say that it starts
with three units of water, each drink takes one unit, and the refill
command takes it back to three units. You should add any new functions
as no-ops first, and let the tests "force" you into implementing them.
