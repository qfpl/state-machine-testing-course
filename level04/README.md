# Level 04 - Positive & Negative Testing

In a previous level you implemented commands with pre-conditions to
ensure they are only run when they are guaranteed to succeed. However
this only tests the 'positive' path within the application. To be
thorough we need commands that do the 'wrong' thing. These 'negative'
commands should use `Ensure` callbacks to verify that errors are
correctly raised, and that the state doesn't change in unexpected
ways.

In this level you will write negative commands to complement your
existing positive ones.

Some negative operations to test:

* Adding milk or sugar to a hot chocolate
* Taking a mug that isn't there
* Adding a mug when one is there

Some of these operations don't change the state of the "real system"
being tested, while others return `Left MachineError`. You will have
to write appropriate `Update` and `Ensure` callbacks, depending on the
behaviour of the command being tested.

# Your Task

Think over the commands you have written so far. For each command,
decide whether you should write a negative command to contemplate it,
and implement any necessary negative commands.
