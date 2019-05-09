# Level 06 - Test-Driven Development and `Var`s

State machine testing works with test-driven development, so let's try
that. Suppose that our users are complaining about having to configure
the drink of their choice each time they visit the machine. Who can
blame them? It's not easy to acquire caffeine before your first cup.

Let's make life a bit easier for our "users" by adding a feature to
save and restore preferences. In true TDD fashion, let's write a
`Command` that represents the act of saving the current drink
configuration, and gives the user a token that restores their
preferences. Then let's write a `Command` that resets the machine to
its previous setting.

If you try to write this command with the techniques you currently
have, you will find a problem: you cannot predict the token the
machine will give, but Hedgehog wants to generate full command
sequences before executing any of them. To solve this problem, we must
look closely at the types.

Hedgehog provides a type `Var`, with the following definition: `data
Var a v = Var (v a)`. Yes, this is just a re-ordering of type
variables. It's so that `Var` is `HTraversable`. Depending on the
phase of the test, `v` will be either `Symbolic` (during generation)
or `Concrete` (during execution). A `Var a Symbolic` is essentially an
opaque token that can be sensibly compared for equality and not much
else. It has an `Ord` instance, but it compares the internal token
numbers and has nothing to do with the underlying `a`. Good enough to
use as a `Data.Map.Map` key, but not that much else. A `Var a
Concrete` is essentially just the actual `a` at execution time.

This means that our `Model v` can store a `Map (Var PreferenceToken v)
Drink`, and track which drinks come from which setting. But how do we
get a `Var PreferenceToken v` to insert as the key?

The answer is in the type of the `Update` callback:

```haskell
Update
  :: forall v. Ord1 v
  => state v
  -> input v
  -> Var output v
  -> state v
  -> Callback input output state
```

The type variable `output` is fixed by the return value of the
`Command`'s `commandExecute` field, and Hedgehog will give our
`Update` callback a symbol representing that result when it constructs
the test sequence.

## Your Task

Write the following `Command`s, and implement the required
functionality in `src/CoffeeMachine.hs`:

* `cSavePreferences`: Save the current state of the machine, and
  return a `PreferenceToken`.

* `cRestorePreferences`: Given a `PreferenceToken`, restore the saved
  drink settings.

Also define a `cReset` `Command`, that resets the machine. See if this
gives you any surprising results.

Set up the types so that it is impossible to fabricate a
`PreferenceToken` that didn't come from a machine. This makes it less
important to write positive and negative variants of the "restore
preferences" command.

If you still want to write positive and negative variants of the
"restore preferences" command, you'll want to write a testing function
that makes the machine generate a `PreferenceToken` that corresponds
to no saved preference.
