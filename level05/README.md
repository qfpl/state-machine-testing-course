# Level 05 - Lensy Models

You may have noticed that in the process of building the model and
various commands, we're capturing more and more information in our
model. This can become unwieldy - each command passes a bunch of
information around that it just doesn't care about. Records alleviate
this somewhat, but larger test suites can have state machine tests
across multiple model types. We want to write each command once, only
caring about the fields that we manipulate.

Enter classy lenses. The basic idea is that we create a typeclass
whose name begins with `Has`, that provides one or more useful lenses:

```haskell
class HasCoins (s :: (Type -> Type) -> Type) where
  coins :: Lens' (s v) Int
```

We then define an instance for our `Model`:

```haskell
instance HasCoins Model where
  coins f m = f (_modelCoins m) <&> \x -> m { _modelCoins = x }
```

(Aside: `lens` has a Template Haskell function called `makeClassy`
that won't do what we want: we need a kind for `s` that is compatible
with `Model`. `makeClassy` will also create one typeclass for the
entire record, and a lens for each field. We want a few classes for
different subsets of fields. It is very useful in other contexts,
though!)

Once you have the classes and instances, you can refactor the commands
to not depend on a specific `Model` type.

## Your Task

Define the `HasMug` typeclass. Define instances `HasDrinkConfig Model`
and `HasMug Model`. Refactor all the commands to not depend directly
on the `Model`, but instead access and update via classy lenses.

If you have completed the task, you should be able to remove the
`$(makeLenses ''Model)` call.
