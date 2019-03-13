# Lens - A Cheatsheet

We will be using lenses in the content of the course, and we encourage
you to use them as well. A `Lens' s a` accesses exactly one `a` within
some structure `s`.

This document will contain enough examples to provide a cheat-sheet
for the lens/prism-using code in this course:

## Generating Lenses

We can generate lenses for the following Haskell record using `{-#
LANGUAGE TemplateHaskell #-}` and one line of code:

```haskell
data Foo = Foo
  { _fooFieldA :: Text
  , _fooFieldB :: Int
  }

$(makeLenses ''Foo) -- Make me some lenses!
```

This will generate the following lenses for us:

```haskell
fooFieldA :: Lens' Foo Text
fooFieldB :: Lens' Foo Int
```

These lenses pick out a `Text`/`Int` from a `Foo`, or can be used to
update a `Text`/`Int` inside a `Foo`. Note that the lenses are named
after field names on `Foo`, minus the initial `_` prefix. This
convention is expected by `makeLenses`. It is possible to configure
it, but few do.

## Using Lenses

Using the lenses we generated earlier and given the following value:

```haskell
let foo = Foo "Fred" 33
```

### As 'getters'

| # | Function/Operator                            | Example                                  |
|---|----------------------------------------------|------------------------------------------|
| 1 | `view`                                       | `view fooFieldA foo           == "Fred"` |
| 2 | `^.` (infix `view`)                          | `foo ^. fooFieldA             == "Fred"` |
| 3 | `to` (turn a nomal function into a "getter") | `foo ^. fooFieldA . to length == 4`      |

#### Non-lens equivalents:

| #   | Example                             |
|-----|-------------------------------------|
| 1,2 | `_fooFieldA foo          == "Fred"` |
| 3   | `length (_fooFieldA foo) == 4`      |

### As 'setters'

| # | Function/Operator  | Example                                               |
|---|--------------------|-------------------------------------------------------|
| 1 | `set`              | `set fooFieldA "Sally" foo         == Foo "Sally" 33` |
| 2 | `.~` (infix `set`) | `foo & fooFieldA .~ "Sally"        == Foo "Sally" 33` |
| 3 | `+~`               | `foo & fooFieldB +~ 3              == Foo "Fred" 36`  |
| 4 | `over`             | `over fooFieldA (fmap toUpper) foo == Foo "FRED" 33`  |

#### Non-lens equivalents:

|   # | Example                                                                |
|-----|------------------------------------------------------------------------|
| 1,2 | `foo { _fooFieldA = "Sally" }                       == Foo "Sally" 33` |
|   3 | `foo { _fooFieldB = _fooFieldB foo + 3 }            == Foo "Fred" 36`  |
|   4 | `foo { _fooFieldB = fmap toUpper (_fooFieldA foo) } == Foo "FRED" 33`  |

# Prisms

Prisms are like lenses for sum types. Instead of focusing on exactly
one value, a `Prism' s a` will extract at most one `a` from `s`.

## Generating Prisms

Prisms are generated using the `makePrisms` template haskell function:

```haskell
data Bar = Baz Foo | Quux Bool

$(makePrisms ''Bar)
```

This will generate prisms `_Baz :: Prism' Bar Foo` and `_Quux ::
Prism' Bar Bool`.

### Using Prisms

Given the two prisms above, and the following values:

```haskell
let
  baz = Baz (Foo "Fred" 33)
  quux = Quux True
```

#### Previewing

| # | Function/Operator        | Example                                    |
|---|--------------------------|--------------------------------------------|
| 1 | `preview`                | `preview _Baz baz == Just (Foo "Fred" 33)` |
| 2 | `preview`                | `preview _Baz quux == Nothing`             |
| 3 | `(^?)` (infix `preview`) | `quux ^? _Quux == Just True`               |
| 4 | `(^?)` (infix `preview`) | `baz ^? _Quux == Nothing`                  |

#### Reviewing

| # | Function/Operator      | Example                     |
| 1 | `review`               | `review _Quux True == quux` |
| 2 | `(#)` (infix `review`) | `_Quux # True == quux`      |

# Composing

`Lens`es (and other optics like `Prism`s) compose with each other
using `(.)` giving an effect that looks a lot like dot-notation in
other languages. This composition is where the payoffs really start,
as it's possible to reach deep into data structures without doing many
levels of `case`-expressions:

```haskell
baz ^? _Baz . fooFieldA == Just "Fred"
```
