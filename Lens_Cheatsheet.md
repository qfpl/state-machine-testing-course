# Lens - A Cheatsheet

We will be using lenses in the content of the course, and we encourage you to
use them as well. Primarily we will be applying lenses in their simplest form,
that is as generalised "getters" and "setters".

This document will contain examples and explanations to provide an easily
digestible to the lenses that are used in this course:

Given the following Haskell record:

```haskell
data Foo = Foo
  { _fooFieldA :: Text
  , _fooFieldB :: Int
  }
```

### Generating the lenses

We will use Template Haskell to write the code for us. We only need to enable
the `TemplateHaskell` language extension and add the following line below our
type:

```haskell
$(makeLenses ''Foo)
```

This will generate the following lenses for us:

```haskell
fooFieldA :: Lens' Foo Text
fooFieldB :: Lens' Foo Int
```

Note that the functions are named exactly as our field names on `Foo`, save for
removing the initial `_` prefix. This is merely a convention from the `lens`
package. The name transformation may be changed by providing additional options
to the template haskell functions.

Using the lenses we generated earlier and given the following value:

```haskell
let foo = Foo "Fred" 33
```

### As 'getters'

| Function/Operator | Example                             |
| `view`            | `view fooFieldA foo == "Fred"`      |
| `^.`              | `foo ^. fooFieldA == "Fred"`        |
| `to`              | `foo ^. fooFieldA . to length == 4` |

### As 'setters'

| Function/Operator | Example                                              |
| `set`             | `set fooFieldA "Sally" foo == Foo "Sally" 33`        |
| `.~`              | `foo & fooFieldA .~ "Sally" == Foo "Sally" 33`       |
| `+~`              | `foo & fooFieldB +~ 3 == Foo "Fred" 36`              |
| `over`            | `over fooFieldA (fmap toUpper) foo == Foo "FRED" 33` |
|                   |                                                      |

