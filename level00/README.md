# Level00

This level is a terse introduction to 'property-based testing'. 

A significant feature of property-based testing is the randomised generation of
inputs for a given test. 

Consider this function:

```haskell
addTen :: Int -> Int
```

If our spec dictates the output should be equal to the input plus ten, over the
range of 0 to 1000 inclusive. We can express that with a property test:

```haskell
prop_myFunc :: Property
prop_myFunc = property $ do
  x <- forAll $ Gen.int (Range.linear 0 1000)
  myFunc x === (x + 10)
```

By default, Hedgehog tests each property with 100 random inputs. A failing
example will trigger its shrinking process and try to find a minimal
counterexample to the property.

# Your Task

Complete the tests by filling in the various generators and pre/post-conditions.
