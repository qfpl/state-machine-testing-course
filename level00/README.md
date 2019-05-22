# Level00

This level is intened to serve as a terse introduction to 'property-based testing'. 

A significant feature of property-based testing is the randomised generation of
inputs for a given test. 

As an example, if we wanted to test a function:

```haskell
addTen :: Int -> Int
```

Using every `Int` between 0 and 1000, inclusive. Checking that the result is
equal to the input, plus 10:

```haskell
prop_myFunc :: Property
prop_myFunc = property $ do
  x <- forAll $ Gen.int (Range.linear 0 1000)
  myFunc x === (x + 10)
```

This will generate 100 test cases (Hedgehog default) and check that your
post-condition holds in all cases. A failing example will trigger the shrinking
process.

# Task

Complete the tests by completing the various generators and pre/post-conditions.
