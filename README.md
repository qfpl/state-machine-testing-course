# State Machine Testing

This is in progress course material for a course on property-based state machine
testing, using the [Hedgehog package](https://hackage.haskell.org/package/hedgehog).

## Requirements

- Written enough Haskell code that `Functor`, `Applicative`, and `Monad` are all
  familiar.
- Tried property-based testing or are familiar with what it is.
- Like your own automated QA department to provide greater assurance about the
  workings of your complicated application(s).

## Course Structure

- Setting Up
  - Preparing the project
  - Setting [hedgehog](https://hackage.haskell.org/package/hedgehog)
  - Structuring your tests using [tasty-hedgehog](https://hackage.haskell.org/package/tasty-hedgehog)

- First tests
  - Terminology and `Command` structure
  - Some simple commands
  - Discussion of test feedback and interpreting errors

- More Commands
  - Commands with arguments
  - We broke it! Oh no.
  
- Flexing the model
  - `Require`
  - Better model transition coverage with positive and negative tests.
