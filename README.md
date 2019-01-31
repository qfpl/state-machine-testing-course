# State Machine Testing

This is in progress course material for a course on property-based state machine
testing, using the [Hedgehog package](https://hackage.haskell.org/package/hedgehog).

## Requirements

- You would like an automated minion to unleash hell on your application,
  breaking it in strange and fascinating ways.

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
  - Better coverage with positive and negative tests.
