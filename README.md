# State Machine Testing

This is in progress course material for a course on property-based state machine
testing, using the [Hedgehog package](https://hackage.haskell.org/package/hedgehog).

## Requirements

- You would like an automated minion to unleash hell on your application,
  breaking it in strange and fascinating ways.

## What is State Machine Testing?

State Machine Testing extends property-based testing to provide a
toolkit for building randomised tests of stateful systems. Like
property-based testing, state machine tests use random generators to
create test cases and shrink failing tests to minimal
counter-examples. The difference with state machine testing is that
the random input is now a sequence of commands to perform instead of
arguments to pure functions.

In this course, we'll be using
[`hedgehog`](hackage.haskell.org/package/hedgehog)'s state machine
testing. The QuickCheck ecosystem has its own
[`quickcheck-state-machine`](https://hackage.haskell.org/package/quickcheck-state-machine)
package which we won't cover.

How do we know that our stateful system is behaving itself? We build a
model of the system being tested, and use it in a few ways:

* Not all actions make sense at all times (e.g., what should happen if
  you try to log in when you're already logged-in?). When hedgehog
  generates a command sequence, we update the model being tested and
  use it to limit the actions we generate.

* When we run tests, we perform commands both on the model and the
  system being tested, and check that their results agree.

## Repository Structure

The system we're testing is a vending machine for hot drinks, defined
in `src/CoffeeMachine.hs`. You can select which drink you'd like,
insert or remove a mug, add milk or sugar, insert coins and dispense a
beverage. We'll be testing these features at different levels of the
course.

The course itself is broken apart into several levels. Because this is
a course about testing, each level is a separate `test-suite` in the
`.cabal` file, and its own directory.

## Course Structure

0. Setting Up
  - Preparing the project
  - Setting [hedgehog](https://hackage.haskell.org/package/hedgehog)
  - Structuring your tests using [tasty-hedgehog](https://hackage.haskell.org/package/tasty-hedgehog)
1. First tests
  - Terminology and `Command` structure
  - Some simple commands
  - Discussion of test feedback and interpreting errors
2. `Require` & Pre-conditions
3. More Commands
4. Positive & Negative Testing
5. Phases and growing models
