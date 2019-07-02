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

Solutions are on the `solutions` branch, one commit per level.

## Running tests with `ghcid`

[`ghcid`](https://github.com/ndmitchell/ghcid) is a helpful tool that
helps to automate the 'edit-save-build' workflow. `ghcid` can also
execute code or perform other actions after a successful build. In
this case we're going to setup `ghcid` to run our tests whenever our
code is in a buildable state.

1) Create a `dev.ghci` script in the root of this project to prepare our repl
and load the required modules:

```
:set -isrc:levelNN
:load levelNN/Main.hs
```

The first line indicates which folders `ghci` will include when looking for any
of our code. We can provide multiple directories by providing a colon (`:`)
separated list.

The second line loads the module that contains the function we want to execute
when the code is buildable. In this case it is `main :: IO ()` function that
runs our tests.

2) Tell `ghcid` to use our `dev.ghci` and also what command to run when
everything is 'All good'. Additionally we will instruct `ghcid` to ignore
warnings:

```shell
$ ghcid -c 'ghci -ghci-script="dev.ghci"' --test=:main -W
```

We use `ghci` instead of `cabal new-repl` so we can provide the `dev.ghci` to
setup our repl environment. The `--test=:main` is the repl command that will be
executed. Finally `-W` tells `ghcid` to ignore any warnings from compilation.

## Running with stack

You will need to run a few additional commands when using [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
# Initialise
$ stack init

# Replace `level01` with the level you are working on
$ stack test :level01

# REPL, if you want it
$ stack ghci :level01
```

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
5. Lensy Models
