# Level 04 - Positive & Negative Testing

In a previous level you implemented commands with pre-conditions to ensure
they are only run when it is appropriate. However this only tests the 'positive'
path within the application. To be thorough we need to add commands that may be
interleaved and check what happens if we execute a command and our desired
requirements have not been satisfied. In this level you will write commands to
ensure both the positive and negative paths are tested.

<!-- We're starting to build up an nice list of commands. So far they have all -->
<!-- separate parts of our model. There are no dependencies so Hedgehog is free to -->
<!-- generate and shrink as it chooses, any sequence of commands is as valid as any -->
<!-- other. -->

<!-- But we need a way to ensure that our commands will only run when it makes sense -->
<!-- to do so. Otherwise we will have commands that execute and fail our tests when -->
<!-- those commands should never have been included to begin with. -->

<!-- We will use the `takeMug` function as an example. If we were to include a -->
<!-- `Command` for this function without an appropriate `Require`, our tests would -->
<!-- pass randomly, only when this command happened to be included in the right -->
<!-- order. -->

<!-- However if we expanded our model to track whether we had added a mug. Then we -->
<!-- could prevent the `takeMug` command from being a valid command unless our -->
<!-- criteria had been satisified. -->

<!-- The type of drink we have selected is another example of this. If we wanted to -->
<!-- check that the buttons to add milk or sugar were working correctly, then we need -->
<!-- to make sure we have the correct type of drink selected. -->

<!-- The goal for this level is to implement the following commands: -->

<!-- * Add Milk/Sugar -->
<!-- * Add Mug -->
<!-- * Take Mug -->

<!-- Be sure to check both the positive (happy) and negative (sad) paths! You will -->
<!-- have to ensure that your `Command`s have the correct pre-conditions and -->
<!-- `Require` checks. -->
