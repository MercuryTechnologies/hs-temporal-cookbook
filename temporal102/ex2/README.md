# Temporal 102 exercise 2, in Haskell

## Running the exercises

As with other Temporal cookbook environments, you'll start a nix shell
with `nix develop --accept-flake-config` in three terminals, two where
you plan to run code and one where you'll run the Temporal server. In
the last one, run:

```bash
$ temporal server start-dev
```

Exercises that don't require the time-skipping server often run fine if
you happen to be running a different Temporal server, perhaps for local
product development. For Temporal 102 exercises with tests, make sure
you're running the server from the Nix shell.

This will start a long-running control plane server that will execute
workflows against workers you write.

## Exercise: Testing Temporal Code

During this exercise, you will:

- Run a unit test for the `translateActivity` activity
- Write and run your own unit test for `translateActivity`
- Write assertions for a Workflow test
- Uncover, diagnose, and fix a bug in the Workflow Definition
- Observe the time-skipping feature in the Workflow test environment

### Part A: Running a Test

We have provided a test for the `translationActivity` to get started.
This test verifier that the Activity connects to a webservice and
translates the term "Hello" into German. Take a moment to study the
first test in `tests/ActivitySpec.hs`. Since the test runs an Activity,
which calls a service, start by running the service:

In one terminal, run:

```bash
$ cabal run ex2:service
```

In another, run:
```bash
$ cabal test
```

(If the worker logs are annoying you, you can banish them with

```bash
$ cabal test 2>/dev/null
```

### Part B: Write another test

Next, write and run your own unit test. Edit `tests/ActivitySpec.hs` and
copy the first test case, changing the term to `"Goodbye"` and the
language to Latvian (`lv`). Assert that the activity returns
`"ardievu"`.

With the service running, run the test suite:

```bash
$ cabal test
```

and observe your test passing.

### Part C: Test the Activity with invalid input

It's useful to test the sad path as well as the happy path. We can do
this with the following test case:

```haskell
it "throws an exception when language code isn't recognized" $ do
  mockEnv <- mkMockActivityEnvironment ()
  let input = TranslateTermInput "Hello" "xq"
  let act = runMockActivity mockEnv $ translateActivity input
  act `shouldThrow` 
    (\e -> case e of
             ApplicationFailure "TranslationError" _ _ _ _ _ -> True
             _ -> False
        )
```

In the Haskell Temporal SDK, Activities signal failure by throwing an
`ApplicationFailure` exception. Take a moment to look it up in
[Hoogle](https://hoogle.haskell.org/), then study the above code and the
error-handling in `Workflow.hs`. Paste the above test case in to
`tests/ActivitySpec.hs`, then modify the selector on the right-hand side
of `shouldThrow` to check for a status code of 400 (hint: it's part of
the text in the second field, `message`, on `ApplicationFailure`).

### Part D: Test a Workflow Definition

Edit the `tests/WorkflowSpec.hs` file. Notice that the `spec` function
wraps a helper function with a server, worker, and client that we
construct for the test suite in particular. The meat of the test is in
`workflowSpec`, which executes a Workflow in the context of a Client
just like you've seen in many `Client.hs` implementations to date.

Replace the `pending` call with assertions for the following conditions:

- `output.helloMessage` should be `bonjour, Pierre`
- `output.goodbyeMessage` should be `au revoir, Pierre`

Run the test suite:

```bash
$ cabal test
```

The workflow test will fail (there's a bug in the workflow). Find and
fix the bug, run the test suite again, and verify that the bug's fixed.

Note two things about this exercise:

- The test completes quickly, even though the Workflow Definition
  contains a ten-second `sleep`. This is the time-skipping feature in
  action.
- The test defines a Worker that registers Workflows and Activities the
  same way as `Worker.hs`, so it runs the Activity Definitions from
  `Workflow.hs`. If we want to test the Workflow in isolation, we can
  use a mock Activity environment to stub out that activity, something
  we'll examine in the optional exercise.

### Optional: Using Mock Activities in a Workflow Test

If you have time, continue with the following steps:

- Uncomment the `MockWorkflowSpec` entries in `ex2.cabal` and
  `tests/Spec.hs`.
- Study `tests/MockWorkflowSpec.hs`. Note that the first two functions
  look very familiar, but then we construct a worker without
  `discoverDefinitions`, instead providing an explicit activity and
  workflow, defined below.
    - The workflow directly calls the `sayHelloGoodbyeWorkflow` function
      that implements our workflow
    - The activity calls a pure function that returns stubbed results
- Exit the translation service with ctrl-C in the terminal where it's
  running. Since the other tests called the service through the Activity
  from `Workflow.hs`, we expect them to fail without a running service.
- Run `cabal test` and watch the stubbed workflow pass while the others
  fail.
