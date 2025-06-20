# Temporal 102 exercise 2, in Haskell

## Running the exercises

As with other Temporal cookbook environments, you'll start a nix shell
with `nix develop --accept-flake-config` in three terminals, two where
you plan to run code and one where you'll run the Temporal server. In
the last one, run:

```bash
$ temporal server start-dev
```

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
first test in `Spec.hs`. Since the test runs an Activity, which calls a
service, start by running the service:

In one terminal, run:

```bash
$ cabal run ex2:service
```

In another, run:
```bash
$ cabal test
```

### Part B: Write another test

Next, write and run your own unit test. Edit `Spec.hs` and copy the
first test case, changing the term to `"Goodbye"` and the language to
Latvian (`lv`). Assert that the activity returns `"ardievu"`.

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
`Spec.hs`, then modify the selector on the right-hand side of
`shouldThrow` to check for a status code of 400.

### Part D: Test a Workflow Definition (todo)
