# Temporal 102 exercises, in Haskell

## Running the exercises

As with other Temporal cookbook environments, you'll start a nix shell
with `nix develop --accept-flake-config`, this time in four terminals.

## Exercise 3: Debugging and Fixing an Activity Failure

During this exercise, you will

- Start a Worker and run a basic Workflow for processing a pizza order
- Inspect the workflow's execution in the web UI
- Diagnose and fix a latent bug in one of the Activity Definitions
- Test, deploy, and verify the fix

### Setup

You'll need four terminals for this exercise, running in the nix
development shell in the `temporal102/ex3` directory.

Start the Temporal server in one terminal:

```bash
$ temporal server start-dev
```

### Part A: Run the workflow

We'll start by running the workflow without looking at the code; we'll
inspect it in the web UI.

Start workers in two terminals:

```bash
$ cabal run ex3:worker
```

Run the client in a third to start the Workflow:

```bash
$ cabal run ex3:client
```

### Part B: Inspect the workflow execution in the web UI

Open the web UI at `localhost:8233` and find the detail page for the
Workflow Execution you just ran, which has the Workflow Type
`Workflow.pizzaWorkflow` and workflow ID `pizza-workflow-order-31337`.

If the detail page still shows a status of Running, wait a few seconds and refresh the page. Once the page shows a status of Completed, use what you've learned about the Event History, try to answer the following questions:

1. Did it use sticky execution for all Workflow Tasks following the first one?
   - Hint: It's possible to determine this without expanding any of the Events in the Web UI, but you may also check the WorkflowTaskScheduled Events for confirmation.
2. What is the Activity Type for the first Activity executed?
3. Which of the 2 Workers started execution of the first Activity?
   - Was it the one running in your first terminal or the second?
   - Did the same Worker complete execution of this Activity?
4. Following execution of the first Activity, which of the following happened next?
   - A) Another Activity was executed
   - B) A Timer was started
   - C) Workflow Execution failed due to an error
   - D) Workflow Execution completed successfully
5. What was the duration specified for the Timer used to delay execution?
   - Hint: this is shown as a timeout in the relevant Event
6. Find the Event associated with the Worker completing execution of the getDistance Activity
   - What is the ID for this Event?
   - What is the ID of the Event logged when this Activity was started by the Worker?
   - What is the ID of the Event logged when this Activity was scheduled by the Cluster?
7. Can you find the input data supplied as a parameter to the getDistance Activity?
8. Can you find the output data returned as output from the getDistance Activity?
9. What was the Maximum Interval value for the Retry Policy used to execute the sendBill Activity?
10. What was the Start-to-Close Timeout value used when executing the sendBill Activity?

Take a moment to switch to the Compact view, and if one of the rows in the table is expanded, click to collapse it. Do you find that this view makes it easier to see the Activities and Timer that ran during the execution?

Click "Expand All" near the upper-right corner of this table. Do you find that this helps you to correlate Events related to the Activities and Timer?

Since the Web UI remembers the current view, be sure to click "Collapse All" and switch back to the History view before continuing.

### Part C: Finding an Activity Bug

One of the pizza shop's franchise managers added a monthly special to
the code at the last minute, offering a $5 discount on all orders over
$30. (Note that the code tracks prices in cents, to avoid floating-point
error in price computations.) The test order created by the Client only
totals $27, not enough to qualify for the discount. Let's try it out:

1. Edit the `Client.hs` file, which creates the test order
2. Add a new pizza to the order: `Pizza "Medium, with extra cheese" 1300`
3. Change the order number from `31337` to something else, to avoid
   complications with duplicate workflow IDs
4. Save your changes and close your editor
5. Submit this pizza order by running the client: `cabal run ex3:client`

You will notice that the workflow doesn't complete. Keep it running, but
open the web UI at `localhost:8233` to see what has happened.

### Part D: Fixing the Activity Bug

You should see that the `billCustomer` activity is failing with an error
along the lines of "Order amount must be nonnegative". (You'll have to
click into the retrying Activity to find the exception.) Temporal will
keep retrying the Activity until you terminate the Workflow or fix the
bug, so let's fix the bug.

1. Run `cabal test`. You'll see that it passes.
2. Open `tests/ActivitySpec.hs` and examine the existing test suite.
3. Add a new test to exercise the new weekly special: namely, that
   orders above $30 get a $5 discount.
4. Rerun `cabal test`. It'll fail again because your test reproduces the
   bug in the Activity.
5. Open `Workflow.hs` and fix the bug in `billCustomer`
6. Run `cabal test` and watch the test pass.

### Part E: Deploying and verifying the fix

- Stop each worker with ctrl-C, then restart with `cabal run ex3:worker`
- The maximum retry interval is 10 seconds, so you should quickly see a
  worker pick up the Activity Task and run it to completion.

This is the end of the exercise.
