# Temporal 101 exercises, in Haskell

This directory contains several exercises that will help you learn the
basics of writing applications with Temporal, based on the [Temporal
101: Introducing the Temporal
Platform](https://learn.temporal.io/courses/temporal_101/) courses.

## Running the exercises

As with other Temporal cookbook environments, you'll start a nix shell
with `nix develop --accept-flake-config` in two terminals, one where you
plan to run code and one where you'll run the Temporal server. In the
latter, run:

```bash
$ temporal server start-dev
```

This will start a long-running control plane server that will execute
workflows against workers you write.

### A note on exercise format

These exercises are structured slightly differently from the exercises
in the official courses. They follow the pattern of structuring input
with a type (rather than, say, a string) which Mercury uses as a best
practice.

## Exercise 1: Build a simple worker and workflow

Ensure that you're running a Temporal server.

### Part A: Review the workflow and worker logic

Inspect `SayHello.hs` and find the definition of `sayHelloWorkflow`.
Review its type signature. Next, review the `main` function in
`Exercise1.hs` and its pattern for creating and tearing down a worker.

### Part B: Change the task queue name for the worker

Change the name of the task queue to `greeting-tasks`.

### Part C: Start the worker

Build and run the worker with:

```bash
$ cabal run temporal101:exercise1
```

### Part D: Start the workflow from the command line

Open another nix shell in the `temporal101` directory. Run the following
command to instruct the Temporal server to run your workflow:

```bash
$ temporal workflow start \
    --type SayHello.sayHelloWorkflow \
    --task-queue greeting-tasks \
    --workflow-id my-first-workflow \
    --input '{"name": "<your name here>"}'
```

Note that the Haskell module where the workflow is defined (`SayHello`)
is part of the workflow type name.

### Part E: View the workflow's status and output

From the command line, run:

```bash
$ temporal workflow show --workflow-id "my-first-workflow"
```

Is the output what you expect?

You can examine the workflow's execution in a web interface, too. In a
browser, open `localhost:8233` and click into the workflow you just ran.
If you made a mistake in the inputs, this will make it easier to
diagnose than reading the log output of the worker in the terminal.

## Exercise 2: Start a workflow programmatically

Ensure that you're running a Temporal server.

### Part A: Review the client logic

Read through `Exercise2.hs`. Compare its `main` function to that of
`Exercise1.hs`.

### Part B: Launch the worker and execute the workflow

Run your worker from exercise 1 again:

```bash
$ cabal run temporal101:exercise1
```

In another terminal (in the nix shell), build and run exercise 2:

```bash
$ cabal run temporal101:exercise2
```

This will hang. Don't panic, leave it running.

### Part C: Inspect the running workflow

Open a web browser to `localhost:8233` and inspect the workflow you just
launched (it'll have a uuid for its workflow ID). You'll notice a status
like "no workers available for task queue `hello-worker`".

### Part D: Rename the worker's task queue to match

Ctrl-C the worker (`temporal101:exercise1`) and rename its task queue
(in `Exercise1.hs`) to `"hello-worker"`. Run it again:

```bash
$ cabal run temporal101:exercise1
```

You should see `exercise2` terminate and the workflow complete in the
Temporal web UI.
