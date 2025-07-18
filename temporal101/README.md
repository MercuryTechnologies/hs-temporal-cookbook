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
is part of the workflow type name. This is provided by the Template
Haskell helpers; if you manually define a workflow it won't have this
annotation by default.

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

## Service Workflow Demo

To invoke operations with side effects, we need to run an Activity. This
demo defines an Activity in `Exercise3.hs` (spoiler warning) that calls
a local HTTP service to obtain a Spanish greeting for a specified name.
The same Haskell file also defines a Workflow to call the Activity, a
worker, and a client that invokes the workflow.

Inspect `Exercise3.hs` before continuing.

### Part A: Run the service

In a terminal in the nix shell, run:

```bash
$ cabal run temporal101:helloserver
```

### Part B: Run the workflow

In another terminal, run:

```bash
$ cabal run temporal101:exercise3
```

### Part C: Inspect the results

In a web browser, visit `localhost;8233` and examine the workflow you
just invoked.

## Exercise 3: Farewell Workflow

`HelloServer` has a `/goodbye` endpoint as well as a `/hello` endpoint.
We're going to write an activity to call it.

### Part A: Create a new activity

Open `Exercise3.hs` and copy the `getSpanishGreeting` activity to
something like `getSpanishFarewell`. Change it to call the `/goodbye`
endpoint instead of `/hello`. Register it with
`Temporal.TH.registerActivity`.

### Part B: Change the workflow to call your new activity

Change `greetingWorkflow` to execute the new activity you created.

### Part C: Run the greeting service and the workflow

In one terminal in the nix shell, run:

```bash
$ cabal run temporal101:helloserver
```

In another, run:

```bash
$ cabal run temporal101:exercise3
```

### Part D: Inspect the results

Open a web browser to `localhost:8233`. Find the workflow that just ran
and inspect the results. Verify that it called the correct endpoint.

## Exercise 4: Multiple implementation platforms

In this exercise, we define a workflow (and a worker to run it) in
Haskell, and use it to invoke an activity that's implemented in Java
(with a worker to run it) and provided in an opaque `.jar` file.

### Part A: Inspect the Haskell

Open `Exercise4.hs` and note how it references the `CreatePdf` activity.
Beyond the use of `KnownActivity`, the rest of the workflow and worker
machinery should look familiar. Change the name in the `CreatePdfInput`
constructor to your own.

### Part B: Start the Java worker

In a terminal in the nix shell, run:

```bash
$ java -classpath java-activity-and-worker.jar io.temporal.training.PdfCertWorker
```
### Part C: Run the Haskell worker

In another terminal, run:

```bash
$ cabal run temporal101:exercise4
```

### Part D: Inspect the fruits of your labor

Open a web browser to `localhost:8233` and examine the execution of the
workflow you just ran.

In the `temporal101` directory, you should find a PDF. Open that in the
web browser and enjoy.

## Extra Study: Retry policies

Part of Temporal's value proposition is durable execution in the face of
temporary failure. Let's look at an example, based around an artificial
failure on one of `HelloServer`'s endpoints.

### Part A: Inspect the Haskell

Open `RetryPolicy.hs` and note how the activity throws an
`ApplicationFailure` to signal its failure. Note also the retry policy
we configure to run the activity.

### Part B: Run the code as-is and examine the failed workflow

In a terminal in the nix shell, run the http service:

```bash
$ cabal run temporal101:helloserver
```

In another terminal, run the retry worker:

```bash
$ cabal run temporal101:retrypolicy
```

Once the latter has completed, open a web browser to `localhost:8233`
and examine the failed workflow. Note that it's annotated with the
number of retries and other metadata about the retry policy in use.

### Part C: Set an improved retry policy

Examine `HelloServer.hs`, then update the `retryPolicy` in
`RetryPolicy.hs` to something you'd expect to succeed.

### Part D: Rerun the experiment and examine the results

If it's still running, ctrl-C out of `helloserver` and start it again.
Run the retry worker again, and examine the results in the web UI.
