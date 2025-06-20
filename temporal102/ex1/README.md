# Temporal 102 exercises, in Haskell

(Once we've finished the exercises, this needs an introduction.)

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

## Exercise 1: Observing Durable Execution

During this exercise, you will:

* Launch multiple Workers and run the Workflow
* Kill one of the Workers during Workflow Execution and observe that the remaining Worker completes the execution
* Observe how Temporal preserves workflow state across worker failures

This exercise demonstrates Temporal's core value: **workflows survive infrastructure failures**.

### Setup

You'll need five terminal windows for this exercise.

### Part A: Understanding the Code

The `sayHelloGoodbyeWorkflow` in `Workflow.hs` includes:

1. **Two Activities**: Translation of "hello" and "goodbye" messages by calling an HTTP service
2. **A 10-second Timer**: Provides a window to test worker failure recovery

The timer between activities gives you time to kill a worker and observe the handoff.

### Part B: Observe Durable Execution

Before proceeding, make sure that there are no Workers running from previous exercises.

#### Step 1: Start the Temporal Server
```bash
# Terminal 1
$ temporal server start-dev
```

#### Step 2: Start the Translation Service
```bash
# Terminal 2
$ cabal run ex1:service
```

#### Step 3: Start Two Workers
```bash
# Terminal 3 - Worker 1
$ cabal run ex1:worker
```

```bash
# Terminal 4 - Worker 2  
$ cabal run ex1:worker
```

#### Step 4: Execute the Workflow
```bash
# Terminal 5 - Client
$ cabal run ex1:client <YourName> <language>
```

Replace `YourName` with your name and `language` with your preferred language code (`fr`, `es`, `de`, or `pt`).

#### Step 5: Kill a Worker During Execution

1. **Watch the worker terminals** - observe which worker picks up the workflow
2. **As soon as you see the first activity complete**, **immediately press Ctrl+C** in that worker's terminal to kill it
3. **Watch the remaining worker** - it should take over and complete the workflow
4. **Check the translation service logs** - you'll see it only translates "hello" once, even during worker failover
5. **Check the Temporal UI** at http://localhost:8233 to see the execution timeline

### Expected Results

You should observe:
- Translation service logs the "hello" translation request
- First worker handles the "hello" translation
- 10-second timer starts
- **Worker dies** (you killed it)
- **Second worker takes over**
- Second worker handles the "goodbye" translation  
- Workflow completes successfully
- **No duplicate work** - translation service shows "hello" was only translated once

The workflow execution completes successfully despite the worker failure, demonstrating Temporal's durable execution capabilities.

---

This is the end of Exercise 1.
