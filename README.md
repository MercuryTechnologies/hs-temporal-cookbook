# (Unofficial) Haskell Temporal SDK Cookbook

This is a set of examples for the [(unofficial) Haskell SDK](https://github.com/MercuryTechnologies/hs-temporal-sdk/).

## Training

This repository provides code used for exercises and demonstrations included in
the Haskell adaptation of the [Temporal 101] and [Temporal 102] training courses.

> [!IMPORTANT]
> Keep in mind that the example code used in this course was designed to
> support learning a specific aspect of Temporal, not to serve as a
> ready-to-use template for implementing a production system.

[Temporal 101]: https://learn.temporal.io/courses/temporal_101/
[Temporal 102]: https://learn.temporal.io/courses/temporal_102/

- [Temporal 101](./temporal101/README.md)
- [Temporal 102](./temporal102/README.md)
  - Hands-On Exercises
    - [Exercise 1 - Observing Durable Execution](./temporal102/ex1/README.md)
    - [Exercise 2 - Testing the Translation Workflow](./temporal102/ex2/README.md)
    - [Exercise 3 - Debugging and Fixing an Activity Failure](./temporal102/ex3/README.md)
  - Examples for Self-Study
    - [Age Estimation](./temporal102/samples/age-estimation/README.md)

## Examples

Most examples assume a Nix development shell and running Temporal development
server; see [the usage section](#usage) below for details.

- [hello](./hello) - basic examples
  - [activity](./hello/Activity.hs) - define an activity and execute it from a workflow
  - [activity_with_boilerplate](./hello/ActivityWithBoilerplate.hs) - same as above, without using Template Haskell convenience utilities
  - [child_workflow](./hello/ChildWorkflow.hs) - execute a workflow that spawns a child workflow
  - [cron](./hello/Cron.hs) - execute a workflow once per minute

## Usage

### Prerequisites

This cookbook depends on [Nix]; if you don't already have Nix installed, please
[follow these instructions](https://lix.systems/install/#on-any-other-linuxmacos-system)
to do so.

[Nix]: https://www.lix.systems

### Running the Examples

> [!WARNING]
> `--accept-flake-config` should **never** be passed to a `nix` command that
> executes untrusted code; a malicious actor could use it to
> [execute commands as root](https://github.com/NixOS/nix/issues/9649)!
>
> Always validate the `nixConfig` in a project's `flake.nix` beforehand; in
> this case, we're using it to provide cached [Garnix CI](https://garnix.io/)
> artifacts.

Unless otherwise specified, all examples require a running Temporal development
server; drop into a Nix development shell and spawn one:

```bash
$ nix develop --accept-flake-config
$ temporal server start-dev
```

The development server web UI should now be available at `localhost:8233`.

Separately, drop into a Nix development shell from which the examples may be
compiled and executed; to run the basic activity example:

```bash
$ nix develop --accept-flake-config
$ cabal run hello:activity_with_boilerplate
```

Upon completion, navigate to the web UI and observe that a workflow of type
`greeting` has completed:

<img title="screenshot" alt="Temporal web UI showing workflow execution" src="./assets/greeting_workflow_example.png" />
