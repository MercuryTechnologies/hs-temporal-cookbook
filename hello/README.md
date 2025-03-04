# Hello, World!

These examples demonstrate basic workflow & activity features.

## Examples

Most examples assume a Nix development shell and running Temporal development
server; see the [top-level README's usage section](../README.md#usage) for
details.

All of these examples can be run via `cabal` using their executable name; for
example `cabal run hello:activity` runs `Activity.hs`, `cabal run hello:cron`
runs `Cron.hs`, and so on.

- [activity](./Activity.hs) - define an activity and execute it from a workflow
- [activity_with_boilerplate](./ActivityWithBoilerplate.hs) - same as above, without using Template Haskell convenience utilities
- [child_workflow](./ChildWorkflow.hs) - execute a workflow that spawns a child workflow
- [cron](./Cron.hs) - execute a workflow once per minute

