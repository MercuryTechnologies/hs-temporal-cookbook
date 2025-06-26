# Code Repository for Temporal 102 (Haskell)

This repository provides code used for exercises and demonstrations
included in the (unofficial) Haskell version of the [Temporal
102](https://learn.temporal.io/courses/temporal_102) training course.

This is teaching code, not production code. It is designed to
demonstrate particular aspects of Temporal, not to serve as a template
for a production-ready system.

For the exercises, make sure to enter a Nix shell whenever you're
opening terminals to run various components:

```bash
$ nix develop --allow-flake-config
```

## Hands-On Exercises

- [ex1/](ex1): Observing durable execution with Temporal
- [ex2/](ex2): Testing Temporal code
- [ex3/](ex3): Debugging an Activity

## Example for Self-study

- [samples/age-estimation/](samples/age-estimation): Calling a remote
  API
- The Haskell exercises tend to use objects for data transfer across the
  board so the Using Objects for Data example has not been included
