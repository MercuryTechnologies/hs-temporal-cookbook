# Age Estimation Workflow

This example provides a Workflow that can estimate someone's age, 
based on their given name. It uses a single Activity implementation, 
which calls a remote API to retrieve this estimation.

Start the Worker:

```
cabal run age-estimation:worker
```

In addition to the Worker, it also includes a file [`Client.hs`](./Client.hs)
that you can use to run the Workflow (pass a name to use as input 
on the command line when invoking it). 

```
cabal run age-estimation:client -- Betty
```

This will output a message with the name and estimated age:

```
Betty has an estimated age of 78
```

## Testing

Additionally, this example provides tests for the Workflow 
and Activity code.

```
cabal test age-estimation:spec
```

This will spawn a temporary development server and a worker configured to
process the workflow under test.

A successful test run should produce a bunch of interleaved output from
Temporal along with the following:

```
Finished in 0.5924 seconds
3 examples, 0 failures
Test suite spec: PASS
```
