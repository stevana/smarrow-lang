# smarrow-lang

`smarrow-lang` is an experimental programming language where programs are state
machines expressed in
[arrow notation](https://www.haskell.org/arrows/syntax.html).

State machines communicate with each other by sending messages, a bit like
Erlang processes but with more restrictions. For example, a state machine may
not spawn new state machines, and its state, input and output types are
specified at compile time.

These restrictions, or extra structure, are the basis for (to be implemented)
features such as:

* Hot-code swapping;
* Type-safe state migrations;
* Input and output evolution.
* [Simulation testing](https://apple.github.io/foundationdb/testing.html) and
  formal verification.

The goal of the language is to provide a testbed for experimenting with how to
program distributed systems.

## Getting started

(This isn't implemented yet, it's merely an exercise in wishful thinking. Have a
look at the testsuite to see what's actually working.)

```haskell
$ cat example.smarr

data Input  = Incr | Read
data Output = Ack | Val Int
data State  = Int

main = proc i -> case i of
         Incr -> do
           n <- get -< ()
           put -< n + 1
           return -< Ack
         Read -> do
           n <- get -< ()
           return -< Val n
```
```bash
$ smarrow deploy repl example.smarr
> Read
0
> Incr
()
> Read
1
```

In a different terminal:

```diff
$ smarrow deployments
Current deployments:
  1. repl example.smarr 9faa7caca5a5e053bc84d5ca2731ce32ea1b265f
$ diff -u example.smarr example2.smarr
-   put -< n + 1
+   put -< n + 2
$ smarrow upgrade repl example.smarr example2.smarr
Update successful!
```

Back in the REPL terminal:

```bash
> Incr
()
> Read
3
```

Deploying and upgrading over SSH should work similarly to REPL/localhost.

## Features

### Arrow notation

### Hot-code swapping

### Adapters

* REPL
* Console
* HTTP

### Deployments

`smarrow deploy <source-file> <deployment-name> [<host>][:<port>]`
`smarrow call <deployment-name> <input>`

### Upgrades

* Patches vs upgrades, semantically check inputs and outputs for changes?

Can we do what Joe suggests:

  https://erlang.org/pipermail/erlang-questions/2011-May/058768.html

But on a state machine level instead of function level?

### Pipelining

XXX: Horizontally compose state machines.

### Supervision trees

### Time-travelling debugger

## Contributing

If any of this sounds interesting, feel free to get in touch.


## See also

* https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/arrows.html
* https://www.haskell.org/arrows/sugar.html
* https://hackage.haskell.org/package/arrowp-qq-0.3.0/src/
* https://karczmarczuk.users.greyc.fr/TEACH/Stage/ArrComp.pdf
