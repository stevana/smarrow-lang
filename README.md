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

State machines are to `smarrow` what functions are to functional programing
languages. A state machine has the type:

```haskell
  input -> state -> (state, output)
```

It's a bit clunky to have to thread through the state explicitly though. We
could have used a state monad and thereby get do-notation, which would improve
the egonomics of writing state machines, but in `smarrow` we go down an other
path and use arrow notation instead for reasons we shall explain next.

### Hot-code swapping

Conal Elliott's work on [compiling to
categories](http://conal.net/papers/compiling-to-categories/) shows how `Arrow`s
that don't use `arr` can be compiled to first-order combinators (Cartesian
closed categories).

We use this fact to compile our state machines (which are `Arrow`s that don't
use `arr`) to a bytecode inspired by Cartesian closed categories. This bytecode
can be sent over the write to remote nodes and update state machines without
downtime, similar to hot-code swapping à la Erlang.

### Adapters

State machines are pure, if we want them to do something useful we must hook
them up to inputs from the "real world". Adapters allow us to turn, for example,
an HTTP server request into an input which we can then feed to a state machine
and reply to the request using the output that the state machine produced.

Similary console I/O or a REPL could be used as adapters.

### Simulation testing

### Time-travelling debugger

### Deployments

`smarrow deploy <source-file> <deployment-name> [<host>][:<port>]`

`smarrow call <deployment-name> <input>`

### Supervision trees

### Upgrades

* Patches vs upgrades, semantically check inputs and outputs for changes?

Can we do what Joe suggests:

  https://erlang.org/pipermail/erlang-questions/2011-May/058768.html

But on a state machine level instead of function level?

### Horizonal composition -- pipelining

XXX: Horizontally compose state machines.

### Vertical composition

## Contributing

If any of this sounds interesting, feel free to get in touch.


## See also

* https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/arrows.html
* https://www.haskell.org/arrows/sugar.html
* https://hackage.haskell.org/package/arrowp-qq-0.3.0/src/
* https://karczmarczuk.users.greyc.fr/TEACH/Stage/ArrComp.pdf
