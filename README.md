# smarrow-lang

`smarrow-lang` is an experimental programming language where programs are state
machines expressed in [arrow
notation](https://www.haskell.org/arrows/syntax.html).

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

## Why a new language?

Barbara Liskov gave two good reasons in her Turing award
[lecture](https://amturing.acm.org/vp/liskov_1108679.cfm) (2008):

> "Programmers write programs in programming languages, so programming language
> constructs are a very good way of communicating to programmers if you are
> trying to get them to understand how to build programs."

> "There's a funny disconnect in how we write distributed programs. You write
> your individual modules, but then when you want to connect them together
> you're out of the programming language and into this other world. Maybe we
> need languages that are a little bit more complete now, so that we can write
> the whole thing in the language."

## Getting started

```bash
$ cabal run smarrow-deploy &
$ cat example/Counter.smarr
```
```
machine Counter where

state : { count : Int = 0 }

language
  : Incr -> {}
  | Read -> Int

function i -> case i of
  { Incr -> do { s <- get -< {}; put -< { count = s.count + 1 } }
  ; Read -> get -< {}
  }
```
```bash
$ cabal run smarrow -- deploy counter example/Counter.smarr
Deployed: counter
$ cabal run smarrow -- invoke counter Incr
{}
$ cabal run smarrow -- invoke counter Read
1
```
```diff
$ diff -u example/Counter.smarr example/Counter2.smarr
-   put -< { count = s.count + 1 }
+   put -< { count = s.count + 2 }
$ cabal run smarrow -- deploy counter example/Counter2.smarr
Upgraded: counter
```
```bash
$ cabal run smarrow -- invoke counter Read
1
$ cabal run smarrow -- invoke counter Incr
{}
$ cabal run smarrow -- invoke counter Read
3
$ fg
cabal run smarrow-deploy
^C
```

## Features

### First-class state machines

In functional programming languages functions are first-class. In `smarrow` we
experiment with making state machines first-class instead.

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

Another useful adaptor is to deploy a bunch of communicating state machines on a
fake network where all messages are kept in an in-memory priorty queue sorted by
arrival time. We can then simulate the network, by popping the queue, advancing
the clock to the arrival time, feed it to the receiving state machine, collect
the outputs, generate arrival times for the outputs and feed them back into the
queue. The arrival times are generated using a pseudo number generater fed with
a seed, this allows us to determinstically get different interleaving of
messages. The fact that we advance the clock to the arrival time makes timeouts
happen without having to wait for them, which is useful when combined with fault
injection of network faults. Essentially this gives us a discrete-event
simulation, which is much faster than testing using Jepsen and is determinstic
too!

### Time-travelling debugger

If we record all inputs that we feed the state machines, then we can implement a
time-travelling debugger by merely feeding the inputs one-by-one to the state
machines and observing how the states change over time.

We can use snapshots and fixed sized ring-buffers of inputs in case there are
too many inputs to store then all.

### Deployments via supervision trees

XXX:

### No modules or packages

A state machine is a unit of deployment, unlike a (pure) function. A state
machine must be under version control before it can be deployed, it's therefore
globally uniquely identifiable and therefore there's no need to further package
it up.

State machines carry automatically checked semantic versioning and other
metadata making it possible to search for state machines, subscribe to
(security) updates for ones that you've deployed, etc.

See also:

* https://erlang.org/pipermail/erlang-questions/2011-May/058768.html
* https://lobste.rs/s/xi3mi0/does_programming_language_with_ml_style
* Unison

### Upgrades

After a system is deployed it will likely need to be upgraded.

XXX: we already talked about hot-code swapping...

* Patches vs upgrades, semantically check inputs and outputs for changes?

* VM upgrades? How does Erlang deal with them?

### Protocols

A state machines input type essentially defines its API, it doesn't say anything
about which sequences of inputs are allowed. Consider the POSIX filesystem API
where we can open a file to get a file handle, which we can then read and write
to, at the end we are supposed to close the file handle and once closed no
further reads or writes are allowed. These legal sequences of inputs define the
protocol. One way to encode protocols is by means of a state machines.

Protocols can be enforced at run-time by protocol checkers that sit in-between
state machines.

### Horizonal composition -- pipelining

XXX: Horizontally compose state machines.

* [Jim Gray on parallelism via pipelining](https://youtu.be/U3eo49nVxcA?t=1949)

### Vertical composition

### Capabilities

* Lecture on [capabilities](https://youtube.com/watch?v=TQhmua7Z2cY) from MIT's
  6.858 Computer Systems Security course (2014)
* [The Heart of Spritely: Distributed Objects and Capability
  Security](https://spritely.institute/static/papers/spritely-core.html)

### Formal verification

* Model-checking à la TLA+: https://github.com/Gabriella439/HasCal
* https://lawrencecpaulson.github.io/2022/10/12/verifying-distributed-systems-isabelle.html

## Contributing

If any of this sounds interesting, feel free to get in touch.

### To do

* records
* state migration
* parametrised inputs/sums of products
* api evolution
* deprecation of machines?
* persistence
* typechecker
* state invariants?
* pre-conditions on input?
* pretty printer
  - and formatter?
* cli
  - rustup?
* encryption
* tunnel, https://tunnelto.dev or https://github.com/pcarrier/srv.us ?
* migrate deployment
  - automatic failover?
* observability
* debugger
* optimise encoding of messages based on frequency in previous deployment?
* echo server benchmark

## See also

* The P programming language
* https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/arrows.html
* https://www.haskell.org/arrows/sugar.html
* https://hackage.haskell.org/package/arrowp-qq-0.3.0/src/
* https://karczmarczuk.users.greyc.fr/TEACH/Stage/ArrComp.pdf
