# Type Directed Search for Agda

## Setup

```console
> git clone --recurse-submodules <https://github.com/mhamido/agda-tds.git>

> cabal build doogle

> cabal exec doogle -- -i <path to a prelude file>

# e.g,

> cabal exec doogle -- -i test-projects/agda-base/Prelude.agda
```

In the event that you get an error akin to `Agda can't find the primitives module for the version <VERSION>`, you might have to copy the directory from another version of Agda.
