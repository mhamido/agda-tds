# Notes

## Parsing

Unfortunately, Agda's grammar is context-sensitive, and spans multiple phases where terms are partially parsed.

The first of which, is where the parser produces a concrete syntax tree, which leaves infix expressions in a flat list, to be resolved later once the operators are resolved and fixity information is available.

e.g,
> (a b c : _) -> a + b * c
is actually parsed as
> (a b c :_) -> [a, +, b, * , c]

Modules of interest:

```md
Agda.Syntax.Parser
Agda.Syntax.Concrete
```

## Scope Checking

To parse the concrete syntax tree into an abstract syntax tree, we need to resolve the fixities of operator names. This is done through the [Agda.Syntax.Concrete.Operators](https://agda.github.io/agda/Agda-Syntax-Concrete-Operators.html) module, but the interface to go through is the method [Agda.Syntax.Translation.ConcreteToAbstract (toAbstract)](https://hackage.haskell.org/package/Agda-2.2.8/docs/Agda-Syntax-Translation-ConcreteToAbstract.html)

Notice that the return type is `ScopeM` which is just an alias for `TCM` (Type Checking Monad). However, the [Agda.Syntax.Scope.Monad](https://hackage.haskell.org/package/Agda-2.2.8/docs/Agda-Syntax-Scope-Monad.html#t:ScopeM) defines a couple utility functions to help us out.

So what do we need to do?

1. Build up an index of modules for an entire project/directory. Use something from [this](https://hackage.haskell.org/package/Agda-2.7.0.1/docs/Agda-Interaction-Library.html).
2. Use [runTCM](https://hackage.haskell.org/package/Agda-2.2.8/docs/Agda-TypeChecking-Monad-Base.html#t:TCM) somewhere in main to start the session.
3. Either [create](https://hackage.haskell.org/package/Agda-2.2.8/docs/src/Agda-Syntax-Scope-Monad.html#createModule) a "fake" module for the interactive session, or assume that the package contains a Prelude module that imports everything.

4. Bring all definitions from the index / Prelude into scope. Maybe through [bindQModule] or [Agda.Interaction.Imports](https://hackage.haskell.org/package/Agda-2.7.0.1/docs/Agda-Interaction-Imports.html)?

5. Use [parseExpr](https://hackage.haskell.org/package/Agda-2.7.0.1/docs/src/Agda.Interaction.BasicOps.html#parseExpr) to get an abstract syntax tree.

6. Perform the search. Something in [here](https://hackage.haskell.org/package/Agda-2.7.0.1/docs/src/Agda.Interaction.SearchAbout.html#findMentions) might be of use, and the idris .

7. Return the results.

## References

- [Notes on the Compiler Pipeline](https://github.com/agda/agda/blob/master/notes/compiler-pipeline.md)

- [Idris Docs: Type-Directed Search](https://docs.idris-lang.org/en/latest/reference/type-directed-search.html)
