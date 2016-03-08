# mirth
Funny little (mostly) stack based DSL experiments.
## Naive
Continuation style (with the free monad). Haskell uses the co-Yoneda lemma to automatically convert a type constructor into a functor that the free monad requires.

## VanLaarhoven
Instead of a type with separate constructors for instructions, things are parameterised over a record of the implementations of the instructions.

Based on ideas from [this article](http://r6.ca/blog/20140210T181244Z.html) from 2014.

