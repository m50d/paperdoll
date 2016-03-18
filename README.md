# Paperdoll

A scala implementation of the paper:
[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)

Essentially a Free Monad that can contain multiple independent effects
(and without needing the Coyoneda trick for non-Functor effects).
New effect "layers" can be added onto the stack, or "unpeeled"
out of the stack, without the implementation of one layer needing to
be aware of any other layers.

[![Build Status](https://travis-ci.org/m50d/paperdoll.svg?branch=master)](https://travis-ci.org/m50d/paperdoll)

## How to use

TODO

## Features

 * Implementations of some popular concrete, useful monads (e.g. `Reader`, `Writer`)
  * i.e. some classes that do useful things when used in `for`/`yield` blocks
 * ScalaZ-compatible typeclass instances allow monad-generic functions
  * i.e. you can use these classes with ScalaZ functions like `traverse`, or with custom functions that work for any monad 
 * Offers a [Free Monad](http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html) equivalent
  * i.e. for any datatype you like, you can extend that datatype to a "command object",
where commands are composed of instance of that datatype and custom functions
  * Note there is no need for the `Coyoneda` trick in this implementation
 * Free monads let you [separate the declaration of a computation from its implementation](http://michaelxavier.net/posts/2014-04-27-Cool-Idea-Free-Monads-for-Testing-Redis-Calls.html)
  * Can use multiple interpreters to run the same monadic computation e.g. test vs live
 * Freer monads let you interleave multiple monadic effects without the complexities of monad transformers
  * Both definition of effects and of interpreters can be completely separate (even in separate codebases).

## Non-features and rationales

 * `Eff#extend` is implemented naïvely and adds overhead to the entire stack it's applied to.
 Therefore the performance of a construct like `f.flatMap(g).extend[...].flatMap(h).extend[...]`
 is likely quadratic rather than linear as it should be.
 Indeed it may be worse than that, since `.extend` fixes a blob in the tree-like queue structure,
 so composing with further operations won't rebalance the tree
 and we lose the efficient "reflection without remorse" structure.
 So the behaviour may actually be cubic.
 On the other hand the implementation is the same as that in `handleRelay`,
 so this aspect of the behaviour is no worse than what the original Haskell implementation
 would do for a chain of `f flatMap g |> handleA flatMap h |> handleB ...`
 Note that a `for { x <- f.extend[...] ; y <- g.extend[...] ; z <- h.extend[...] } yield ...`
 construct should still behave linearly, so I believe this is not a problem in practice; patches are very welcome.
 * There are no performance tests. I don't have time to do these, but would welcome contributions.
 * Since `paperdoll-core` is very generic, a lot of the tests need at least one effect implementation.
 So I've moved those tests down into `paperdoll-tests` 
 * There is no automatic binary compatibility checking in the build. MiMA seems to only support SBT, not maven.
 I find the maintainability advantages of maven compelling and will not accept patches to convert to SBT,
 but any implementation of binary compatibility checking in the maven build would be very welcome.
 * Paperdoll depends on ScalaZ since it makes extensive use of `Leibniz`. I would prefer to depend on Cats
 but this functionality is a firm requirement. 

## Implementation notes

In several places where there is a multi-parameter type `F[X, Y]`
I have added a corresponding:

    sealed trait F_[X] {
      final type O[X, Y] = F[X, Y]
    }

so that `F_[X]#O` can be used to express the partially applied type
instead of a type lambda. Any type ending in `_` is likely to be
an instance of this pattern.

Algebraic data types generally offer a `fold` method which is designed
to be the safe equivalent of a pattern match. When reviewing Scala
[it is difficult to distinguish between safe and unsafe pattern matches](http://typelevel.org/blog/2014/11/10/why_is_adt_pattern_matching_allowed.html),
so my preferred style is to avoid pattern matches entirely.
This also makes it possible to hide trait implementation subtypes.
(I have used pattern matching on `Inr`/`Inl` when working with `Coproduct`s
since Shapeless does not offer a suitable `fold` method)

## TODO

 * Implement more effect types
 * Create a test that demonstrates combining two unrelated effect monads and running in either order
 * Get into Maven Central
 * Finish this document
 * Release 1.0
  
## Notices

Copyright 2015-2016 Michael Donaghy. md401@srcf.ucam.org

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this project except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
either express or implied. See the License for the specific
language governing permissions and limitations under the License.
