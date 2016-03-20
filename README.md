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
  * Both definition of effects and of interpreters can be completely separate (even in separate codebases)
 * Improving on the paper, Paperdoll uses a `Coproduct`-based representation for the effect stack,
 allowing effects to be reordered and interpreted in in different orders by different interpreter stacks. 

## Non-features and rationales

 * Currently, supported monads are limited to (reimplementations of) those in the paper.
 While this is sufficient to prove the concept, it would be better to add `Layer` adapters
 for popular real-world Scala cases (e.g. those used by doobie).
 The `Eff` abstraction should be easy to use with any existing monad implementation
 without needing to change the concrete monad values (as long as they *are* values -
 so perhaps only `Free`-like monads, but there is still an installed base of those
 that it is well worth being able to interoperate with).
 Contributions are very welcome; instances for an external library "foo"
 should be placed in a new `paperdoll-foo` maven module.
 * The `send` in the paper is equivalent to `send` followed by `extend` in Paperdoll.
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
 * Use of type members vs. type parameters is arguably inconsistent in places, as is general style.
 In some cases this is deliberate pragmatism so as to ensure that the types can be used in practice;
 in others I couldn't get type inference to work correctly with a more natural representation.
 * Compilation time is really awful, particularly in the case of errors.
 * There are no performance tests. I don't have time to do these, but would welcome contributions.
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
[it is difficult to distinguish between safe and unsafe pattern matches](
http://typelevel.org/blog/2014/11/10/why_is_adt_pattern_matching_allowed.html),
so my preferred style is to avoid pattern matches entirely.
This also makes it possible to hide trait implementation subtypes
(by using anonymous classes) where appropriate.
I have used pattern matching on `Inr`/`Inl` when working with `Coproduct`s
since Shapeless does not offer a suitable `fold` method.

Since `paperdoll-core` is very generic, a lot of the tests need at least one effect implementation.
So I've moved those tests down into `paperdoll-tests`. TODO: Reevaluate project structure

The project is split into a large number of small modules,
primarily to prove that the interpreters truly are independent.

I am a great admirer of [the circe philosophy](https://github.com/travisbrown/circe/blob/v0.3.0/DESIGN.md),
but this project is in many respects an opposite: I think there is value
in demonstrating that freer monads can be implemented in a very strictly safe
subset of Scala, and that is the primary goal for this project.
A more pragmatic project would likely provide the same API,
but make use of unsafe casts internally for performance. 

## TODO for 1.0

 * Consider moving NDet (and potentially other cases) into -core for pragmatism regarding implicit resolution
 * Implement remaining things from the paper
  * Might be worth splitting out -examples projects
  * Finish the "How to use" section of this document
 * Get into Maven Central
  * Requires adding gpg signing to the build
 * Decide on whether to do a gitter channel / conduct section / etc.

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
