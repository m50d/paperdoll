# Paperdoll

A scala implementation of the paper:
[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)

Essentially a Free Monad that can contain multiple independent effects
(and without needing the Coyoneda trick for non-Functor effects).
New effect "layers" can be added onto the stack, or "unpeeled"
out of the stack, without the implementation of one layer needing to
be aware of any other layers.

## Implementation notes

In several places where there is a multi-parameter type `F[X, Y]`
I have added a corresponding:

    sealed trait F_[X] {
      final type O[X, Y] = F[X, Y]
    }

so that `F_[X]#O` can be used to express the partially applied type
instead of a type lambda. Any type ending in `_` is likely to be
an instance of this pattern.

Algebraic data types generally offer a fold method which is designed
to be the safe equivalent of a pattern match. When reviewing Scala
[it is difficult to distinguish between safe and unsafe pattern matches]((http://typelevel.org/blog/2014/11/10/why_is_adt_pattern_matching_allowed.html),
so my preferred style is to avoid pattern matches entirely.
This also makes it possible to hide trait implementation subtypes.

## Future tasks

 * Implement more effect types
 * Make sure it's possible to realign an effect stack (use shapeless.Align)
 * Rename the eff methods (e.g. send and handleRelay) to something easier to understand
  * The original idea (and reason for the project name) was for an extended metaphor
  of adding and removing layers of clothing on a FrenchKISS-style paper doll
 * Get into Maven Central
  * GPG signing in build (I know how to do this, it's just a matter of doing it)
 * Release 1.0
 * Binary compatibility checking in the build (using MiMA or similar)
 * Port to Cats if it:
  * Adds Leibniz or equivalent
  * Becomes 1.0
  
## Notices

Copyright 2015-2016 Michael Donaghy. md401@srcf.ucam.org

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
either express or implied. See the License for the specific
language governing permissions and limitations under the License.
