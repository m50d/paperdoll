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

## Future tasks

 * Rename the queue operations to something sensible now that I understand them,
 rather than the symbol bingo they currently are
 * Rename the queue types to something easier to understand
 * Rename the layer types to something easier to understand
  * The original idea (and reason for the project name) was that 
 * Implement more effect types
 * Binary compatibility checking in the build (using MiMA or similar)
 * Get into Maven Central
  * GPG signing in build (I know how to do this)
 * Release 1.0
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
