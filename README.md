# Paperdoll

A scala implementation of the paper:
[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)

Essentially a Free Monad that can contain multiple independent effects
(and without needing the Coyoneda trick for non-Functor effects).
New effect "layers" can be added onto the stack, or "unpeeled"
out of the stack, without the implementation of one layer needing to
be aware of any other layers.

[![Build Status](https://travis-ci.org/m50d/paperdoll.svg?branch=master)](https://travis-ci.org/m50d/paperdoll)

## Features

   * A [Free Monad](http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html) implementation
      * i.e. for any datatype you like, you can extend that datatype to a "command object",
      where commands are composed of instance of that datatype and custom functions
    * Note that with Paperdoll there is no need for the `Coyoneda` trick, simplifying your code
    * Free monads let you [separate the declaration of a computation from its implementation
   ](http://michaelxavier.net/posts/2014-04-27-Cool-Idea-Free-Monads-for-Testing-Redis-Calls.html).
   You can then use multiple interpreters to run the same monadic computation e.g. test vs live
   or realtime datastore vs batch datastore (when using "lambda architecture")
   * Freer monads let you interleave multiple monadic effects without the complexities of monad transformers
      * Both definition of effects and of interpreters can be completely separate (even in separate codebases)
      * Effects are applied in a common-sense order according to their order in the code - different effects
      can be interleaved
   * Implementation is compatible with ScalaZ Monads
      * i.e. you can use existing ScalaZ-compatible functions like `traverse` on paperdoll effect stacks
   * Adapters to allow you to use popular monads from existing libraries as effect layers

### Compared to other Scala implementations of the same paper

   * Full `Coproduct`-based representation of the effect stack (improving on the paper),
   allowing effects to be reordered and interpreted in in different orders by different interpreter stacks
   * Bidirectional integration with established monad libraries including ScalaZ and Cats
   * Idiomatic Scala style, including use of OO where appropriate (i.e. not a direct Haskell port)
   * Designed for user-friendliness, with readable method names and extensive scaffolding to encourage correct type inference.
   * Intended as first-class, production-quality code

## How to use

### Declaring the dependency

    <dependency>
      <groupId>com.github.m50d</groupId>
      <artifactId>paperdoll-all</artifactId>
      <version>0.5</version>
      <type>pom</type>
    </dependency>

### Basic effect representation

Values of type `Effects[R, L, ?]` form a monad (using ScalaZ's `Monad` implementation).
You can use `Effects.send` or `Effects.sendU` (which infers the type if there
is a ScalaZ `Functor` instance) to create `Effects` values, and then use monadic
`for`/`yield` (or other monad-based constructs) to work with them:

    import scalaz.std.option._
    import scalaz.syntax.monad._
    import paperdoll.core.effect.Effects._

    val eff1 = for {
      r <- sendU(Option(1))
      s <- sendU(Option.empty[String])
    } yield s + r

To use these values you apply the appropriate handler(s), and then
finally call `.run`:

    import paperdoll.std.OptionLayer._

    val result: Option[String] = handleOption(eff1).run


### Combining multiple effects

So far, this is just an overcomplicated way of doing a `for`/`yield` that
we could have done natively with `Option`.
The difference comes when we want to mix and match two or more effects,
which we do by using `.extend` to extend the effects into a common stack.
I recommend using a type alias to name the effect stack for convenience:

    import shapeless.{:+:, CNil}
    import paperdoll.scalaz.Writer_
    import paperdoll.scalaz.WriterLayer._
    import paperdoll.std.Option_
    import scalaz.std.anyVal._
    import scala.collection.BitSet

    type MyStack = Option_ :+: Writer_[Int] :+: CNil

    val eff2 = for {
      _ <- sendTell(2).extend[MyStack]()
      t <- eff1.extend[MyStack]()
      _ <- sendTell(1).extend[MyStack]()
    } yield t

    val resultA: (Option[String], Int) = handleWriterMonoid[Int].apply(handleOption(eff2)).run
    val resultB: Option[(String, BitSet)] = handleOption(handleWriterCollection[Int, BitSet].apply(eff2)).run

    val eff3 = eff2.extend[Writer_[Int] :+: Option_ :+: CNil]()
    val resultC: (Option[String], Int) = handleWriterMonoid[Int].apply(handleOption(eff2)).run

(`sendTell` here is just a convenience function - you can still call `send`
or `sendU` directly for `Writer` or any other effect)

This is more powerful and flexible than monad transformers. We didn't have to choose
which way our effects would nest until interpretation time: `eff2` and `eff3` are
completely equivalent. Furthermore the effects can be interleaved, and happen in the
order they're specified in code (thus `resultA._2 == 2`), rather than all the
`Writer` effects happening before all the `Option` effects or vice versa (which
would make the writer result either `0` or `3`).
This is what we would expect from reading the definition of `eff2` (monadic effects
are not generally commutative, we should not be surprised that composing them
in a different order would yield a different result).

Note also that we have full, `Free`-style separation of the declaration of an effect
and the interpreter for that effect: for `Writer` there are two interpreters, the
`handleWriterMonoid` interpreter that merges all the written values into a single value,
and the `handleWriterCollection` interpreter that returns a collection of the values
that were written.

### Supported effects

To represent a collection (`List`, `Vector` etc.) as an effect TODO: NDet for stdlib.

TODO: list of all the other supported ones

#### ScalaZ

TODO: mention not supporting StateT/IndexedState

#### Cats

TODO: Not supporting state / reader / etc.

### Custom effects

TODO: Basic and advanced use cases

## Non-features and missing functionality

### Explicitly intended for future versions

   * Porting to work with `-Ypartial-unification`, hopefully replacing use of `Unapply`.
   * Consistent tagging of layers. Sometimes we might want multiple "versions" of the same effect
   in a stack (e.g. two different `Writer_[String]` effects for two different logs).
   It would be necessary to be able to distinguish these effects at the type level i.e. using
   some kind of "tagged" types. I have added ad-hoc support for something similar in
   `paperdoll-arm`'s `Region` effect (which uses a shapeless `Nat` to "label" each region),
   but it would be good to have general-purpose support for this instead.  

### Not scheduled for any particular release but contributions welcome

   * `Effects#extend` is implemented naïvely and adds overhead to the entire stack it's applied to.
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
   construct should still behave linearly, so I believe this is not a problem in practice.
   * Compilation time is really awful, particularly in the case of errors
   * There are no performance tests.
      * Constant-factor performance is likely bad. Contributions are likely a waste of time without tests.
   * The `Arr` type is closely related to `Kleisli`/`Arrow`, but I don't fully understand
    the similarity and am not yet aware of concrete use cases for it.
   * There is no support for `MonadPartialOrder`. In my limited experimentation it didn't seem
   to work correctly and looked to be subsumed for most purposes by `MonadTell` et al.
   * I have not implemented the `MonadCatchIO` example from the paper as there is no single established
   IO monad implementation in Scala and I don't really understand the approach ScalaZ is taking.
 
### Design compromises and deliberate omissions

   * The `send` in the paper is equivalent to `send` followed by `extend` in Paperdoll.
   I found it made the code clearer to separate the two (and it should make effects
   more compositional/reusable), but there may be an efficiency penalty.
   * Use of type members vs. type parameters is arguably inconsistent in places, as is general style.
   In some cases this is deliberate pragmatism so as to ensure that the types can be used in practice;
   in others I couldn't get type inference to work correctly with a more natural representation.
   * I have largely ignored variance. Good type inference is a higher priority than correct variance,
   but contributions that add co- or contravariance without compromising type inference are welcome.
   * There is no automatic binary compatibility checking in the build. MiMA seems to only support SBT, not maven.
   I find the maintainability advantages of maven compelling and will not accept patches to convert to SBT,
   but any implementation of binary compatibility checking in the maven build would be very welcome.
   * Paperdoll depends on ScalaZ since it makes extensive use of `Leibniz`. I would prefer to depend on Cats
   (at least for `paperdoll-core`), but this functionality is a firm requirement. `Is` is an inadequate
   replacement as it does not support upper bounds.
   I also use a feature of `MonadPlus` that I don't believe  is presently implemented in Cats,
   and make some use of `Unapply` and `Forall`. This last could reasonably be replaced with `~>`.
   Finally, modern versions of cats require `-Ypartial-unification` which is incompatible with
   ScalaZ `Unapply`. This makes migration or coexistence very difficult.
 
### Permanently open for improvement

   * Contributions of layers for popular monads are very welcome;
   instances and support code for an external library "foo"
   should be placed in a new `paperdoll-foo` maven module.

## Implementation notes

In several places where there is a multi-parameter type `F[X, Y]`
I have added a corresponding:

    sealed trait F_[X] {
      final type O[X, Y] = F[X, Y]
    }

so that `F_[X]#O` can be used to express the partially applied type
instead of a type lambda. In the case of `Layer`s I have used
`F` rather than `O` for the type member to emphasise that `F` is a
Functor-like type. Any type ending in `_` is likely to be
an instance of this pattern.

Algebraic data types generally offer a `fold` method which is designed
to be the safe equivalent of a pattern match. When reviewing Scala
[it is difficult to distinguish between safe and unsafe pattern matches](
http://typelevel.org/blog/2014/11/10/why_is_adt_pattern_matching_allowed.html),
so my preferred style is to avoid pattern matches entirely.
This also makes it possible to hide trait implementation subtypes
(by using anonymous classes) where appropriate.

`Leibniz#subst` represents that types are equal so in a sense it doesn't matter
"which way round" it's used, but for consistency I would prefer to first substitute
implicit parameters, then regular parameters, then self.

As `paperdoll-core` is very abstract, a lot of tests for `paperdoll-core` code
require one or more effect implementations.
So I've moved those tests down into `paperdoll-all` rather than add test-only effects.

The project is split into a number of small modules,
primarily to prove that the interpreters truly are independent.
I am not entirely convinced the dividing lines are in the right places;
suggestions for improvements are welcome.

I am a great admirer of [the circe philosophy](https://github.com/travisbrown/circe/blob/v0.3.0/DESIGN.md),
but this project is in many respects an opposite: I think there is value
in demonstrating that freer monads can be implemented in a very strictly safe
subset of Scala, and that is the primary goal for this project.
A more pragmatic project would likely provide the same API,
but make use of unsafe casts internally for performance. 

## TODO for 1.0

   * Extend Translator support to cover many monad transformers
      * Allows some limited support for ReaderWriterState and friends
   * Finish paperdoll-doobie
   * Tighten up PGP signature checking of upstream (i.e. specify key fingerprints)
   * Finish TODOs in this document (in particular examples)
   * Final code/readme review for readability
      * With particular focus on examples
      * Ensure no abbreviations where a full name will work
 
## Conduct

This project does not have a code of conduct. Please raise
any conduct issues either via the issue tracker or directly
with me (Michael Donaghy) by email to md401@srcf.ucam.org.
If you want to talk about a conduct matter privately I will
respect that - I will likely be unwilling to act on an allegation
if you want to keep the allegation and/or your identity private
(I think it's important that project participants be protected
from anonymous accusations), but you can always talk to me
and I won't take any public actions without your consent.

The following is explicitly not a code and not binding,
but is a guide to my current thinking on issues that I have
seen arise elsewhere. My decision is final in all cases:

   * Be polite. Correctness is no excuse for rudeness.
      * At times it may be even better to not answer at all if you
      know the answer to a (technical or other) question but are not
      able or willing to express that answer politely.
      * This applies particularly when interacting with newcomers
      to the project, the language, or programming in general.
   * I expect discussion to occur on a wide variety of subjects,
   not necessarily "on-topic". However everyone has the right to disengage
   from a particular topic and others should respect that.
      * Note that this applies even to technical matters.
      It's correct to be wary of the [XY problem](http://xyproblem.info/),
      but if someone has a particular requirement and is not interested
      in discussing why they have that requirement, respect that.
   * If any participant requests some reasonable behaviour from you to
   accommodate them personally
   (including but not limited to: referring to them in a particular
   fashion or none, not discussing particular topics in project spaces
   (provided those topics are not directly relevant to the project))
   please respect that. If you find an accommodation that someone
   else has requested to be onerous then please raise this as a
   conduct issue.
     * Generally I will either require participants to make that
     accommodation or add the non-accommodation to the Content Notes
     section of this document.
      * Please request accommodations only in good faith and only for yourself.

I will generally look to resolve conduct issues amicably through
discussion wherever possible. In cases where this fails,
the only sanction I am able or willing to impose is temporary
or permanent banning from project spaces. Cases of serious harassment
remain a matter for law enforcement, not project governance.

I ask those on the banlist not to participate in community spaces
associated with this project, including but not limited
to the issue tracker and wiki.
The banlist does not affect your license to the software.
I may or may not take technical measures to enforce bans;
the presence or absence of a technical ban measure should not be taken
to imply the existence of a ban not on the list in this document,
or the nonexistence of a ban on this list in the document.
If a technical ban measure is preventing you from participating
but your name is not listed in this document, please file an
issue if possible, or email me at md401@srcf.ucam.org if you
cannot file an issue. 

### Banlist

   * Tony Morris (indefinite)

### Content Notes

Paperdoll source code may contain sexualised elements, and discussion
around it may be or become sexualised. Individuals who find sexualised
code or conversation inherently threatening are advised not to
contribute to this project or participate in its spaces.

### Rationale

Conduct has sadly become an issue in the Scala community of late.
Some projects have responded by adopting codes of conduct.
In my view such codes have been counterproductive on the whole:
they necessitate all the downsides of a formal process,
but are rarely clear and objective enough to ensure consistent,
predictable handling of incidents or allegations).
The end result is the worst of both worlds: project leaders
are no longer empowered to take case-specific action,
but nor can contributors be confident that conduct issues will
always be dealt with appropriately.

The TypeLevel Code of Conduct in particular has been ambiguously enforced
with very little accountability. Specifically, the inconsistent
statements from Lars Hupel and Miles Sabin at various times
regarding alleged violation (or not) of the Code of Conduct
by Tony Morris are irreconcilable with the basic principles
of transparency and accountability in project governance.
I would speculate that this arose out of an unwillingness to directly
confront a specific individual who was causing problems,
and/or an attempt to generalize a single problem into
a policy that would prevent all future problems of that nature.
I consider that this would be premature generalization
with all that that implies.
Therefore I explicitly intend to handle conduct issues in Paperdoll
at a personal/individual level, unless and until I feel I have
sufficient experience to set good general policies.

(In my experience large classes of people have incompatible needs;
even having a code at all implies particular power structures that
I perceive as class-loaded and exclusionary toward certain categories
of marginalized people. Every code is a reflection of a particular culture,
and all cultures are exclusionary to some potential participants.
So I believe a project should form policies appropriate to its own
culture and community, and will be ill-served by adopting
something intended as a general-purpose code of conduct)

Mr Morris has upset people (including newcomers and myself)
in Scala-related spaces on numerous occasions. I find it implausible
that his statements were not intended to upset their recipients,
and in any case he has been repeatedly warned about them by many people.
My decision to ban him is taken in full awareness of his contributions to the Scala
community (in particular as founder of ScalaZ, which Paperdoll depends on);
while such contributions are laudable they do not make the things he says
any less unacceptable.

Paperdoll is an act of artistic self-expression (though it is also intended
as a production-standard library). Sex is part of human life
and I do not think it appropriate or healthy to pretend otherwise;
therefore sexualised code has its place in the project.
(I mention this as a content note because it differs from the TypeLevel Code of Conduct,
which states "participants and community organizers should not use
sexualized images, activities, or other material"). 

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
