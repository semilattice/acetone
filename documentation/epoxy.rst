Epoxy
=====

Epoxy is a dialect of System Fω
with type classes
and row polymorphism.
It is intended to be a programming language
that follows the same principles as Acetone.
The scope of Epoxy can be compared to
that of Haskell and PureScript:
functional programming
with an expressive type system.

Like Acetone, Epoxy features
purity, no globals, and no foreign function interface.

Epoxy comes with an extensive standard library
that favors compositionality and orthogonality over convenience,
as it is believed that this is important
in the development and maintenance of large applications.
One implication is that simple programs will
require a relatively large amount of code.
On the other hand, changing large programs
will be easier.

When designing the standard library,
it is extremely important to think a lot about everything,
and not make assumptions based on past experience.
This is especially the case in I/O, network, time, and text components.
For example, one may have experience
with programming languages that feature one true string type.
Encoding and decoding are applied as necessary.
However, in a large application with many integrations,
it may be desirable to use different encodings in different places.
The Epoxy standard library therefore offers many string types,
each with their own in-memory representation.
This is at the expense of some convenience,
because the programmer must now think about what they need.

It is a general trend indeed, in the design of Epoxy,
that the programmer is given freedom.
The programmer is trusted
on their competence and understanding
of all relevant concepts.
If the programmer does not understand a concept,
they are trusted to spend time learning the concept.
