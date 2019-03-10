Philosophy
==========

The design of Acetone and the Acetone IR is
guided by some general principles.
These principles are
layed out in this document.
For programs that do not
adhere to similar principles,
compilation with Acetone
may not work well.

Purity
------

Acetone will only optimize programs
that are devoid of side-effects.
Such programs are easier to optimize,
and it seems more realistic that
programs will generally be
effect-free rather than effectful
in the long term.

State threading
---------------

Acetone does not expose a
foreign function interface
for calling foreign code.
Instead, foreign code may call
Acetone-compiled code, passing in
subroutines as arguments.
This means that facilities such as
file systems, networking, clocks, etc
are not available directly, and must be
passed through the entire program as arguments
rather than being univerally available.

This simplifies the compilation and linking
of programs, and increases modularity of programs
by deferring choice of implementation to the caller.

Lazy evaluation
---------------

Acetone may turn strict code
into lazy code at will
to improve efficiency.
There is no guarantee over the
order or frequency of
evaluation of expressions.
