Targets
=======

In this document you will find
what targets are supported
and how to use them.

The tooling is very barebones,
and you will need an understanding
of text manipulation and shell scripting
in order to use them.
If you are not familiar with these techniques,
I highly recommend you learn them.
They will be useful every day.

ECMAScript
----------

To compile code to ECMAScript,
invoke Acetone like so::

    TODO: Acetone invocation example.

The compiled units may subsequently
be linked to form a full program.
Linking will ensure that units can
call each other, and that the runtime
support library is in place.
First, build the runtime support library::

    nix-build -A runtime.ecmascript

Then, link the compiled units together::

    result/bin/link unit1.js unit2.js unit3.js

The result is an ECMAScript file that
exposes the globally-defined values,
and you may call it from any
hand-written ECMAScript code.
