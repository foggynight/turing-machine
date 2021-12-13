turing-machine
==============

Turing machine simulator with support for multitape and nondeterministic Turing
machines.


Turing Machines
---------------

A Turing machine is defined by its...
- Input alphabet: `Σ`
- Tape alphabet: `Γ = Σ ∪ {⊔}`
- Set of states: `Q`
- Transition table: `δ = {Q x Γ -> Q x Γ x {L, R, S}}`
Where `⊔` represents the blank symbol, and `{L, R, S}` (left, right, stay)
represents the direction which the head should move along the tape.

Turing machines use infinite tape(s) as their input/output and storage medium.
These tapes are defined as infinite in either one or both directions depending
on the definition being referenced, but both models are equivalent. This
simulator uses tapes which are infinite in both directions.

Turing machine computations can be viewed as a sequence of configurations, or a
tree of configurations in the case of nondeterministic Turing machines, where a
configuration is a snapshot of the Turing machine's state, the positions of its
heads, and the contents of its tapes.

Turing machine programs are defined by a transition table. Input to the program
is written on a tape, the tape is fed into the Turing machine, and the
transition table determines the sequence of configurations which compose the
computation.


Installation
------------

The following instructions assume you will be building `turing-machine` using
make. To build without make, use the commands contained within the makefile.

To install the required CHICKEN eggs (libraries) and build `turing-machine`,
execute the following from the repo directory:

    make eggs
    make

This will install the eggs with elevated privileges, and create the
`turing-machine` executable in the repo directory.


Usage
-----

### Writing Programs

`turing-machine` programs are written in text files, where each line of the text
file is either empty, a comment, a CONF directive, or a transition rule. Empty
lines and comments are ignored, CONF directives are used to customize the
simulator, and transition rules define the program itself.

The syntax of a transition rule is as follows:

    current-state read-symbol* -> next-state write-symbol* move-direction*

There may be zero or more whitespace-separated read/write symbols and move
directions in a transition rule, but all of the transition rules in a program
must contain the same number of each.

By default, the initial state is 0, and the halt states are A (accept), R
(reject), and E (error); but these along with much of the syntax used to write
`turing-machine` programs can be customized using CONF directives.

For example programs and to see some of the other features of `turing-machine`
not mentioned here, such as the usage of CONF directives, see the example and
test directories.

### Running Programs

To run a `turing-machine` program, pass the name of the text file containing the
program as a command line argument to `turing-machine`, such as:

    ./turing-machine example/binary-palindrome.tm

This will start `turing-machine`, load the specified program, read a single line
of input which will be written onto the input tape, and perform the computation.
`turing-machine` will display the progress of the computation during its
execution, and the results of the computation once it has terminated.

Additionally, there is a REPL mode which fetches inputs in a loop. The desired
mode is to be specified using a command line argument passed before the name of
the file containing the program, such as:

    ./turing-machine repl example/binary-palindrome.tm

There are two modes: eval and repl, with eval mode being the default.


Dependencies
------------

- CHICKEN 5
  - srfi-1
  - vector-lib


License
-------

Copyright (C) 2021 Robert Coffey

This is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License version 3.
