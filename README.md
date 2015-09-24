A solver for typedef inference in C like languages
==================================

This is a prototype typedef inference tool for C. It reconstruct
typedef's from a Core version of C.

Install
-----

1 - First, download Haskell [stack](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md).

2 - Run

    stack setup

to install the correct version for GHC Haskell compiler (that will be
the latest one).

3 - Run

    stack build

to build the tool. Since it will install all necessary libraries, this
may take some time.


Executing
--------

The best way to execute (without having to install it) is using
stack. Just run:

    stack exec typedefsolver-exe -- -i FILENAME

it will parse and infer typedefs for a simplified C syntax file and
output the results on terminal and on _FILENAME.tdef_

Test suite
--------

There's a small test suite based on examples sent by Fernando. To
execute the test suite, just run

    stack test

Note that not all examples are handled. Examples using function
pointers and array syntax initialization are not supported (yet!).

Bugs and other things
-----------------

Any problems you may find, fell free to register a new issue on
project's issue tracker.


Documentation
------------

I'll start a documentation to describe input files syntax (both Core C
and constraints) and how the solver and constraint generation works.
