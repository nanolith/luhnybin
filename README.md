My solution to the LuhnyBin problem
===================================

Since I was a little late to the game, I thought I would devise a bit of a
unique solution to this problem.  I wrote a simple state machine to handle
parsing, and a source-source compiler via lex+yacc to transform this state
machine description to C++.  More information can be found
[here][cxxfilter_doc].

BUILDING cxxfilter
------------------

To build this solution to the LuhnyBin problem, please run GNU Make from the
src subdirectory.

For more information, please see the src/cxxfilter/README.md.

[cxxfilter_doc]: luhnybin/tree/master/src/cxxfilter "cxxfilter source documentation"
