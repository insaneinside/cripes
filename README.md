# Common Components for Rule-Based Grammar Manipulation & Analysis

## Status

This is a work in progress.  It is not yet complete.


## Scope

This project is intended to provide the basic components for handling rule- and
pattern-based grammar definitions.  Future developments may expand this scope
to a fully-modular parser/lexer "framework", but current focus is on completing
the infrastructure.

## License

All code is to be distributed under the terms of the Apache License
version 2.0.  See file LICENSE for details.


## Expectations and Goals

At present, the overarching goal is as stated above: provide the "bare
necessities" for handling rule-based grammar definitions --- in a flexible and
modern fashion.

We expect that

  * the API will be clean, easily-understood, and idiomatic/"natural" in the
    target language;
  * all code will meet current standards for the sofware industry in terms of
    quality, testing, and reusability; and
  * components from this project shall have as small a resource footprint as
    reasonably possible to achieve stated goals.

Feature goals are as follows.

  * Distinct types, and procedures for interacting with and analysing them,
    will be provided for tokens, patterns, rules, and the grammars that
    they constitute.

    - Tokens should be be implemented as an interface (accessed via
      polymorphism or similar language feature) to enable use of user-defined
      types with existing parsing algorithms.

      A trend in linguistics research and industry practice in the past decade
      (or two) is to allow some form of namespacing in a grammar for better
      organization and compartmentalization; this is a probably a good idea,
      and should be implemented somehow.

      At minimum, provided token types should include the standard terminals
      and nonterminals; an adapter type for patterns of tokens should
      be provided if possible.

    - Pattern types should be able to represent patterns of elements of
      arbitrary types so that both parser and lexer algorithms can make use
      of them.

      Types for the basic regular-expression operations (repetition,
      concatenation, alternation, element classes, and grouping) should be
      provided.  Consider implementing lookahead/lookbehind,
      subpattern references.

## Language Choice

Our expectations place tight restrictions on the choice of language for an
implementation.  The requirement of a small resource footprint rules out most
interpreted languages; maximizing reusability w.r.t. downstream users' language
choice further narrows the scope to something that can export functions to and
from C.

The author's experience on a previous Ruby project of similar scope led him to
the conclusion that
a [functional](https://en.wikipedia.org/wiki/Functional_programming) language
could be a better choice for the project than e.g. C++; a short search led to
the selection of Rust.
