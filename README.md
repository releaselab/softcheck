# SoftCheck

![main workflow](https://github.com/joaosreis/softcheck/actions/workflows/main.yml/badge.svg)

SoftCheck is a platform written in OCaml that allows the definition and
computation of data-flow analyses. Through the use of a customizable
intermediate representation (SCIL), it aims to make process of defining analysis
targetting different source languages simpler and more easily extensible and
reusable. The platform is built around the notion of the monotone framework,
which is a generalization of the Kildall's lattice approach to data-flow
analysis, and a fixpoint equation solver.

The platform should be provided with a source language frontend which consists
in a mapping from a program represented in the original language to one
represented in SCIL. The platform intermediate language syntax is parametrized.
SCIL provides a set of instructions that deal with the flow of the program,
and is instantiable with the remaining expressions of the source language.
By providing this frontend, the platform has the necessary means to generate
a control-flow graph for any program represented with SCIL.

The definition of an analysis then composed of the following tasks:

- Define the lattice of the properties to be analyzed;
- Define the function that generates the set of monotone equations that relate
to the entry and exit information of each node in the control-flow graph.

The aspects of these equations that relate to the base set of SCIL instructions
is shared between the different instances of SCIL-represented programs. As for
the parameterized part of the SCIL language that is specific to each source
language, we can also parameterize the parts of the analysis that are specific
to each original language. This way, we can have a base for defining
analyses that is common to every SCIL-represented language, and worry only about
the parts of the analysis that vary according to the source language.

Given those definitions, the platform can now compute a data-flow analysis for
any program in a language whose frontend and language-specific analysis details
were provided by solving the generated equation system using an efficient
fixpoint algorithm.

## Install

You need OCaml (>= 4.11) and opam installed.

```bash
opam install https://github.com/joaosreis/softcheck.git
```
