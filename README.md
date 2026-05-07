# TesisLisp

Historical theorem prover and logic programming system originally developed in LISP, currently being reconstructed and ported to modern Common Lisp environments.

Repository: https://github.com/dsilberschmidt/TesisLisp

## Status

Current work focuses on:

- recovering historical behavior
- restoring compatibility with modern Common Lisp implementations
- documenting the architecture
- building regression tests
- understanding the original inference strategy before modifying semantics

Main active branch for historical debugging:

```text
rescue-historical-dama1
```

## Historical Context

TesisLisp was originally developed as an academic thesis project.

The system implements:

- clause transformation
- unification
- resolution
- subsumption
- tautology elimination
- heuristic search strategies
- interactive theorem proving
- logic query evaluation

The codebase preserves many characteristics of the original dialect and style:

- MacLisp-inspired conventions
- dynamic/global variable assumptions
- non-ANSI compatibility behaviors
- symbolic interactive workflow

The current reconstruction attempts to preserve those semantics as faithfully as possible.

## Current Implementations Tested

### SBCL

Primary current target.

Working:

- loader
- parser pipeline
- clausal transformation
- basic resolution
- simple evaluation examples
- Dama1 compilation

Still under investigation:

- historical search behavior
- infinite frontier expansion in some examples
- dynamic scoping assumptions from the original environment
- compatibility semantics between historical Lisp and ANSI Common Lisp

### CLISP

Exploratory branch planned.

Goal:

- compare runtime behavior with a more permissive historical Common Lisp implementation
- use CLISP as a behavioral oracle
- identify SBCL-specific incompatibilities

## Repository Structure

```text
TESIS/
    core inference engine
    search
    resolution
    heuristics
    clausal transformation
    evaluation
    interpreter

PROBLEMA/
    example knowledge bases and problems
    dama1..7
    presidente
    transilvania
    append
    reverse
```

## Main Components

### CLAUSAL.LSP

Transforms formulas into clausal form.

Includes:

- implication elimination
- negation pushing
- quantifier elimination
- Skolemization
- distribution
- CNF generation

### RESOLVER.LSP

Core resolution engine.

### BUSQUEDA.LSP

Search control and expansion strategy.

### ESTRATEG.LSP

Heuristics and expansion ordering.

### EVALUAR.LSP

Goal evaluation using resolution.

### INTERPRE.LSP

Interactive interpreter.

## Example

Compile and test one of the historical examples:

```bash
cd ~/Proyectos/TesisLisp && sbcl --load loader.lisp \
  --eval "(in-package :tesis)" \
  --load PROBLEMA/FUENTES.LSP \
  --eval "(compilar dama1 'objeto-dama1)" \
  --eval "(setq base-interna (armar-clausulas objeto-dama1))" \
  --eval "(setq goal-interno (armar-goal '(hay habitacion1 dama)))" \
  --eval "(format t \"~%eval => ~S~%\" (evaluar base-interna goal-interno))" \
  --quit
```

## Current Investigation

One of the main current goals is understanding why some historically solvable examples generate unbounded frontier growth under SBCL.

Current hypotheses include:

- differences in dynamic vs lexical scoping
- historical macro semantics
- equality semantics
- ordering/subsumption interactions
- compatibility behavior of utility functions
- implementation differences in old Lisp systems

The project currently prioritizes:

```text
understanding historical semantics first,
modifying algorithms second
```

## Philosophy of the Port

The objective is not merely to “make it work”, but:

- preserve historical behavior
- understand the original architecture
- document the inference model
- modernize carefully
- avoid accidental semantic drift

## License

Historical academic code.

License status still under review.

## Authors

Original system:

- Fabio Friedlaender
- Daniel Silberschmidt

Modern reconstruction / SBCL compatibility work:

- Daniel Silberschmidt

