# TesisLisp

Historical theorem prover and logic programming system originally developed in LISP, currently being reconstructed and ported to modern Common Lisp environments.

Repository: https://github.com/dsilberschmidt/TesisLisp

## Status

The original 1989 sources run **unmodified** on SBCL. `TESIS/` is byte-identical
to the first commit of the code (2020-05-10, `fca8f52`); everything needed to
bridge the historical dialect (likely TLC-LISP) to SBCL lives in `loader.lisp`.

`./run-tests.sh` checks 56 queries: every atom of the propositional problems
(dama1-7, presidente1-2, transil1-2), verified against an independent
model-checking oracle (`oraculo.py`), plus first-order queries with answer
extraction (append, pert, reverse).

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

## Dialect bridge (`loader.lisp`)

- Reader: `\x` is a character, `[a b c]` a vector, `|` an ordinary symbol, DOS `^Z` is whitespace; stray top-level `)` are ignored, as the original reader did (there are two in the sources).
- Lisp-1 semantics: functions are values (`(mapcar fusionar l)`, `(setq horizontal <)`), variables and vectors can be called (`(funcion x)`, `([a b] 1)`). A code walker rewrites each `de` at load time.
- 1-based `nth`/`substring`/`string-search`/`memvec`; generic `=` (a character equals its code); `do` with cond-style exit clauses; MacLisp `if`; `df` fexprs (`on`/`off`); `print` breaks the line after printing.
- Lists in user input/output use the dialect's vector notation: answers print as `?Z=[A B]`.

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

## Usage

Interactive, as in 1989:

```text
$ ./lgc
LGC 1989 - (ayuda) muestra el machete
> (load "PROBLEMA/FUENTES.LSP")
> (c app)
> (i)
?-(append ?x ?y [a b])
?X=NIL
?Y=[A B]
Mas (s/n)?
```

`(ayuda)` prints the full cheat sheet (commands, keys, flags, strategies).
`(off flag-monitor flag-interrupcion)` disables the step-by-step monitor
(on by default; press space to continue). `exit` leaves the interpreter,
`(quit)` the toplevel.

Regression: `./run-tests.sh` (about 20 s).

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

