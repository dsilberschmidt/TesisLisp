# TesisLisp

**LGC — Lenguaje de programación lógica de primer orden con diversos mecanismos de inferencia.**
A complete resolution theorem prover and logic programming language written in Lisp in 1989
as a graduation thesis, running again — unmodified — on modern SBCL.

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

LGC was the graduation thesis of **Fabio Friedlaender** and **Daniel Silberschmidt**
at the *Instituto NCR de Ciencias de la Computación* (Argentina), an institution that
no longer exists. It was completed in 1989 and approved with a grade of 100
(certificate dated January 15, 1990).

- **Motivation** (from the original proposal, `TESISDOC/texto/PROPUES.txt`): Prolog
  cannot express simple non-Horn sentences ("if A then B or C", "not A") in a single
  clause, its inference is incomplete, and some problems are solvable only if the
  clauses are given in a particular order. LGC aimed at programming in first-order
  logic independently of the control component.
- **Design**: a refutation system by resolution over unrestricted (non-Horn) clauses,
  with a complete inference mechanism — breadth-oriented, set-of-support search with
  configurable heuristics instead of Prolog's depth-first backtracking — plus a
  compiler from first-order formulas to clausal form (Skolemization included),
  tautology elimination, factoring ("fusión"), subsumption, answer extraction,
  clause recycling and an interactive step-by-step monitor.
- **Environment**: the authors were taught Lisp and Prolog by their AI teacher, who also
  provided the Lisp system they used (remembered as TLC-LISP); they never saw its manual
  and worked from his notes. Neither author owned a computer: the code was
  written on scheduled machine time on an XT at the Facultad de Ingeniería. A
  session started with DOS, Sidekick, `lisp` and `>>>(load "lgc")`
  (`TESISDOC/texto/CARTILLA.txt`).

## Dialect bridge (`loader.lisp`)

- Reader: `\x` is a character, `[a b c]` a vector, `|` an ordinary symbol, DOS `^Z` is whitespace; stray top-level `)` are ignored, as the original reader did (there are two in the sources).
- Lisp-1 semantics: functions are values (`(mapcar fusionar l)`, `(setq horizontal <)`), variables and vectors can be called (`(funcion x)`, `([a b] 1)`). A code walker rewrites each `de` at load time.
- 1-based `nth`/`substring`/`string-search`/`memvec`; generic `=` (a character equals its code); `do` with cond-style exit clauses; MacLisp `if`; `df` fexprs (`on`/`off`); `print` breaks the line after printing.
- Lists in user input/output use the dialect's vector notation: answers print as `?Z=[A B]`.

## Repository Structure

```text
TESIS/        the 1989 engine, unmodified (UTI, UNIFICAR, RENOMBRA, UTICLA, RESOLVER,
              TAUTOLOG, FUSION, SUBSUME, MONITOR, RESPUES, RECICLAR, BUSQUEDA,
              ESTRATEG, CLAUSAL, INICIAL, EVALUAR, INTERPRE, LGC)
PROBLEMA/     original example bases: dama1..7, presidente1-2, transil1-2, app, pert, rev
TESISDOC/     original thesis documentation (Word 6 .DOC): proposal, introduction,
              structure, modules, search, answers, syntax and CARTILLA (user card)
TESISDOC/texto/  the same documents as UTF-8 text, readable on GitHub
BASURA/       1989 drafts and experiments (including a small Prolog)
loader.lisp   the TLC-LISP -> SBCL bridge, the `(tlc::lgc)` toplevel and `(ayuda)`
lgc           launcher
run-tests.sh  56 regression queries;  oraculo.py  independent oracle for them
```

## Provenance

`TESIS/` is byte-identical to the code as first committed on 2020-05-10:

```bash
git diff --stat fca8f52 -- TESIS/      # prints nothing
```

Earlier porting attempts edited these files; they were all reverted so that every
adaptation lives in `loader.lisp` and the original text stays untouched.

## Notes on the 1989 code

Found while restoring it; all left as they were:

- Two stray closing parentheses at top level (`ESTRATEG.LSP` after `ancestro`,
  `EVALUAR.LSP` after `pertenece-vacia-bis`). The original reader tolerated them; the
  loader ignores them and says so.
- `ancestro` has a typo — `(or clausula1 clausula2)` closes too early — so it always
  returns NIL. It only matters with `flag-filtrado-ancestros`, which is off by default.
- `pertenece-vacia-bis` (used by `evaluar`) only checks the first new resolvent for the
  empty clause, while the unused `pertenece-vacia` checks them all. It gives the right
  answer on all 48 propositional test cases.
- Search is complete, so a goal can have several refutations: answering `s` to
  `Mas (s/n)?` can print `Si.` again for the same ground goal.
- A low `limite-profundidad` can be slow: below the depth a proof needs, the whole
  bounded space is explored before giving up (reverse of 3 elements: limit 8 takes about
  a minute and fails, no limit finds `[C B A]` in 8 s).

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

## Quick start

```bash
sudo apt install sbcl          # macOS: brew install sbcl
git clone https://github.com/dsilberschmidt/TesisLisp.git
cd TesisLisp
./lgc
```

Then, at the `>>>` prompt: `(ayuda)` prints the cheat sheet (in Spanish).

## Usage

Interactive, as in 1989:

```text
$ ./lgc
LGC 1989 - (ayuda) muestra el machete
>>> (load "PROBLEMA/FUENTES.LSP")
>>> (c app)
>>> (i)
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

## Web (branch `web`, in progress)

```bash
python3 web/servidor.py        # http://localhost:8080
```

A one-page front end: pick one of the 1989 bases (or write your own), type a goal,
get the original system's output. Each query runs `(tlc::consulta-env)` in a fresh,
throwaway SBCL process with CPU, memory and 20 s limits, one at a time; no toplevel,
no nested Lisp, no read-time evaluation. Python standard library only.

Container (Ubuntu 24.04 + SBCL + Python, non-root, read-only code):

```bash
docker build -t lgc1989 .
docker run --rm -p 8080:8080 lgc1989          # http://localhost:8080
```

Give the instance 1 GB of memory: the worst case (a runaway search up to the 20 s cap)
peaks at about 430 MB of SBCL heap. `LGC_HEAP_MB=256` fits a 512 MB instance, but
runaway queries then fail without partial output.

## Philosophy of the Port

The objective is not merely to “make it work”, but:

- preserve historical behavior
- understand the original architecture
- document the inference model
- modernize carefully
- avoid accidental semantic drift

## License

Historical academic code.

License status still under review (to be agreed by the authors).

## Authors

Original system:

- Fabio Friedlaender
- Daniel Silberschmidt

Modern reconstruction / SBCL compatibility work:

- Daniel Silberschmidt

