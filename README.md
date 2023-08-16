# L-system Fractals

In this project I use an L-System to implement Turtle graphics (i.e. a drawing produced my moving a point around and adding lines),
specifically to produce simple fractal drawings.

The main point is to illustrate how complex abstractions used in the correct way can result in nice simplifications.

## L-Systems

A [Lindenmeyer System](https://en.wikipedia.org/wiki/L-system) (L-System) is a type of formal grammar
in which productions (i.e. rewrite rules) are applied simultaneously to a fixed initial string (the axiom).

A simple example illustrates the Cantor set:
```
Alphabet: { A, B }
Axiom: A
Rules: A -> ABA
       B -> BBB
```

Expansion is as follows:
```
1. A
2. A           B           A
3. ABA         BBB         ABA
4. ABA BBB ABA BBB BBB BBB ABA BBB ABA
```

This system is *context-free* as there is a single letter on the LHS of each rule, and *deterministic* as
each letter appears in at most one rule.

The code will assume systems are context-free and deterministic.

Many Wikipedia descriptions of fractals include a specification as an L-system, others can be determined by trial and error.

## Usage

First install dependencies and build via `stack build`.

The test.svg image is produced by the command
```
stack run -- -i 3 --name=Sierpinsky -f test.svg
```

All implemented systems are in `src/Systems.hs` and their corresponding command flags names are listed by passing an empty string to `--name`:
```
stack run -- --name=""
```

Note that the output size will usually grow exponentially with the number of iterations, e.g. Koch snowflake with n=10 will produce an SVG of about 100MB.
I have not really tried to optimize performance.
