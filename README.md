# A (Simple) General Framework for Kleene-algebra-based Matching and Substitution

This Haskell library allows to specify a substitution mechanism for any
application that works on lists.

The library is mainly split in two parts: Kleene-style pattern matching (regular
languages) and a template + fill-in mechanism.

## Substitutions

Substitutions are specified as _rules_ (`Substitution.Rule`), that link
a _pattern_ (`Substitution.Pattern`) to a _template_ (`Substitution.Template`).

A set of rules may be applied to a given stream of tokens, either once (`Substitution.substituteOnce`)
or _until no further transformation is possible_ (`Substitution.substituteAll`).


## Patterns

Patterns are defines in a (somewhat extended) Kleene-algebra-style. Formally,
there is
  1. Sequence of patterns `(xxx yyy)`
  2. Alternative of patterns `(xxx | yyy)`
  3. Option `(xxx ?)`
  4. Repetition/Kleene star `(xxx *)`
  5. Empty pattern

There is also a special _capture_ pattern, to specify that the given portion
of a pattern, when matched, should be extracted and stored, typically to be
used in a template.

Patterns are represented by the `Substitution.Pattern.Pattern` type, which is
_type generic for labels_. Concretely, this means that a pattern may be defined
on any type of label (characters, strings, complex types).


### Labels

The labels that may be used in patterns are constrained by the `Matcher` typeclass,
found in `Substitution.Label`.

A `Matcher` instance allows to match any token with a label. It is used for matching
general patterns.

Types are separate for pattern labels and token to match so that one can define
advanced behaviours (e.g. matching classes, _à la_ POSIX regex with matchers such
as \s, \w, etc.).

For instance, to define a POSIX-style regex matcher, one could write:
```haskell
data PRx =
      Single Char
    | Range Char Char
    | Space

instance Matcher PRx Char where
    (Single c)    //> c' = c == c'
    (Range c1 c2) //> c' = (c >= c1) && (c <= c2)
    (Space)       //> c' = isSpace c'
```

`Matcher` instances may also define a `isConsuming` function that determines if
a token is consumed when matched against a given label.


### Automata

Once defined, a pattern is compiled into a finite state machine or _automaton_.

Automata are defined in `Substitution.Automaton`.

The main purpose of automata is to be able to perform the actual pattern matching:
a stream of token matches a pattern if it is a word recognized by the automaton (i.e.
if it is part of that automaton's language).

Derivation is obtained thanks to the `Substitution.Pattern.derive` function. Then,
`Substitution.Automaton.Reader` is used to perform automaton enactment.


## Templates

Templates are basically lists of tokens, interspersed with "variables", i.e. references
to eventual pattern captures.

Like for patterns, templates are _type generic_ as per the type of their content, so that
they can be used in any setting.


## Parsers

The library also provides facilities to write custom _parsers_ for the various parts
of a substitution mechanism. Such facilities are found in `Substitution.*.Parser`.

In general, the idea is to provide a function to turn a string into a specific label
or token, as well as a configuration for the parser (i.e. delimiters used in a 
pattern specification, etc.).








