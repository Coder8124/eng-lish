# The eng-lish Language Specification

This is the specification for eng-lish: what the language is, keyword by keyword. It describes the language precisely enough that someone could write a second eng-lish compiler from it.

**If you are trying to learn eng-lish, this is the wrong document.** Start with the [language reference](../language-reference/index.md), which teaches each feature with examples. This one defines them.

## Contents

| Chapter | Covers |
|---|---|
| [1. Lexical structure](01-lexical-structure.md) | Characters, comments, keywords, literals, identifiers |
| [2. Types](02-types.md) | The type system and conversions |
| [3. Expressions](03-expressions.md) | Operators, precedence, calls, indexing |
| [4. Statements](04-statements.md) | Declarations, assignment, control flow, output |
| [5. Declarations](05-declarations.md) | Functions, classes, packages |
| [6. Beginner mode](06-beginner-mode.md) | What `use beginner.` changes |
| [grammar.ebnf](grammar.ebnf) | The whole grammar in one file |

## What this specification describes

It describes the **reference implementation** — the `englishc` compiler in `src/` — as of commit `a0e8b77`.

Where the implementation and this document disagree, that is a bug in one of them, and worth [reporting](https://github.com/Coder8124/eng-lish/issues). Every rule here was checked against the real compiler, including the awkward ones. Where a feature is accepted by the parser but does not work all the way through to a running program, this specification says so rather than quietly pretending otherwise.

`grammar_basic.txt` in the root of the repository is **not** part of this specification. It is the original design sketch from before the language was built, and several ideas in it were never implemented. This document supersedes it.

## How to read the rules

Grammar rules are written in EBNF:

| Notation | Meaning |
|---|---|
| `"word"` | The literal word, matched **case-insensitively** |
| `a b` | `a` followed by `b` |
| <code>a &#124; b</code> | Either `a` or `b` |
| `[ a ]` | `a`, or nothing |
| `{ a }` | `a` repeated zero or more times |
| `( a b )` | Grouping |

The words **must**, **must not**, and **may** carry their usual weight: **must** describes something an implementation is required to do, and **may** describes something it is permitted to do.

## The shape of the language, in one page

An eng-lish program is a list of sentences, each ending in a period. Sentences are written to be read aloud.

```
Note: this is a comment.

use "numeric".

Let total be a standard number with value 0.

For each index from 1 to 10,
    Add index to total.
End.

If total is at least 50 then
    output "That is a big total.".
otherwise
    output total.
End.

To double with a standard number number returning a standard number:
    Give back number * 2.
End.

output the result of double with total.
```

Four properties are worth knowing before reading any further, because they explain most of what looks unusual later:

1. **Keywords are case-insensitive.** `While`, `while`, and `WHILE` are the same word. Only `Note:`, which starts a comment, insists on its capital letter.
2. **Whitespace and line breaks carry no meaning.** Indentation is for humans. Blocks are closed by writing `End.`, not by outdenting.
3. **Some keywords are several words long.** `standard number`, `lock and key list`, and `is greater than or equal to` are each a single indivisible keyword, and must be written with exactly one space between the words.
4. **Arithmetic has two forms.** Inside an expression you use the symbols `+ - * /`. As a whole sentence you use words: `Add 1 to count.` The word forms are statements and cannot appear inside an expression.
