# 1. Lexical structure

How the compiler turns program text into tokens. Implemented in `src/lexer.rs`.

## 1.1 Characters and whitespace

Source text is UTF-8. Outside of string literals, only ASCII is meaningful.

Spaces, tabs, carriage returns, and newlines separate tokens and carry no other meaning. A program **may** be written on one line or spread across many; indentation has no effect on how it is parsed. Blocks are closed with an explicit `End.`

## 1.2 Case

Every keyword is matched **case-insensitively**. `While`, `while`, `WHILE`, and `wHiLe` are one and the same keyword.

There is exactly one exception, and it runs the other way: the comment marker `Note:` **must** be written with a capital `N`. Lowercase `note:` is not a comment, and will fail to parse.

Identifiers preserve the case you write, and are compared case-sensitively. `total` and `Total` are two different variables.

## 1.3 Comments

```ebnf
comment = "Note:" { any character except newline } ;
```

A comment starts at `Note:` and runs to the end of the line. It may start a line or follow code on the same line.

```
Note: this whole line is a comment
Let count be a standard number with value 1.   Note: so is this part
```

There are no block comments. `Note:` inside a string literal is just text.

## 1.4 Multi-word keywords

Several keywords are made of more than one word, and are matched as a **single indivisible token**:

`standard number` · `lock and key list` · `fixed list` · `unique collection` · `at least` · `at most` · `the following` · `end kind` · `end create`

These **must** be written with exactly one ASCII space between the words. Two spaces, or a line break in the middle, will not match — `standard  number` is not the type `standard number`.

When one keyword is a prefix of another, the longest match wins: `lock and key list` is one token, not `lock` followed by `list`; `end kind` is one token, not `end` followed by `kind`.

## 1.5 Literals

### Numbers

```ebnf
integer = digit { digit } ;
decimal = digit { digit } "." digit { digit } ;
```

A decimal literal **must** have at least one digit on each side of the point. `3.14` is a decimal; `3.` and `.5` are not decimal literals. This is what lets a sentence end in a period without ambiguity: in `output 3.` the `3` is an integer and the `.` ends the sentence.

Literals are never negative. There is no unary minus in eng-lish — write `negative 5`.

### Text

```ebnf
text = '"' { character | escape } '"' ;
escape = "\n" | "\t" | "\r" | '\"' | "\\" ;
```

Exactly five escape sequences are recognised:

| Escape | Means |
|---|---|
| `\n` | Newline |
| `\t` | Tab |
| `\r` | Carriage return |
| `\"` | A double quote |
| `\\` | A backslash |

Any other backslash sequence is **not** an error and is **not** interpreted: `"\q"` is the two characters `\` and `q`. There are no `\0` and no `\u` escapes.

A literal newline inside the quotes is allowed, so text may span lines.

### Truth values

```ebnf
boolean = "true" | "false" ;
```

Both are case-insensitive, like every other keyword.

## 1.6 Identifiers

```ebnf
identifier = ( letter | "_" ) { letter | digit | "_" } ;
```

Names of variables, functions, parameters, properties, and classes. ASCII letters, digits, and underscores; the first character **must not** be a digit.

Keywords win over identifiers, so a keyword cannot normally be used as a name. **Single letters are a common trap:** `a` and `an` are keywords, so `Let a be a standard number with value 1.` does not work. Use a descriptive name.

## 1.7 Reserved words

Every word below is a keyword and **must not** be used as a name, except as noted in 1.8.

```
let  be  a  an  with  value
text  standard number  decimal  boolean  list  dictionary
lock and key list  fixed list  unique collection
true  false
add  subtract  multiply  divide  remainder  quotient  divided
to  by  from  of  as  against  titled
same  equal  is  not  greater  less  than  at least  at most
if  then  else  otherwise  while  repeat  for  each  stop  skip
output  plot  chart  line  bar  scatter  histogram
returning  nothing  give  back  call  result  and  or  negative
end  end kind  end create
define  kind  property  the  the following  following
created  create  ask  asking  extends  set  remove  use
```

Two of these are reserved but unreachable: **`repeat`** and standalone **`following`** are recognised by the lexer but no rule in the grammar uses them. They **must not** be used as names, and an implementation **may** reject them with a clearer message.

Note that `called` and `that`, which appear in class definitions, are **not** keywords — they are ordinary identifiers that the parser matches by name, and they may be used as variable names.

## 1.8 Keywords admitted as names

Ten keywords are deliberately allowed as identifiers, because they read naturally as names:

`result` · `value` · `kind` · `property` · `a` · `an` · `add` · `subtract` · `multiply` · `divide`

So `Let value be a standard number with value 3.` is legal, and in beginner mode a function may be called `add`. Note that `a` and `an` are on this list but still cannot be *declared* as variables in the usual way, because the article is consumed first.

Everything else in 1.7 is off limits: `Let line be …` and `Let set be …` are errors.
