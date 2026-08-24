# 2. Types

eng-lish is statically typed. Every variable has a type fixed when it is declared, and the compiler checks types before generating any code. Implemented in `src/semantic.rs`.

## 2.1 The types

```ebnf
type = "text"
     | "standard number"
     | "decimal"
     | "boolean"
     | "list" [ "of" type ]
     | dictionary-type
     | "fixed list"
     | "unique collection" [ "of" type ]
     | identifier ;
```

| Written | Holds | Notes |
|---|---|---|
| `text` | A string of characters | |
| `standard number` | A whole number | 64-bit signed |
| `decimal` | A number with a fractional part | 64-bit floating point |
| `boolean` | `true` or `false` | |
| `list of T` | Many values of one type `T` | Plain `list` means `list of standard number` |
| `dictionary` | Values looked up by a text key | Also written `lock and key list` |
| *ClassName* | An instance of a class | Any identifier is read as a class name |

Because any bare identifier is accepted as a class type, a misspelled type name is not caught while parsing. `Let x be a decmal with value 1.` parses cleanly and is rejected later, when the compiler cannot find a class called `decmal`.

## 2.2 Dictionary types

```ebnf
dictionary-type = ( "dictionary" | "lock and key list" )
                  [ ( "from" | "of" ) type "to" type ] ;
```

`dictionary` and `lock and key list` mean exactly the same thing. Written with no key and value types, a dictionary maps `text` to `standard number`.

The key type **must** be `text`. Any other key type is rejected.

```
Let ages be a new dictionary.
Let scores be a new dictionary from text to decimal.
```

## 2.3 Types that parse but do not work

Two types are accepted by the parser and then cannot be used for anything:

- **`fixed list`** always produces the empty tuple type. There is no syntax for writing its element types, and no way to construct a value of it.
- **`unique collection`** parses, and accepts an element type, but no literal and no built-in function ever produces one.

An implementation **may** accept these for compatibility. Programs **must not** rely on them; they are not part of the working language, and this specification does not define their behaviour.

## 2.4 Type compatibility

Types **must** match exactly, with one convenience: where a `decimal` is expected, a `standard number` is accepted and widened automatically. This applies to arithmetic, to arguments of built-in maths functions, and to assignment.

The reverse is not allowed. A `decimal` will not silently become a `standard number`, because that would lose the fractional part.

There is no truthiness. The condition of an `If` or a `While` **must** be a `boolean`; a number is not accepted in its place.

## 2.5 Conversions

```ebnf
conversion = ( "standard number" | "decimal" | "text" ) "of" primary ;
```

Only these three target types can be written. `boolean of x` is not conversion syntax.

The operand is a *primary* expression, not a full expression, which has two consequences worth knowing:

- `text of a + b` means `(text of a) + b`.
- `text of scores[0]` means `(text of scores)[0]`.

Parenthesise when you mean otherwise.

### What actually works

The type checker permits more conversions than the code generator implements. This table reflects what compiles all the way to a running program:

| Conversion | Type-checks | Runs | Use instead |
|---|---|---|---|
| `decimal of` a whole number | yes | **yes** | |
| `standard number of` a decimal | yes | **yes** | |
| `text of` a number | yes | **no** | `numberToText` / `decimalToText` |
| `standard number of` text | yes | **no** | `textToNumber` |
| `decimal of` text | yes | **no** | `textToDecimal` |
| `text of` a boolean | yes | **no** | — |

Every conversion involving `text` is accepted by the type checker and then fails during code generation with `Unsupported type conversion`. This is a gap in the implementation, not a rule of the language. Until it is closed, **use the built-in functions** for anything involving text:

```
Let count be a standard number with value 5.
Let label be a text with value the result of numberToText with count.
output label.
```

## 2.6 Inferred

There is one internal type, `Inferred`, used while working out the types of [beginner mode](06-beginner-mode.md) functions. It has no syntax — you cannot write it — and it **must not** survive to code generation. An implementation resolves every `Inferred` before emitting code, defaulting unresolved parameters to `standard number` and unresolved return types to nothing.
