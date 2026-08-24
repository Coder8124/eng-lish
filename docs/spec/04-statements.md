# 4. Statements

A statement is one sentence, and nearly all of them end in a period.

## 4.1 Closing a block

Loops, functions, and methods **must** be closed with `End.` Constructors close with `End create.` and classes with `End kind.`

`If` is the exception: **after an `If`, the `End.` is optional.** An `If` also ends at the next `otherwise`, `else`, `End`, `End create`, `End kind`, or at the end of the program. Both of these are valid:

```
If count is at least 1 then
    output count.
End.

If count is at least 1 then
    output count.
```

`While` and `For each` have no such leniency — leaving off `End.` there is an error. This asymmetry is real, and worth remembering.

## 4.2 Declaring a variable

```ebnf
declaration = "Let" identifier "be" [ "a" | "an" ] type "with value" expression "." ;
```

```
Let count be a standard number with value 0.
Let name be a text with value "Ada".
Let ready be a boolean with value true.
Let scores be a list of decimal with value [1.5, 2.5].
```

The article `a` or `an` is optional, and either one may be used regardless of the following word.

### Declaring a dictionary

```ebnf
dict-declaration = "Let" identifier "be" [ "a" | "an" ] ( "new" | "empty" )
                   ( "dictionary" | "lock and key list" )
                   [ ( "from" | "of" ) type "to" type ] "." ;
```

```
Let ages be a new dictionary.
Let scores be a new dictionary from text to decimal.
```

### Declaring an object

```ebnf
object-declaration = "Let" identifier "be" [ "a" | "an" ] identifier
                     "created with" arguments "." ;
```

```
Let counter be a Counter created with 0.
```

## 4.3 Assignment

```ebnf
assignment = "Set" identifier "to" expression "."
           | "Set" identifier "[" expression "]" "to" expression "."
           | "Set" "the" identifier "of" identifier "to" expression "." ;
```

```
Set count to 10.
Set scores[0] to 99.
Set ages["Alice"] to 13.
Set the width of box to 5.
```

The property form requires the word `the`.

## 4.4 Arithmetic sentences

```ebnf
compound = "Add" expression "to" identifier "."
         | "Subtract" expression "from" identifier "."
         | "Multiply" identifier "by" expression "."
         | "Divide" identifier "by" expression "." ;
```

These read as instructions rather than as equations:

```
Add 1 to count.            Note: count becomes count + 1
Subtract 2 from total.     Note: total becomes total - 2
Multiply total by 3.       Note: total becomes total * 3
Divide total by 2.         Note: total becomes total / 2
```

The value being added may be any expression: `Add 2 * index to total.` is fine. The thing being changed **must** be a plain variable name.

For `Multiply` and `Divide`, whatever comes before `by` **must** be a plain name — `Add count by 1.` is an error, and so is multiplying into an expression.

### Legacy forms

Two older forms still parse and compile identically:

```
Multiply 2 to total.       Note: legacy -- same as Multiply total by 2.
Divide 2 from total.       Note: legacy -- same as Divide total by 2.
```

They read backwards and **should not** appear in new code. An implementation **must** continue to accept them.

## 4.5 Output

```ebnf
output = "output" expression "." ;
```

```
output "Hello!".
output count.
```

## 4.6 If

```ebnf
if = "If" expression [ "then" ] [ "," ] block
     { ( "otherwise" | "else" ) "if" expression [ "then" ] [ "," ] block }
     [ ( "otherwise" | "else" ) [ "," ] block ]
     [ "End" [ "." ] ] ;
```

Both `then` and the comma are optional. `otherwise` and `else` mean the same thing; `otherwise` is the form used throughout the documentation. `otherwise if` chains as far as you like.

```
If score is at least 90 then
    output "A".
otherwise if score is at least 80 then
    output "B".
otherwise
    output "Keep going.".
End.
```

The condition **must** be a boolean.

## 4.7 Loops

```ebnf
while = "While" expression [ "," ] block "End" "." ;
for-each = "For each" identifier "from" expression "to" expression [ "," ] block "End" "." ;
```

```
While count is less than 10,
    Add 1 to count.
End.

For each index from 1 to 5,
    output index.
End.
```

`For each` counts over a range of whole numbers, inclusive at both ends. The loop variable is a `standard number`, and both bounds **must** be whole numbers.

**There is no `for each … in …` form.** To walk a list, count over its indices:

```
For each index from 0 to 2,
    output scores[index].
End.
```

Two statements control a loop from inside it, and both **must** appear inside one:

```
stop.      Note: leave the loop entirely
skip.      Note: jump to the next turn
```

## 4.8 Calling things

```ebnf
call = "Call" identifier [ "with" arguments ] "." ;
ask = "Ask" identifier "to" identifier [ "with" arguments ] "." ;
```

`Call` runs a function and throws away the answer. `Ask` runs a method on an object.

```
Call greet.
Call greet with "Ada".
Ask counter to increment.
```

To keep the answer, use the expression forms from [3.5](03-expressions.md) instead:

```
output the result of double with 21.
output the result of asking counter to getValue.
```

Note that `Ask` has nothing to do with asking the user a question. To read input, call the built-in `readLine` or `readNumber`:

```
Let name be a text with value the result of readLine.
```

## 4.9 Returning

```ebnf
return = "Give back" [ expression ] "." ;
```

```
Give back total.
Give back.          Note: leaves a function that returns nothing
```

## 4.10 Removing from a dictionary

```ebnf
remove = "Remove" expression "from" identifier "." ;
```

```
Remove "Alice" from ages.
```

## 4.11 Plotting

```ebnf
plot = "plot" expression [ "against" expression ]
       [ "as" [ "a" | "an" ] chart-kind [ "chart" ] [ "plot" ] ]
       [ "titled" text-literal ]
       "to" text-literal "." ;
chart-kind = "line" | "bar" | "scatter" | "histogram" ;
```

```
plot values as a bar chart titled "Results" to "results.html".
plot heights against weights as a scatter plot to "people.html".
```

The data **must** be a list, and so **must** the `against` series. The words `chart` and `plot` after the kind are decoration, so `as a bar chart` and `as a scatter plot` both read naturally.

The `to "file"` part is **required**, and both it and the title **must** be literal text, not a variable. If the kind is left out, a line chart is drawn.

## 4.12 Statements made of an expression

A sentence **may** consist of an expression alone, but only when it begins with a name. This means a sentence cannot start with `the`: `the result of f with 1.` is not a valid statement. Use `Call f with 1.` instead.
