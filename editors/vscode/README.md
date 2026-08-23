# eng-lish for VS Code

Syntax highlighting and snippets for [eng-lish](https://github.com/Coder8124/eng-lish) programs (`.eng` files).

## What you get

- **Colours** for keywords, types, comparison phrases, strings, numbers, `Note:` comments, and all 124 built-in function names.
- **Snippets** for the shapes you write most: `let`, `if`, `while`, `foreach`, `to` (function), `kind` (class), `dict`, `plot`, `use`, `beginner`. Type the short name and press Tab.
- **Bracket matching** and auto-closing for `(`, `[`, and `"`.

## Installing it

You need the extension folder where VS Code looks for extensions.

**Link it (best while you are working on the extension):**

```bash
ln -s "$PWD/editors/vscode" ~/.vscode/extensions/eng-lish
```

Run that from the root of the eng-lish repo, then restart VS Code.

**Or package it into a file you can share:**

```bash
npm install -g @vscode/vsce
cd editors/vscode
vsce package
```

That makes `eng-lish-0.1.0.vsix`. Install it with **Extensions → … → Install from VSIX**, or:

```bash
code --install-extension eng-lish-0.1.0.vsix
```

## Compiling from inside VS Code

The repo ships build tasks in `.vscode/tasks.json`. Press **Cmd+Shift+B** (Ctrl+Shift+B on Windows/Linux) to compile the file you are looking at. There are also tasks for "Compile and Run", "Print LLVM IR", and "Build Compiler".

Compiler errors are printed in the terminal as `Line 12: something went wrong`. They are not yet clickable, because `englishc` does not print the file name next to the line number — fixing that in the compiler is a good first contribution.

## Changing the highlighting

`syntaxes/eng-lish.tmLanguage.json` is **generated**. The list of built-in function names is read straight out of `src/stdlib.rs` so the two can never drift apart. If you add a keyword or a built-in, regenerate instead of hand-editing:

```bash
python3 editors/vscode/generate-grammar.py
```

Edit the keyword lists at the top of `generate-grammar.py` to add new syntax. Two rules matter:

- **Everything is case-insensitive.** Every token in `src/lexer.rs` is declared with `ignore(case)`, so `While`, `while`, and `WHILE` are all the same word. The generated patterns are wrapped in `(?i: … )`.
- **Longest phrase first.** eng-lish has multi-word keywords like `lock and key list` and `is greater than or equal to`. The generator sorts every alternation longest-first so the long phrase wins instead of matching just `list` or `is`. Getting this backwards is the usual cause of a phrase highlighting in pieces.

Note that the playground has a second, simpler highlighter in `playground/static/index.html` (a keyword list compiled into one regex). It is a separate implementation — if you add a keyword, update both.
