---
name: run-eng
description: Compile and run an eng-lish .eng file
allowed-tools: Bash
---

Compile and run the eng-lish file given in $ARGUMENTS:

1. Run: `englishc $ARGUMENTS`
2. If compilation succeeds, run the output binary: `./${ARGUMENTS%.eng}`
3. Show the program output clearly.
4. If there are errors, quote the exact error lines and explain what they mean in plain English.
