# Claude Conventions for eng-lish

## Commits

- Commit after every response that makes changes — one commit per logical change, not one giant commit at the end.
- Do **not** add `Co-Authored-By` or any self-credit line to commit messages.
- Keep commit messages concise: a short subject line, then a blank line, then bullet points if needed.

## Code style

- No comments unless the why is genuinely non-obvious.
- No docstrings or multi-line comment blocks.
- Don't add error handling for things that can't happen.
- Don't introduce abstractions beyond what the task requires.

## eng-lish packages

- New packages go in `packages/<name>/<name>.eng`.
- Only use language constructs visible in the existing examples — don't guess at unverified syntax.
- Packages export functions and classes only; top-level statements in a package file are ignored at import time.

## General

- Don't create markdown files unless explicitly asked.
- Don't rewrite existing files unless explicitly asked.
