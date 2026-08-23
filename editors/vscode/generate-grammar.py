import json, re, pathlib

ROOT = pathlib.Path("/Users/pragun/Lynbrook/eng-lish")

src = (ROOT / "src" / "stdlib.rs").read_text()
builtins = sorted(
    {n for blk in re.findall(r"names: vec!\[[^\]]*\]", src)
       for n in re.findall(r'"([a-zA-Z][a-zA-Z0-9_]*)"', blk)},
    key=lambda s: (-len(s), s),
)


def alt(words):
    return "|".join(
        re.escape(w).replace("\\ ", " ")
        for w in sorted(words, key=lambda s: (-len(s), s))
    )


# Every lexer token carries ignore(case) except "the", which lexer.rs:379-386
# patches after tokenizing -- so the whole grammar is case-insensitive.
CONTROL = [
    "For each", "otherwise if", "otherwise", "else", "if", "then", "while",
    "end kind", "end create", "end", "give back", "stop", "skip", "call", "ask",
]
DECL = [
    "define", "kind", "called", "extends", "property", "the following",
    "let", "be", "with value", "with", "value", "returning", "nothing",
    "to", "use", "created", "create", "set", "remove", "output", "each", "for",
    "a", "an",
]
# Compound-assignment statement keywords (parser.rs:530-570) and the
# prepositions they take. `remainder`/`quotient` are the bare prefix forms.
ARITH = [
    "add", "subtract", "multiply", "divide", "remainder", "quotient",
    "divided", "by", "from", "of",
]
TYPES = [
    "standard number", "lock and key list", "unique collection", "fixed list",
    "dictionary", "decimal", "boolean", "text", "list",
]
COMPARE = [
    "is not greater than or equal to", "is not less than or equal to",
    "is greater than or equal to", "is less than or equal to",
    "is not greater than or equal", "is not less than or equal",
    "is greater than or equal", "is less than or equal",
    "is not equal to", "is not greater than", "is not less than",
    "is not at least", "is not at most", "is not same to", "is not same",
    "is not equal", "is equal to", "is greater than", "is less than",
    "is at least", "is at most", "is same to", "is same", "is equal",
    "is not", "is",
]
LOGICAL = ["and", "or", "not", "negative"]
THE = ["the remainder of", "the quotient of", "the result of asking",
       "the result of", "divided by", "asking"]
PLOT = ["plot", "against", "titled", "histogram", "scatter", "chart",
        "line", "bar", "as"]
# repeat and standalone `following` are reserved by the lexer but unreachable
# in the parser -- coloured so nobody names a variable with them.
DEAD = ["repeat", "following"]

grammar = {
    "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
    "name": "eng-lish",
    "scopeName": "source.eng",
    "patterns": [{"include": "#" + n} for n in [
        "comments", "strings", "class-name", "function-name", "types",
        "comparison", "control", "the-forms", "arithmetic", "plot", "declaration",
        "logical", "constants", "builtins", "numbers", "operators",
    ]],
    "repository": {
        "comments": {
            "comment": "lexer.rs:6 -- `Note:` is the one case-SENSITIVE token",
            "match": r"Note:.*$",
            "name": "comment.line.note.eng",
        },
        "strings": {
            "name": "string.quoted.double.eng",
            "begin": '"',
            "end": '"',
            "beginCaptures": {"0": {"name": "punctuation.definition.string.begin.eng"}},
            "endCaptures": {"0": {"name": "punctuation.definition.string.end.eng"}},
            "patterns": [{
                "comment": "lexer.rs:306-320 -- only these five escapes are real",
                "match": r'\\[ntr"\\]',
                "name": "constant.character.escape.eng",
            }],
        },
        "class-name": {
            "match": r"(?i:\bcalled\b)\s+([A-Za-z_][A-Za-z0-9_]*)",
            "captures": {"1": {"name": "entity.name.type.eng"}},
        },
        "function-name": {
            "comment": "parser.rs:584 -- `To <name>` opens a function definition",
            "match": r"(?i:\bto\b)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?=(?i:with|returning)\b|:)",
            "captures": {"1": {"name": "entity.name.function.eng"}},
        },
        "types": {"match": r"(?i:\b(?:%s)\b)" % alt(TYPES), "name": "storage.type.eng"},
        "comparison": {"match": r"(?i:\b(?:%s)\b)" % alt(COMPARE), "name": "keyword.operator.word.eng"},
        "control": {"match": r"(?i:\b(?:%s)\b)" % alt(CONTROL), "name": "keyword.control.eng"},
        "the-forms": {"match": r"(?i:\b(?:%s)\b)" % alt(THE), "name": "keyword.other.eng"},
        "arithmetic": {"match": r"(?i:\b(?:%s)\b)" % alt(ARITH), "name": "keyword.operator.word.eng"},
        "plot": {"match": r"(?i:\b(?:%s)\b)" % alt(PLOT), "name": "keyword.other.plot.eng"},
        "declaration": {"match": r"(?i:\b(?:%s)\b)" % alt(DECL + DEAD), "name": "keyword.declaration.eng"},
        "logical": {"match": r"(?i:\b(?:%s)\b)" % alt(LOGICAL), "name": "keyword.operator.word.eng"},
        "constants": {"match": r"(?i:\b(?:true|false)\b)", "name": "constant.language.eng"},
        "builtins": {
            "comment": "generated from src/stdlib.rs `names: vec![...]`",
            "match": r"\b(?:%s)\b" % alt(builtins),
            "name": "support.function.builtin.eng",
        },
        "numbers": {
            "comment": "float first -- lexer.rs:325 needs digits on both sides",
            "patterns": [
                {"match": r"\b[0-9]+\.[0-9]+\b", "name": "constant.numeric.float.eng"},
                {"match": r"\b[0-9]+\b", "name": "constant.numeric.integer.eng"},
            ],
        },
        "operators": {"match": r"[+\-*/]", "name": "keyword.operator.eng"},
    },
}

out = ROOT / "editors" / "vscode" / "syntaxes" / "eng-lish.tmLanguage.json"
out.write_text(json.dumps(grammar, indent=2) + "\n")
print("wrote", out, "with", len(builtins), "builtins")
