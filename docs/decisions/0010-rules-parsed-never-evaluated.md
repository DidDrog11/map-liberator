# 0010. Consistency rules are parsed structurally, never evaluated

Date: 2026-09-10 (records an earlier decision). Status: accepted.

## Context

Operators type cross-field rules such as `deaths <= confirmed` into a text
box. The app is publicly hosted. Anything typed by a user is untrusted.

## Options

1. Evaluate the rule text as R (`eval(parse(text = ...))`) against the
   form values. Simple and flexible.
2. Parse each rule into left side, operator, right side; require the left
   side to be a declared variable name and the right side a declared name
   or a number; compare numerically.

## Decision

Option 2. Anything that is not of that shape is reported as an error
against the rule, not executed. `system("echo pwned") <= 1` parses as an
unknown variable.

## Why

Option 1 is remote code execution on the server for anyone who can reach
the app. The flexibility it offers is not needed: the rules users want are
comparisons between declared variables.

## Consequences

- Rule syntax is deliberately limited to one comparison per line.
- The property has a regression test; a future contributor who reaches for
  `eval` will break it.
