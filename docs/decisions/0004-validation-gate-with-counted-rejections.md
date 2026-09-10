# 0004. Validation gate at entry, rejections counted

Date: 2026-09-10 (records an earlier decision). Status: accepted.

## Context

Transcription error is the failure mode of hand extraction. The alternative
to this app, a spreadsheet, accepts anything typed into a cell.

## Options

1. Accept any value and clean afterwards.
2. Declare each variable's type and, optionally, cross-field rules; refuse a
   commit that breaks them, and say why.

## Decision

Option 2. The operator declares variables as `name, type` (count, numeric,
binary, ordinal with levels, text) and optional comparisons such as
`confirmed <= suspected`. A form commit is validated field by field, then
against the rules; a failure is shown next to the form and the commit does
not happen. Every refused commit increments a visible counter.

## Why

- Refusing at entry is the only point where the operator still has the
  source in front of them and can look again. Cleaning afterwards means
  guessing.
- Counting refusals makes the claim "typed entry prevents errors"
  measurable: the count is the number of entries that would have entered a
  spreadsheet wrong.

## Consequences

- Form mode requires a valid schema before any region can be entered.
- The count is evaluation evidence (see 0006) as well as feedback.
- Rules that cannot be evaluated (a text variable, a missing value) are
  skipped rather than treated as violations, so validation never refuses a
  value on grounds the operator cannot act on.
