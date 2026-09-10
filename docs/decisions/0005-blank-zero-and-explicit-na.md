# 0005. Blank-as-zero is opt-in; NA is typed explicitly

Date: 2026-09-10. Status: accepted.

## Context

The NCDC state tables leave zero cells empty. A report can also lack a
column entirely (the early 2020 tables have no deaths column) while page 1
reports a national total. So an empty field can mean zero, or not reported,
depending on the source, and the two must not be confused in the ledger.

## Options

1. Blank is always refused; the operator types 0 into every empty cell.
2. Blank is always zero.
3. Blank is refused by default; a per-project option records blank numeric
   fields as 0; a typed `NA` records a missing value regardless.

## Decision

Option 3. The option is a checkbox beside the schema. `NA` is accepted in
count, numeric and binary fields only; text and ordinal fields are always
required.

## Why

- Option 1 makes most keystrokes zeros and turns every slip into a counted
  rejection that measures nothing.
- Option 2 silently records zero for a variable the source never reported.
- Under option 3 the ledger distinguishes a number, a zero from an empty
  cell, and a missing value, and the choice of convention is the operator's
  and is recorded in the project notes.

## Consequences

- The option is not yet saved in the project file; it must be re-ticked
  after loading. Persisting it would be a small change and a new record.
- A rule touching a missing value is skipped (see 0004).
