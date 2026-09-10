# 0002. Long ledger, one row per region per variable, character values

Date: 2026-09-10 (records an earlier decision). Status: accepted.

## Context

Extracted values must be stored in a session, saved to disk, reloaded weeks
later, and exported to CSV. Users declare their own variables, of mixed
types, and add variables mid-project.

## Options

1. Wide table: one column per variable, typed columns.
2. Long table: one row per region per variable, with a single character
   `Value` column, plus provenance columns.

## Decision

Option 2. Every committed value is a row carrying region id and name,
variable, value as text, entry mode, reference file, timestamps and timing.

## Why

- A wide table's shape depends on the schema, so adding a variable or
  loading a project with a different schema forces a migration. A long
  table never changes shape.
- Mixed types in one column force it to be character. Typing is enforced at
  entry by the validator (see 0004), which is where a wrong value can still
  be corrected, rather than at storage, where it can only be coerced.
- Old ledgers load into new versions: missing columns are filled with NA
  when rows are bound.

## Consequences

- Sorting the value column in the app is alphabetical.
- Analysis pivots to wide and casts using the schema saved with the project.
- `NA` in the value column is a real missing value, distinct from "0".
