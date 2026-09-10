# 0009. Project file is a versioned list; bare data frames load as legacy

Date: 2026-09-10 (records an earlier decision). Status: accepted.

## Context

Saving originally wrote the ledger data frame alone. Adding schema
persistence needed more in the file, and existing project files, including
the published Lassa dataset, had to keep opening.

## Options

1. Change the saved object and accept that old files break.
2. Save a named list with a format marker and version; on load, sniff the
   object and treat a bare data frame as version 1.

## Decision

Option 2. The file holds `format`, `version`, `saved_at`, `ledger`,
`schema_text` and `rules_text`. Saving always writes the current version.
Loading a legacy file returns a NULL schema so the operator's current
declarations are left in place rather than blanked.

## Why

- Users keep project files for months; breaking them is a data-loss event.
- A pure build and read pair can be unit-tested without Shiny.

## Consequences

- New per-project settings (for instance blank-as-zero, see 0005) need a
  version bump and a read path that tolerates their absence.
- Save is a browser download; there is no server-side store, by design for
  a public deployment.
