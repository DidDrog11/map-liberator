# 0008. Region list as an alternative to the map, sharing one click contract

Date: 2026-09-10. Status: accepted.

## Context

For a tabular source the operator already knows the region's name.
Finding its polygon on the map is a wasted step per entry, and in tables
sorted by case count the region's position changes week to week.

## Options

1. Map only; rely on tooltips and zoom.
2. A separate table-entry mode with its own ledger path.
3. A list view of the loaded regions that emits exactly what the map emits,
   so the workbench does not know which view produced a click.

## Decision

Option 3. `mod_region_list` returns `list(click, selected)`, the same shape
as the map engine. `app.R` owns a Map / Region list switch, forwards clicks
from both through one event holder so switching views never replays a
stale click, and hands the combined source to the workbench. The list has
a search box, an Entered column keyed on the current reference file, and
an optional recently-entered-first order derived from earlier files only.

## Why

- One ledger path means one set of validation, provenance and tests.
- The list shows progress per region for the current file, which the map
  cannot without colouring polygons.
- Ordering by earlier entry mirrors a table's order once followed once,
  without pretending to reproduce the source's own cumulative ordering,
  which the ledger can only approximate.

## Consequences

- Form mode clears the row selection after each click so a row can be
  re-clicked to correct an entry.
- Ordering by the source's cumulative counts was considered and rejected:
  an order that is nearly right costs more than one that is plainly
  alphabetical.
