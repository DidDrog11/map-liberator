# 0003. Two entry modes: batch paint and per-region form

Date: 2026-09-10 (records an earlier decision). Status: accepted.

## Context

Sources come in two shapes. A choropleth or presence map assigns one value
to many regions at once. A table or labelled map gives each region several
different values.

## Options

1. One mode: select regions, add one variable at a time. Tables become many
   passes over the same regions.
2. One mode: a form per region. Choropleths become one form per region even
   when fifty regions share a value.
3. Both, chosen by the operator.

## Decision

Option 3. Batch mode paints one variable onto a selection. Form mode opens a
validated form for one region on click. Both write through the same row
builder into the same ledger and can be mixed in one project.

## Why

Each mode is the natural motion for its source shape, and forcing either
onto the other shape multiplies keystrokes without adding information. The
cost is one radio button and a second code path that shares its tail.

## Consequences

- Controls that only make sense in one mode are hidden in the other.
- The form is built from the declared schema (see 0004), so form mode
  requires a schema; batch mode does not.
