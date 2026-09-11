# 0012. Record only regions the source reports; absence is an analysis step

Date: 2026-09-11. Status: accepted.

## Context

A per-region table lists every region, most with empty cells. Extracting
it completely means entering a row of zeros for every region with nothing
reported, which is most of the keystrokes in a typical week. The question
is whether those zeros are extraction or analysis.

## Options

1. Enter every region every week, zeros included, so the ledger is
   rectangular.
2. Enter only regions with at least one reported value; leave the rest
   absent from the ledger and let the analysis impute zero where the
   source's coverage makes that the right reading.

## Decision

Option 2 is the recommended practice. It is a recommendation to operators,
not a constraint in the app: nothing stops an operator entering zeros.

## Why

You cannot extract what the source does not show. An empty cell for a
region is the absence of a report, and turning it into a zero is an
inference about what absence means for that source in that week. That
inference belongs in the analysis, where it can be stated once and applied
consistently, rather than in the extraction, where it is typed hundreds of
times and cannot be distinguished from a reported zero afterwards.

It also matters that the ledger stays honest about provenance: every row
in it corresponds to something printed on the page.

## Consequences

- Downstream analysis must complete the region-by-week grid explicitly,
  filling zero where the source covered the region and NA where it did
  not (a region absent from the table entirely, or a week with no report).
- The blank-as-zero option (0005) still applies to the regions that are
  entered: a region with one non-empty cell has its empty cells recorded
  as 0, because within a listed row an empty cell is a printed zero.
- The tutorial states the recommendation in plain terms.
