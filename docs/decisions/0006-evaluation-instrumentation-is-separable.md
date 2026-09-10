# 0006. Timing and rejection counts are evaluation instrumentation, not product features

Date: 2026-09-10. Status: accepted.

## Context

The manuscript needs evidence that the app is faster and less error-prone
than extraction into a spreadsheet. That needs per-report time and a count
of prevented errors, gathered during a real extraction. Users of the app
do not need either.

## Options

1. Build the measurement into the app as a permanent feature.
2. Measure externally with a stopwatch and notes.
3. Instrument the app so measurement is automatic during the evaluation,
   but keep it confined so it can be hidden or removed for release without
   touching the extraction path.

## Decision

Option 3. Each committed row records seconds since the reference file was
loaded, net of pauses, and the paused total; the sidecar has a pause
button; the workbench header shows the rejection count. All of this lives
in the sidecar's returned metadata, two ledger columns and one header
string. Nothing else reads them.

## Why

- A stopwatch cannot separate reading time from entry time per row, and
  pauses for orientation would contaminate the measurement.
- Recording per row rather than per report lets the analysis look at
  where time goes, which is the question a reviewer will ask.
- Keeping it separable means the shipped app can drop it without a
  redesign, and the evaluation can be described honestly as instrumentation
  added for the study.

## Consequences

- The ledger columns `Secs_Since_Image_Load` and `Secs_Paused` may be
  absent from ledgers produced by a release build; analysis code must
  tolerate that.
- The pause is operator-controlled, so timing depends on discipline; the
  protocol in `analysis/sitrep_formats.md` says when to pause.
- Removing or hiding the instrumentation before release is a decision to be
  recorded when taken.
