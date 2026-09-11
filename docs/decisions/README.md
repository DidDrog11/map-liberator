# Architecture decision records

One file per decision that had real alternatives. Each states the context,
the options, what was chosen and why, and the consequences. Records are not
edited once accepted; a later change gets a new record that supersedes the
old one, so the reasoning history is kept.

Status values: **accepted**, **superseded by NNNN**, **proposed**.

| # | Decision | Status |
|---|---|---|
| [0001](0001-plain-shiny-app-not-a-package.md) | Plain Shiny app, not an R package | accepted |
| [0002](0002-long-ledger-with-character-values.md) | Long ledger, one row per region per variable, character values | accepted |
| [0003](0003-two-entry-modes.md) | Two entry modes: batch paint and per-region form | accepted |
| [0004](0004-validation-gate-with-counted-rejections.md) | Validation gate at entry, rejections counted | accepted |
| [0005](0005-blank-zero-and-explicit-na.md) | Blank-as-zero is opt-in; NA is typed explicitly | accepted |
| [0006](0006-evaluation-instrumentation-is-separable.md) | Timing and rejection counts are evaluation instrumentation, not product features | accepted |
| [0007](0007-source-loaded-as-original-pdf.md) | Reference source is the original file, shown in the browser's own viewer | accepted |
| [0008](0008-region-list-shares-the-map-contract.md) | Region list as an alternative to the map, sharing one click contract | accepted |
| [0009](0009-versioned-project-file-with-legacy-fallback.md) | Project file is a versioned list; bare data frames load as legacy | accepted |
| [0010](0010-rules-parsed-never-evaluated.md) | Consistency rules are parsed structurally, never evaluated | accepted |
| [0011](0011-the-operator-reads-the-source.md) | The operator reads the source; no AI in the extraction path | accepted |
| [0012](0012-record-only-what-the-source-shows.md) | Record only regions the source reports; absence is an analysis step | accepted |

Records 0001 to 0004, 0009 and 0010 document decisions made before this
folder existed; their dates are when the record was written.
