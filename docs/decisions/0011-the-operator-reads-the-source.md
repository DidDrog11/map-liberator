# 0011. The operator reads the source; no AI in the extraction path

Date: 2026-09-10. Status: accepted.

## Context

Reading numbers out of a table image is something a language model can
now do, and during the evaluation extraction it was tempting to use one.
The question is whether that belongs in the tool or its protocol.

## Options

1. Add model-assisted reading of the source, with the operator checking.
2. Keep the app as a structured, validated manual transcription tool.

## Decision

Option 2. The app never reads the source. AI assistance is confined to
building and testing the tool, and to after-the-fact checks that compare
a completed ledger against machine-readable totals.

## Why

The app is for people who cannot afford proprietary software or do not have
the skills to build their own process. That audience needs a tool that is:

- **offline**: it runs from a browser against files on the user's machine,
  with no service that has to be reachable or paid for;
- **lightweight**: a Shiny app and a browser, nothing to install beyond R
  and a few packages, no accounts or keys;
- **cheap**: free to run, indefinitely, with no per-page or per-token cost.

A model in the extraction path breaks all three. It also changes what the
evaluation measures: the manuscript's claims are about a human operator
using structured entry, and they only hold if the human does the reading.

## Consequences

- The evaluation extraction is done entirely by hand, including cells of
  no analytic value, because completeness is what is being measured.
- Tooling around the task is fair: the region list, blank-as-zero, the
  pause button, the archive download script. None of them read a value.
- The page-1 benchmark parser reads text, not the state table, and is used
  only to check a finished ledger against printed totals.
