# 0001. Plain Shiny app, not an R package

Date: 2026-09-10 (records an earlier decision). Status: accepted.

## Context

The app is deployed to shinyapps.io and run locally from a clone. R
packages bring `R CMD check`, roxygen documentation, a `DESCRIPTION` with
declared dependencies, and a conventional layout that JOSS reviewers
recognise.

## Options

1. Keep a flat layout: `app.R`, `global.R`, `R/mod_*.R`, sourced explicitly.
2. Convert to a package with the app served by a `run_app()` function.

## Decision

Option 1 for now. `app.R` sources `global.R` and an explicit list of module
files. Dependencies are listed in `CLAUDE.md` and `README.md` rather than a
`DESCRIPTION`.

## Why

- The deployment target is a directory of files; shinyapps.io bundles
  whatever `library()` calls appear at start-up. A package changes how the
  bundle is built and what gets installed on cold start.
- The audience is people who will run the app, not people who will call its
  functions from R. A package's API surface is not the product.
- Converting is cheap later and hard to undo: the module files already have
  no side effects on load.

## Consequences

- Tests live under `tests/testthat/` with a helper that sources the modules,
  rather than `testthat` package discovery.
- A JOSS submission will likely want a `DESCRIPTION` at least for
  dependency declaration. That is a deliberate future decision, to be taken
  with the deployment path in view, and will supersede this record.
