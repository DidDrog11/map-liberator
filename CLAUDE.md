# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Map Liberator is a single-page R Shiny app for "digitising" static maps (PDF/PNG situation reports) into tabular data. The user loads GADM administrative boundaries for a country, aligns a reference image side-by-side, clicks polygons to paint values, and exports CSV. There is no package structure and no build step — it is a plain Shiny app run from the project root, with a testthat suite under `tests/`.

## Commands

```r
# Run the app (from project root — paths in global.R depend on getwd())
shiny::runApp()

# One-time data generation (required before the app is usable)
source("setup.R")
```

```r
# Run the test suite (from the project root)
Rscript tests/run_tests.R
# or, interactively:
testthat::test_dir("tests/testthat")
```

`tests/README.md` lists every test as a plain-language claim, split into domain rules (the author reviews and ticks these) and plumbing; update it whenever a test is added or changed. Test-only dependencies: `testthat`, `withr`, `pdftools` (the benchmark-parser tests skip silently without pdftools, so a green run on a machine lacking it is not full coverage). No package scaffolding: `tests/testthat/helper-app.R` sources `global.R` and every module, resolving the project root relative to the test directory. Shiny modules are exercised with `shiny::testServer`. Note that `state_manager_server` returns a *list* of reactives, so tests read `session$returned$ledger()`, not `session$returned()`.
`setup.R` downloads GADM boundaries via `geodata::gadm()`, aggregates them upward to levels 3/2/1/0, simplifies them, and writes `data/gadm/gadm41_<ISO3>_<level>_pk.rds`. Set `TARGET_COUNTRIES` at the top of `setup.R` to `c("GBR", "NGA")` etc. instead of the default `"ALL"` — running "ALL" downloads every country and takes hours. `process_country()` skips a country when its level-0 file already exists, so re-running is cheap/resumable.

`data/gadm` is gitignored (GADM licence), so a fresh clone has no spatial data and every country load will fail until `setup.R` has run.

Dependencies (installed manually, no renv/DESCRIPTION). App: `shiny`, `shinyjs`, `bslib`, `leaflet`, `sf`, `terra`, `dplyr`, `countrycode`, `DT`, `base64enc`. `setup.R` only: `geodata`, `rmapshaper`. `analysis/` only: `pdftools`.

`shinyWidgets` and `shinycssloaders` were removed from `global.R` - nothing in the app called them. `geodata` is no longer loaded at app start either; it is a `setup.R` dependency. Keep it that way: every `library()` in `global.R` is bundled into the shinyapps.io deployment and paid for on cold start.

## `docs/decisions/`

Architecture decision records, one file per decision with real alternatives (layout, ledger shape, entry modes, validation, blank/NA semantics, evaluation instrumentation, PDF sidecar, region list, project file, rule parsing, no AI in the extraction path). Records are not edited after acceptance; a change gets a new superseding record. Write one when a change alters a recorded decision or introduces a new one with trade-offs.

## `analysis/`

Manuscript/validation code, deliberately outside the app: `app.R` sources only `global.R` and an explicit list of `R/mod_*.R`, so nothing here is ever loaded by the running application. `parse_ncdc_benchmark.R` recovers the page-1 figures from NCDC sitreps (national totals, states/LGAs affected, named states) as ground truth for scoring an extraction. Every field fails soft into a `parse_flags` column rather than a silent `NA`.

`download_ncdc_sitreps.R` fetches the full NCDC Lassa sitrep archive into `data/sitreps/` (PDFs gitignored, `manifest.csv` kept), names each file by year and epi week from the listing's `download` attribute, reads the week and Table 3 page back out of each PDF (`verify_ncdc_sitreps()`), and renders the Table 3 page to PNG for the sidecar (`render_ncdc_table3()`). Re-running resumes. `check_ledger_against_table1.R` compares a project's per-document sums with the Table 1 current-week totals from page 1; where the text layer is unusable it falls back to `ocr_table1.R` (tesseract on a rendered, cropped, upscaled Table 1, digits-only pass plus labelled rows) and cross-checks against the difference of consecutive cumulative rows. Outputs and per-year review sheets live in `analysis/output/`. `sitrep_formats.md` documents the layout eras: no per-state table before 2020 week 7, a stable twelve-column table after, and a broken text layer through 2021 to mid 2022 that makes caption detection fall back to "page 4, assumed".

## Architecture

`app.R` sources `global.R` then each `R/mod_*.R` explicitly (there is no auto-loading — a new module file must be added to the `source()` block at the top of `app.R`). Modules communicate by returning lists of reactives that the caller passes down as arguments; there is no shared global reactive store.

Wiring in `app.R`'s server:

```
controls_server("ctrl") ──> controls_out ──> map_engine_server("map")
                                  │                    │
                                  └──────┬─────────────┘
                                         v
                             workbench_server("workbench") ──> bridge_data ──> state_manager_server("state")
                                         ^                                              │
                                         └──────────── loaded_state ────────────────────┘
```

`bridge_data` is a `reactiveVal` in `app.R` that breaks the circular dependency between the workbench (produces rows) and the state manager (saves rows, and loads rows back into the workbench). `restored_schema` is the mirror image: the state manager loads a schema, and `mod_controls` restores it into the schema inputs. Both exist because neither module can be constructed before the other.

### Module responsibilities

- **`mod_controls.R`** — the largest module. Owns project metadata, country/level selection, admin-level filters, and the data-loading pipeline. Takes `restore_schema` (from a loaded project) and `restore_metadata` (from `metadata_from_rows()`, which `app.R` computes when a source document that already has ledger rows is loaded, so the panel refills with that document's source, dates and week). Returns `geom_data`, `context_data`, `geom_trigger`, `metadata`, `add_trigger`, `clear_trigger`.
- **`mod_map_engine.R`** — Leaflet wrapper. Renders target + context layers, tracks `selected_ids`, and redraws a single polygon on click rather than the whole layer.
- **`mod_region_list.R`** — tabular alternative to the map: the loaded target layer listed by name with a search box and an "Entered" column counting values already recorded for each region against the current sidecar image. Returns `list(click, selected)`, the same contract as the map engine. `app.R` owns a `view_mode` radio (Map / Region list), forwards clicks from both views through one `last_click` reactiveVal so switching views never replays a stale click, and hands the combined `region_source` to the workbench. Form mode clears the row selection after each click so the same row can be clicked again.
- **`mod_workbench.R`** — the data ledger (`DT` table): appends one row per selected region on `add_trigger`, supports delete + CSV download. The schema is documented in the file header; it is deliberately long/tidy (one row per region per variable), so ledgers written by older versions still load - `bind_rows` fills missing columns with `NA`.
- **`mod_schema.R`** - lets the operator declare the variables being extracted (`name, type` per line, types `count`/`numeric`/`binary`/`ordinal`/`text`) plus optional cross-field rules. Nested inside `mod_controls`. Rules are parsed structurally into (lhs, op, rhs) and compared numerically - never `eval(parse())`, because the app is publicly hosted and this text is operator-supplied. Also exposes `blank_zero`, an opt-in checkbox that makes `validate_value()` record an empty count/numeric/binary field as `0` instead of refusing it (NCDC tables leave zero cells blank).  Independently of that flag, typing `NA` into a count/numeric/binary field records a missing value, for a variable the source does not report at all; rules touching a missing value are skipped rather than violated.
- **`mod_state_manager.R`** - saves and loads the project. The file is a named list (`format`, `version`, `saved_at`, `ledger`, `schema_text`, `rules_text`, and from v3 `project_name` and `blank_zero`, defaulting when absent); a bare data frame is read as the legacy v1 format, so projects saved before schema persistence - including the published Lassa dataset - still open. Saving always writes the current version. `build_project_state()` and `read_project_state()` are pure and unit-tested without Shiny. Returns `list(ledger, schema)`; `schema` carries schema text, rules text, project name and blank-as-zero, and is NULL for a legacy file so the operator's current declarations are not blanked. `mod_controls` restores the project name and `mod_schema` the checkbox.
- **`mod_sidecar.R`** — reference upload. Images are base64-inlined, transformed with CSS `rotate()`/`scale()`, and drag-to-pan via two CSS variables. PDFs are shown in the browser's own viewer through an `<iframe>` served from a per-session `addResourcePath()` (random prefix from `session$token`, removed on session end), because data-URI PDFs hit browser size limits; `global.R` raises `shiny.maxRequestSize` to 50 MB for them. Returns a reactive carrying the current image's `file_name`, `loaded_at`, `paused_at` and `paused_secs`. The clock starts on upload and can be paused from the sidebar; the workbench stamps active seconds (`Secs_Since_Image_Load`, net of pauses) and `Secs_Paused` onto every committed row.
- **`mod_scope.R`** — **dead code.** An earlier version of the geography controls, superseded by `mod_controls.R` and not sourced by `app.R`. Don't edit it expecting a behaviour change.

### Entry modes

The workbench commits in one of two modes, chosen by `entry_mode` in `mod_controls`:

- **`single`** - one variable painted across a batch of selected regions, committed by the "Add Data to Ledger" button. Suits a choropleth, where many regions share a value.
- **`form`** - clicking a region opens a modal built from the schema, validated per-field and against the cross-field rules before anything reaches the ledger. Suits a tabulated source where each region carries several different values.

Both funnel through `build_rows()`, which recycles `ids`/`variables`/`values` against each other, so the ledger stays long/tidy either way. Commits blocked by validation are counted and surfaced in the workbench header - that count is the evidence for the manuscript's error-prevention claim.

### The load pipeline (mod_controls.R)

Loading is a hand-rolled state machine, not a single reactive: `engine_step()` holds a string (`"init"` → `"do_s1"` → `"prep_s2"` → `"do_s2"` → `"prep_s3"` → `"do_s3"` → `"finish"` → `"close"`) and one `observe()` dispatches on it. The `prep_*` steps only flip a spinner icon and `invalidateLater(100)` so the modal repaints before the blocking `sf` work starts — that's the whole reason the steps are split. When adding a stage, add both a `prep_` and a `do_` step and keep the `invalidateLater` between them, or the progress modal freezes.

Loading is deliberately two-phase in the UI: **1. Load Data** runs the pipeline into `loaded_cache`, and only then does **2. Visualise Map** copy it into `geometry_output` and bump `geom_trigger`. Heavy geometry work never happens on the render path.

### Geometry conventions

- `load_gadm_locally(iso, level)` in `global.R` reads the exact-level file if present, else falls back to the level-3 file and aggregates upward with `st_union()`. Everything is transformed to EPSG:4326 and passed through `validate_and_repair()` (`st_make_valid`).
- `layerId` is the join key between the map and the ledger. It is set to `GID_<target_level>`, falling back to a row index when that column is missing. Leaflet click IDs, `selected_ids`, and `Region_ID` in the ledger are all this value.
- `add_hierarchy_label()` builds the `Tooltip` column (HTML) used as the Leaflet `label`. Call it on any geometry destined for the target layer.
- Context layers (`Adm0`/`Adm1`/`Adm2`) are drawn non-clickable in the `context_pane` (zIndex 390) beneath the interactive target layer in `geom_pane` (400), with line weight increasing at higher admin levels. Target-layer line weight adapts down as polygon count rises (>500, >1000).

### Leaflet resize caveat

Toggling map height or the reference-image panel only toggles CSS classes; the Leaflet instance must then be told to recompute. `app.R` does this with `runjs()` + `setTimeout` calling `invalidateSize()` after the CSS transition. The two handlers reach the map differently — `toggle_size` looks up `.leaflet-container` inside `#map_col` via `HTMLWidgets.getInstance`, `toggle_ref` uses `HTMLWidgets.find` on `#map-map`. Any change to the map's DOM id or wrapper must keep both selectors working, and the timeout must stay longer than the `0.4s` CSS transition.
