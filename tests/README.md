# What the tests assert, and why

The suite under `tests/testthat/` was written with Claude Code. This file
lists every test as a plain-language claim so the author can review the
behaviours being asserted without reading the test code. It is kept in step
with the suite: a new or changed test gets a new or changed line here.

Run the suite from the project root:

```r
testthat::test_dir("tests/testthat")
```

**How to review.** Each claim is either a **domain rule** (a decision about
what the app records, which the author owns and should confirm) or
**plumbing** (a check that the machinery works, which can reasonably be left
to whoever wrote the code). Tick the domain rules you have read and agree
with; query anything that does not match your intent. A test that encodes
the wrong rule will pass forever, so this review is the safeguard.

Review key: `[ ]` not yet reviewed, `[x]` reviewed and agreed, `[?]` queried.

---

## Schema and validation (`test-schema.R`)

The variable declarations and the per-value checks that stand between what
the operator types and what reaches the ledger.

### Domain rules

- [ ] **count rejects anything that is not a whole number >= 0.** Negative,
  fractional and non-numeric entries are refused with a specific message.
  Appropriate because counts of cases or deaths cannot be any of those.
- [ ] **empty input is always rejected as required** (when blank-as-zero is
  off). A blank field is not silently treated as anything.
- [ ] **binary accepts only 0 and 1.**
- [ ] **ordinal accepts declared levels exactly, and is case sensitive.** A
  level typed as "High" when the declaration says "high" is refused, so the
  ledger never holds two spellings of one category.
- [ ] **numeric accepts decimals and rejects non-finite values** (Inf, NaN).
- [ ] **count values are canonicalised for the ledger.** "12.0" is stored as
  "12", so identical values are identical strings.
- [ ] **blank_zero records empty numeric fields as 0 but still requires
  text.** With the option on, an empty count/numeric/binary field becomes 0;
  text and ordinal fields are still required; a typed value is unaffected.
  Appropriate because the NCDC tables leave zero cells empty.
- [ ] **an explicit NA records a missing value for numeric types only.**
  Typing NA into a count field stores a missing value, for a variable the
  source does not report at all. "NA" in a text field is kept as the literal
  text. A rule touching a missing value is skipped, not violated.
- [ ] **check_rules flags exactly the violated comparisons.** A rule such as
  `confirmed <= suspected` is reported when broken and only then.
- [ ] **rules over non-numeric variables are skipped rather than failed.** A
  text variable has no ordering, so a rule naming it cannot fire.

### Plumbing

- **a well-formed schema parses cleanly.** `name, type` lines become a table
  of variables.
- **blank lines and comments are ignored.**
- **malformed declarations are retained and explained, not dropped.** A bad
  line stays in the table with an error message, so the operator sees what
  to fix instead of a variable silently disappearing.
- **an empty schema is an empty frame, not an error.**
- **well-formed rules parse into structural comparisons.** `a <= b` becomes
  left side, operator, right side.
- **rules referencing undeclared variables are rejected.**
- **rule text never reaches the R parser.** `system("echo pwned") <= 1` is
  treated as an unknown variable name, never executed. The app is publicly
  hosted and this text is typed by users, so this is a security property
  with a regression test.

## Workbench, the ledger (`test-workbench.R`)

How rows are built in both entry modes, what provenance they carry, and the
validation gate.

### Domain rules

- [ ] **batch mode writes one row per selected region.** Selecting three
  regions and adding one variable gives three rows, each carrying the
  region's resolved name.
- [ ] **committed rows carry epi week and image provenance.** Every row
  records the epi week from the metadata panel, the name of the reference
  file on screen, a measured number of seconds since that file was loaded
  (asserted to be in range, not merely present), and an ISO 8601 timestamp
  with a timezone offset.
- [ ] **clock pauses are subtracted from active time and recorded.** With
  90 s since load, 30 s of finished pauses and a pause still running for
  10 s, the row records about 50 s active and about 40 s paused. A sidecar
  without pause information counts as never paused.
- [ ] **form mode writes one row per declared variable from a single
  click.** Clicking one region and filling three fields gives three rows for
  that region, marked as form entries.
- [ ] **type violations are blocked and counted.** A non-numeric count
  produces no rows, and the rejection tally in the header increments.
- [ ] **cross-field rule violations are blocked.** Individually valid values
  that break `confirmed <= suspected` are refused; corrected values commit;
  the earlier rejection stays counted.
- [ ] **blank numeric fields commit as 0 when the schema allows it, and are
  refused otherwise.** The same blank form is accepted with zeros when
  blank-as-zero is on and rejected, with a rejection counted, when it is off.
- [ ] **NA typed into a numeric field is committed as missing, distinct from
  blank-as-zero.** In one form, a typed value, a blank and an NA give a
  number, a 0 and a missing value respectively.
- [ ] **re-entering a region for the same file replaces its rows instead of
  appending.** A second click on a state already entered for the current
  file opens the form in editing state; submitting leaves the same number
  of rows with the new values. A different region is a fresh entry.
- [ ] **a period end is recorded when given and NA for a single-date
  report.**
- [ ] **Apply Metadata re-stamps the current document's rows, or the
  selected rows.** Rows committed before the metadata panel was filled in
  can be corrected in bulk: with no selection, every row for the current
  source document takes the panel's project, source, dates and epi week;
  with a selection, only those rows do.

### Plumbing

- **an empty selection commits nothing.**
- **regions absent from the layer resolve to Unknown rather than erroring.**
- **the batch commit button does nothing in form mode.** The two modes cannot
  be triggered by each other's controls.
- **a loaded ledger replaces the session ledger.** Loading a project swaps in
  its rows.

## Region list (`test-region-list.R`)

The tabular alternative to the map.

### Domain rules

- [ ] **the Entered column counts variables recorded against the current
  image only.** A state with two values for the current file shows 2;
  values for other files do not count.
- [ ] **recently-entered ordering follows earlier files and ignores the
  current one.** Rows are ordered by the most recent entry across earlier
  files, never-entered rows last alphabetically, and entries for the
  current file do not move rows. Row indices still map to the right region
  after reordering.

### Plumbing

- **a row click in form mode emits the region's layerId and clears the
  selection**, so the same row can be clicked again.
- **a multi-select in batch mode is the batch and emits no click.**
- **an empty layer renders a placeholder rather than erroring.**

## Sidecar, the reference viewer (`test-sidecar.R`)

### Domain rules

- [ ] **the clock starts on upload and pause/resume accumulate paused
  time.** Before any upload there is no clock; upload starts it; pause and
  resume add the paused interval to the total.
- [ ] **loading a new image resets the clock.** A new file is a new timing
  unit, so pause state from the previous file is discarded.
- [ ] **pausing before any image is loaded is a no-op.**

### Plumbing

- **a PDF upload is served from a session-scoped resource path and hides
  the image controls.** The file is copied under a random per-session
  prefix that is registered with Shiny, so one session cannot reach
  another's upload.

## State manager, saving and loading (`test-state-manager.R`)

### Domain rules

- [ ] **a saved project round-trips through disk with its schema.** Ledger,
  schema text and rules text come back identical, so an extraction resumed
  later commits against the same definitions.
- [ ] **legacy projects, a bare data frame, still load.** Files saved before
  schema persistence, including the published Lassa dataset, open.
- [ ] **a legacy file yields a NULL schema so current declarations
  survive.** Loading an old file fills the ledger but does not blank the
  schema the operator has typed.
- [ ] **project name and blank-as-zero travel with the file and default
  when absent.** A saved project restores its name and its blank-as-zero
  setting; a file from before those fields existed loads with an empty
  name and the option off.

### Plumbing

- **the saved object records format and version.**
- **a project with no schema declared is not treated as legacy.**
- **an empty ledger survives a round trip.**
- **NULL schema arguments become empty strings, never NULL.**
- **unrecognised files are rejected with a clear error.**
- **loading exposes the ledger and schema separately.**
- **a corrupt file surfaces an error instead of crashing the session.**

## Benchmark parser (`test-benchmark-parser.R`)

Analysis code, not part of the app: recovers page-1 figures from an NCDC
report as ground truth for checking an extraction. These tests run only
when `pdftools` is installed and the week 52 2025 report is in `www/`;
otherwise they skip, so a green run without pdftools is not full coverage.

### Domain rules

- [ ] **current-week figures match the printed Table 1.** Every value is
  asserted against the numbers printed in the report, field by field. If
  NCDC's layout changes and the parser starts reading the wrong row, this
  fails rather than quietly invalidating the accuracy assessment.
- [ ] **cumulative and prior-year rows are read in the right order.**
- [ ] **named states are recovered and agree with the tabulated count.** The
  states listed in the Highlights match the count of states in Table 1.
- [ ] **CFR is consistent with the deaths and confirmed counts.**

### Plumbing

- **the week 52 2025 report parses without flags.**
- **a corpus run keeps unreadable reports as flagged rows.** A bad PDF is a
  row with a flag, not a missing row.
- **a missing file is an error, not a silent empty result.**
