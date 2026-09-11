# NCDC Lassa fever situation reports: what can be extracted, and from where

Working notes for the state-week extraction exercise. Based on the 482 reports
listed on the NCDC site as of 10 September 2026, fetched and catalogued by
`analysis/download_ncdc_sitreps.R`. The manifest at `data/sitreps/manifest.csv`
records, per report, the page carrying the per-state table and whether the
text layer was machine-readable.

## Layout eras

The per-state table carrying suspected, confirmed, probable, HCW and deaths is
present from early 2020. Earlier reports summarise the situation differently.

| Era | Reports | Per-state figures available | Where |
|---|---|---|---|
| 2017 to 2019 | 135 | New confirmed cases by state are given in the Highlights text. Cumulative confirmed by state is shown as a map with count bands and, from 2018, a CFR bar chart with counts per state. Suspected and deaths by state are not tabulated. | page 1 text; Figure 1 map; Figure 5 chart |
| 2020 weeks 1 to 6 | 6 | A confirmed-only table by state, whose columns were being developed week to week. | page 3 or 4 |
| 2020 week 7 onward | 338 | "Table 3. Weekly and Cumulative number of suspected and confirmed cases." Twelve columns: for the current week and cumulatively, Suspected, Confirmed, Trend, Probable, HCW, Deaths (confirmed cases). Empty cells denote zero. | page 4 in the 6 to 8 page layout, occasionally page 5 |
| 2021 to mid 2022 | about 60 of the above | Same table. The PDF text layer in these files does not extract cleanly, so the caption cannot be located by software; the manifest marks the page as "assumed: page 4". | page 4 |

Manifest notes, recorded rather than corrected by renaming:

- One report is listed with a 2007 date; page 1 gives 2017. The 2017 layout
  has no "Epi Week" line, so the week stays as listed (13).
- One listing row links to the same file as 2020 week 3
  (`duplicate_content_of` in the manifest). Extract it once.
- Four reports carry a different epi week on page 1 from the listing
  (`week_mismatch`): listed 2021 week 34 is week 33, listed 2022 week 23 is
  week 18, listed 2022 week 53 is 2023 week 1, and listed 2024 week 16 is
  week 17. Use the week printed on the page and enter it in the app's Epi
  Week field, since the file name follows the listing.
- Three links return HTTP 404 (2021 week 24, 2022 week 21, 2023 week 38) and
  six files download but cannot be opened (2021 weeks 21, 23, 30, 32; 2022
  weeks 19, 20). Those nine weeks are gaps in this archive.

## Variation within the stable era, and where pausing the clock is reasonable

- **Row order.** 2020 lists states alphabetically with a fixed 37 rows. From
  about 2022 the rows are sorted by cumulative confirmed cases, so a state's
  position changes week to week.
- **Rows without an Admin 1 polygon.** The 2025 week 52 table has 38 rows
  including "Jos". Such rows need an operator decision, either to skip or to
  attribute to a state, and the decision belongs in the project notes.
- **Only regions with something reported are entered.** A state whose
  whole current-week row is empty is not entered; the analysis fills zero
  for states the table covered (decision record 0012). A state with at
  least one non-empty cell is entered in full, and its empty cells are
  zeros, handled by the app's "Blank numeric fields record as 0" option.
- **A missing column is not zero.** The 2020 week 1 to 6 tables list
  confirmed cases only, while Table 1 on page 1 gives national suspected,
  probable and death totals (week 1: 18 confirmed, 2 deaths). Where a
  variable is absent from the table, type `NA` into its field so it is
  recorded as missing rather than 0.
- **Exception, applied to 2020 week 1.** Page 1 attributes both deaths to
  named states (one each in Abia and Ondo). Because the national total is
  fully attributed, deaths for every other state are recorded as 0, an
  inference from the total rather than a reading of the table, and the two
  attributed deaths are entered from page 1. Apply the same reasoning only
  when a total is fully attributed on page 1; otherwise the variable stays
  `NA`. Suspected and probable are not attributed by state in these weeks
  and stay `NA`.

The reference loaded into the sidecar is the PDF itself. The browser's PDF
viewer gives page navigation, zoom and search, so Table 3 and page 1 are
both reachable from one file, one clock and one provenance stamp. This is
the low-tech path any user has; nothing is converted first. The rendered
pages under `data/sitreps/png/` (Table 3 page, and a composite with page 1
beneath it) are optional convenience output and are not part of the
protocol.
- **Trend arrows and HCW** are present but are not part of the target schema.
- **Cumulative columns** repeat across weeks. Recording only the current-week
  block is the cheaper exercise. Cumulative figures are a useful cross-check
  at year end (week 52 cumulative should equal the sum of weekly values).

## Recommended app setup for the 2020 week 7 onward era

Geography: Nigeria, Admin 1. Entry mode: form per region. Schema:

```
suspected, count
confirmed, count
probable, count
deaths, count
```

Rules: `confirmed <= suspected`. A rule `deaths <= confirmed` would refuse
legitimate rows, since a death can be reported in a week after the case was
confirmed. Tick "Blank numeric fields record as 0".

Load the report PDF (`data/sitreps/lassa_sitrep_<year>_w<week>_<date>.pdf`)
into the sidecar. The file name carries the year and epi week, and is
stamped on every ledger row, so the ledger can be joined back to the report
without relying on the metadata panel being updated each time.

## Timing protocol

The clock starts when the page image is uploaded. Pause it (sidebar button)
when extraction stops: orienting to a new layout, looking something up, or a
break. Each ledger row records active seconds since the image was loaded and
the paused total, so per-report time is the active time on the last row
committed for that image. The blocked-commit count in the workbench header
is the validation evidence.

Recording the first report of each era separately is worth it: orientation
cost is real but is not extraction cost.

## What cannot be done from these reports

- Weekly suspected, probable or deaths by state before 2020 week 7.
- LGA-level counts in any era. The LGA map is an attack-rate choropleth in
  bands, which is the binary or ordinal extraction the app was first built
  for, not counts.
- Anything for the nine reports that are missing or unreadable.
