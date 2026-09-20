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

## 2017 to 2019: per-state figures from the Highlights text

The weekly per-state figures are in the "In the reporting week" bullet on
page 1: confirmed cases by state, then deaths by state, and in 2017 suspected
cases by state. The list is a complete enumeration, so a state named for
confirmed cases but absent from the deaths list had zero deaths that week.
National figures with no state breakdown (suspected in 2018 and 2019, and
deaths in sentences such as "with 14 deaths were recorded from eight States
Edo (11), ...") are entered as `NA` for every state.

- **Deaths are deaths among confirmed cases**, as in the later tables. A
  death described as a probable case is not recorded.
- **Late reports.** Sentences such as "Taraba state reported seven additional
  confirmed cases with 5 deaths that were not previously reported" are not
  that week's figures. They are recorded under separate variables,
  `confirmed_late` and `deaths_late`, against the report that carried them,
  and the week's own confirmed and deaths for that state are 0. What to do
  with late reports is then an analysis decision made once.
- **File names run one week ahead of the page in 2017 and 2018.** Enter the
  week printed on the page.
- Five 2017 reports (weeks 7, 8, 32, 40, 44) carry a small text table of new
  confirmed cases by state; it is the same information as the sentence.

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

## Completing the state-by-week grid (analysis, not extraction)

Only regions the source reports are entered (decision record 0012), so the
ledger is completed to a full grid afterwards. The rule runs per document
and per variable, and depends on what the ledger says about that variable
in that document:

| entered values for the variable | unentered states | explicit NA states |
|---|---|---|
| all NA | NA: the report gave no state breakdown | NA |
| none NA | 0: the report enumerated the variable by state | (none) |
| a mix | 0: the enumeration exists | stay NA: the operator saw no usable value for that state |

Above the rule: a week with no document in the ledger is NA for everything,
and the late-report variables (`confirmed_late`, `deaths_late`) follow the
same logic as any other enumerated variable. The mixed case should be rare;
list its occurrences and check why they arose before trusting them.

**Rows with no region.** A document that was reviewed and reports nothing for
any region is recorded with `Region_ID` NA and `Entry_Mode` `nil`. Read such a
row as a statement about the document, not about a state: a value of 0 means
the variable was enumerated and no region reached it, so the grid fills 0 for
every state; NA means the document gives no state breakdown for that variable,
so the grid stays NA. This is what separates a week that was read and found
empty from one that was never extracted, which the ledger otherwise cannot
distinguish. Sum these rows into national totals as normal — they carry the
national figure — but exclude them when counting how many states reported.

## OPEN DECISION: unattributed deaths in 2017-2019 (settle at reconciliation)

Not yet decided. It affects six rows in 2018 and nothing later, because the
2020-onward tables attribute deaths per state explicitly. Decide it when going
through the reconciliation, then record the answer here and apply it through
`analysis/correct_2018_project.R` so the change is logged.

**The question.** Some weeks give a deaths figure with no state attached:

> "In the reporting Week 33 ... three new confirmed cases were reported from
> Edo state **with two new deaths**"

Only Edo reported cases, so reading the deaths as Edo's is natural, and that is
how they were entered. The protocol as written says NA, on the grounds that the
source did not attribute them.

**Evidence for treating it as an inference rather than an observation.** Deaths
in these reports are deaths *in confirmed cases* - confirmed at any time, not
necessarily that week - so they need not fall in a state reporting new cases.
Across 2018-2019 there are 73 weeks where deaths ARE attributed by state, and in
three of them a death falls in a state with no confirmed cases that week:

| week | death in a state with zero cases |
|---|---|
| 2018 w12 (page) | Ebonyi |
| 2018 w14 (page) | FCT |
| 2019 w05 (page) | Enugu |

Note the limit of that evidence: of those 73 weeks, **none** is a single-state
week, so there is no direct evidence about the exact shape in question.

**Options.**

1. Keep them against the single reporting state. Simple, probably right most of
   the time, but records an inference as though the source stated it.
2. NA for every state. Follows the current protocol, but discards the national
   figure entirely - the report says two deaths happened and the ledger ends up
   with no record of them.
3. Record the national figure as a document-level row (`Region_ID` NA, the same
   mechanism as a nil return) and no state-level deaths row for that week. Keeps
   the number, attributes nothing, and does not double-count: the reconciliation
   sums one row per variable per document either way. Attribution then becomes
   an analysis step, as with the grid-completion rule above.

**The six rows, which are three different cases.**

- 2018 page weeks 33, 35, 42 - single state, deaths unattributed.
- 2018 page week 46 - "Edo (1) and Ondo (2) with one new death", recorded as
  Ondo. Two candidates, one death; a coin flip either way.
- 2018 page weeks 22, 26 - no deaths clause at all, recorded as 0. A separate
  question, and 0 looks right: these reports state deaths whenever they occur,
  so silence reads as none.

If option 3 is chosen, `check_ledger_against_highlights.R` should also verify a
document-level figure against the sentence's stated total; it currently ignores
region-less rows and would pass them silently.

## Charts drawn over Table 3

2022 week 2 draws the age/sex pyramid and the cases-by-state bar chart on top of
Table 3. The table is a raster underneath them, so zooming or cropping cannot
recover it. `analysis/extract_pdf_image.R` pulls the table's own image out:

```r
source("analysis/extract_pdf_image.R")
pdf_page_images(path, page = 4)            # list the images, largest first
extract_pdf_image(path, page = 4, out = "data/sitreps/png/<name>_table3.png")
```

The extracted PNG is convenience output in the sense of decision record 0007:
load it in the sidecar to read the numbers, but the ledger's `Image_File` should
still name the report, not the derivative.

Checked across all 47 usable 2022 reports by looking for a figure caption
sharing the Table 3 page: **week 2 is the only one**. The same check is worth
re-running per year, since the object-level scan cannot see inside the compressed
object streams that the PDF 1.7 reports use.

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
