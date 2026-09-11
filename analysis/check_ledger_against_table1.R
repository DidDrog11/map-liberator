# analysis/check_ledger_against_table1.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Cross-check a hand-extracted ledger against the national totals printed
#   in Table 1 of each NCDC sitrep. For every source document in the ledger,
#   sum the current-week values entered across states and compare with the
#   current-week row the benchmark parser reads from page 1.
#
#   This reads page-1 text only. It never reads the state table, so it is a
#   check applied after extraction, not part of it (decision record 0011).
#
# USAGE (from the project root)
#   source("analysis/check_ledger_against_table1.R")
#   res <- check_ledger_against_table1("path/to/MapLiberator_Project.rds",
#                                      sitrep_dir = "data/sitreps")
#   res$mismatches
# ------------------------------------------------------------------------------

source("analysis/parse_ncdc_benchmark.R")

check_ledger_against_table1 <- function(project_file, sitrep_dir = "data/sitreps",
                                        variables = c("suspected", "confirmed", "probable", "deaths"),
                                        out_csv = NULL) {
  obj    <- readRDS(project_file)
  ledger <- if (is.data.frame(obj)) obj else obj$ledger
  ledger <- ledger[!is.na(ledger$Image_File) & ledger$Variable %in% variables, , drop = FALSE]

  # Ledger side: per document, sum of each variable across states, plus how
  # many states were entered and how many values were NA.
  ledger$num <- suppressWarnings(as.numeric(ledger$Value))
  docs <- split(ledger, ledger$Image_File)
  led <- do.call(rbind, lapply(names(docs), function(f) {
    d <- docs[[f]]
    sums <- vapply(variables, function(v) {
      x <- d$num[d$Variable == v]
      if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
    }, numeric(1))
    # Table 1 counts states with confirmed cases, so compare like with like.
    conf <- d[d$Variable == "confirmed" & !is.na(d$num) & d$num > 0, , drop = FALSE]
    data.frame(file = f, states_entered = length(unique(conf$Region_ID)),
               na_values = sum(is.na(d$num)),
               led_suspected = sums[["suspected"]], led_confirmed = sums[["confirmed"]],
               led_probable = sums[["probable"]], led_deaths = sums[["deaths"]],
               stringsAsFactors = FALSE)
  }))

  # Page-1 side.
  paths <- file.path(sitrep_dir, led$file)
  bench <- parse_ncdc_corpus(paths[file.exists(paths)])
  res <- merge(led, bench[, c("file", "epi_week", "year", "wk_suspected", "wk_confirmed",
                              "wk_probable", "wk_deaths", "wk_states", "parse_flags")],
               by = "file", all.x = TRUE)

  for (v in variables) {
    res[[paste0("diff_", v)]] <- res[[paste0("led_", v)]] - res[[paste0("wk_", v)]]
  }
  res$diff_states <- res$states_entered - res$wk_states
  res <- res[order(res$year, res$epi_week), ]

  diff_cols <- grep("^diff_", names(res), value = TRUE)
  res$any_mismatch <- rowSums(!is.na(res[, diff_cols]) & res[, diff_cols] != 0) > 0

  if (!is.null(out_csv)) {
    dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
    write.csv(res, out_csv, row.names = FALSE)
  }
  list(all = res, mismatches = res[res$any_mismatch | !is.na(res$parse_flags) & nzchar(res$parse_flags), ])
}
