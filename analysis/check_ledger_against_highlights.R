# analysis/check_ledger_against_highlights.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Score a 2017-2019 extraction state by state against the Highlights text.
#   Those years have no per-state table, but the opening bullet on page 1 is
#   machine-readable and enumerates confirmed cases by state, and often deaths
#   by state:
#
#     "In the reporting Week 09 ... thirty five new confirmed cases were
#      recorded from five States Edo (19), Ondo (5), Bauchi (1), Ebonyi (9),
#      and Plateau (1), with seven new deaths in confirmed cases from three
#      states Ondo (2), Edo (2), and Ebonyi (3)"
#
#   That is a richer benchmark than any later year offers: it checks WHICH
#   states carry WHICH values, not just that the national totals add up. It is
#   used after the fact to check a finished ledger (decision record 0011); it
#   is not an extraction path.
#
# WHAT IT CHECKS, per document
#   confirmed  every state in the sentence is in the ledger with the same
#              value; every ledger state with confirmed > 0 is in the sentence;
#              the sentence's stated total equals the sum of its own list.
#   deaths     as above where the sentence lists deaths by state. Where it
#              gives a national figure only, the ledger should hold NA for
#              every state (the protocol in sitrep_formats.md), and that is
#              what is checked. "with no new death" is a complete enumeration
#              of zeros.
#
# USAGE (from the project root)
#   source("analysis/check_ledger_against_highlights.R")
#   res <- check_ledger_against_highlights("path/to/project.rds", year = 2018)
#   res$summary    one row per document
#   res$mismatch   one row per disagreement, for review
#
# LIMITS
#   Spelled-out numbers up to 999 are understood. State names are matched
#   after normalisation and a short alias list (FCT, common misspellings);
#   anything unmatched is reported, never guessed. A deaths clause naming
#   states without counts is resolved only when the total equals the number
#   of states (one each); otherwise it is reported as unresolvable.
# ------------------------------------------------------------------------------

suppressMessages(library(shiny))
source("R/mod_state_manager.R")

# --- NUMBERS IN WORDS ---------------------------------------------------------

.units <- c(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
            seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11, twelve = 12,
            thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16,
            seventeen = 17, eighteen = 18, nineteen = 19)
.tens  <- c(twenty = 20, thirty = 30, forty = 40, fifty = 50, sixty = 60,
            seventy = 70, eighty = 80, ninety = 90)

# "thirty five", "thirty-five", "one hundred and two", "no" -> integer or NA.
words_to_int <- function(s) {
  s <- tolower(trimws(s))
  if (!nzchar(s)) return(NA_integer_)
  if (grepl("^\\d+$", s)) return(as.integer(s))
  if (s == "no") return(0L)
  toks <- strsplit(gsub("[-,]", " ", s), "\\s+")[[1]]
  toks <- toks[toks != "and" & nzchar(toks)]
  total <- 0L; cur <- 0L
  for (t in toks) {
    if (t %in% names(.units))      cur <- cur + .units[[t]]
    else if (t %in% names(.tens))  cur <- cur + .tens[[t]]
    else if (t == "hundred")       cur <- cur * 100L
    else return(NA_integer_)
  }
  as.integer(total + cur)
}

# --- STATE NAMES ----------------------------------------------------------------

.norm_state <- function(x) {
  x <- tolower(gsub("[^A-Za-z]", "", x))
  aliases <- c(fct = "federalcapitalterritory", abuja = "federalcapitalterritory",
               nassarawa = "nasarawa", nasarrawa = "nasarawa",
               akwaibom = "akwaibom", crossriver = "crossriver")
  ifelse(x %in% names(aliases), aliases[x], x)
}

# --- THE HIGHLIGHTS SENTENCE ------------------------------------------------------

# Footnote markers are glued to words ("confirmedii", "suspectedi"). Strip
# them before anything else looks at the text.
.clean <- function(t) {
  # Bullet glyphs come through as private-use or symbol code points (U+F0B7
  # from the Symbol font, U+2022), which [[:space:]] does not cover and which
  # otherwise sit at the end of a clause and defeat end-of-clause matching.
  t <- gsub("[•●‣]", " ", t)
  t <- gsub("[[:space:]]+", " ", t)
  gsub("(confirmed|suspected|probable|active|death|deaths)[iIvV]+\\b", "\\1", t, perl = TRUE)
}

# All "Name (n)" or "Name(n)" pairs in a clause, in order. The count is the
# leading integer in the parentheses, so "Ebonyi (4 and 2 probable deaths)"
# yields 4. Multi-word names are allowed ("Akwa Ibom (2)"); a leading "and"
# or "from" is stripped.
.state_counts <- function(clause) {
  m <- gregexpr("([A-Z][A-Za-z]+(?: [A-Z][A-Za-z]+)*)\\s*\\((\\d+)", clause, perl = TRUE)
  hits <- regmatches(clause, m)[[1]]
  if (length(hits) == 0) return(setNames(integer(0), character(0)))
  nm <- sub("\\s*\\(.*$", "", hits)
  nm <- sub("^(and|from|in|States?|states?)\\s+", "", nm)
  # "from Six States Edo (3)": a capitalised count and "States" precede the
  # first name and get swept into the match.
  nm <- sub("^[A-Z][a-z]+\\s+States?\\s+", "", nm)
  nm <- sub("\\s+(States?|states?)$", "", nm)
  n  <- as.integer(sub("^.*\\((\\d+).*$", "\\1", hits))
  # "Seventeen (17) new confirmed" is a count restated in digits, not a state.
  keep <- !(tolower(nm) %in% c(names(.units), names(.tens), "hundred"))
  setNames(n[keep], nm[keep])
}

# States named WITHOUT counts, in any of the phrasings the reports use:
#   "from Ondo and Benue states"   "recorded in Taraba state"
#   "in Plateau and Edo state"     "from Edo"  (end of clause, no "state")
# An aside after a dash ("from Gombe - Gombe case was imported from Borno
# state") is dropped before the names are read.
.state_names_only <- function(clause) {
  # Drop parentheticals first: the date range "(12th - 18th August, 2019)"
  # contains a dash, and the aside rule below would otherwise cut the clause
  # there and lose the state list that follows it.
  clause <- gsub("\\([^)]*\\)", "", clause)
  clause <- sub("\\s+[-\u2013]\\s+.*$", "", clause)
  # The list ends at "state(s)", the clause end, punctuation, or the next
  # lowercase word ("in Ebonyi among Health Care workers").
  m <- regmatches(clause, regexpr("(?:from|in)\\s+((?:[A-Z][A-Za-z]+(?: [A-Z][A-Za-z]+)?)(?:\\s*,\\s*|\\s+and\\s+|\\s+)?)+?(?=\\s*[Ss]tates?\\b|\\s*$|\\s*[,.]|\\s+[a-z])", clause, perl = TRUE))
  if (length(m) == 0) return(character(0))
  frag  <- sub("^(?:from|in)\\s+", "", m, perl = TRUE)
  parts <- trimws(strsplit(gsub("\\s+and\\s+", ", ", frag), ",")[[1]])
  parts <- sub("\\s+[Ss]tates?$", "", parts)   # "Bauchi State" -> "Bauchi"
  parts <- parts[nzchar(parts) & !grepl("\\(", parts)]
  # Words that are not states but can be capitalised at a clause start.
  parts[!tolower(parts) %in% c("confirmed", "states", "state", "the")]
}

# parse_highlights(page_text) -> list(week, confirmed_total, confirmed,
#   deaths_total, deaths, deaths_stratified, note)
parse_highlights <- function(page_text) {
  t <- .clean(page_text)
  i <- regexpr("In the reporting [Ww]eek", t)
  out <- list(week = NA_integer_, confirmed_total = NA_integer_, confirmed = integer(0),
              deaths_total = NA_integer_, deaths = integer(0), deaths_stratified = NA,
              note = "")
  if (i < 0) { out$note <- "no Highlights sentence"; return(out) }

  # The sentence runs to the cumulative "From 1st ..." bullet (or 700 chars).
  s <- substr(t, i, nchar(t))
  j <- regexpr("\\bFrom\\s+1st\\b|\\bFrom\\s+January\\b|\\bSo far\\b|\\bCumulatively\\b", s, perl = TRUE)
  s <- if (j > 0) substr(s, 1, j - 1) else substr(s, 1, 700)

  out$week <- as.integer(sub("^.*?[Ww]eek\\s+(\\d{1,2}).*$", "\\1", s, perl = TRUE))

  # Split into the confirmed clause and the deaths clause. The deaths clause is
  # introduced by "with", "and", or (2019) just a comma.
  k <- regexpr("(?:\\b(?:with|and)\\s+|,\\s*(?!(?:with|and)\\b))(no\\s+new\\s+deaths?|\\w+(?:[ -]\\w+)?\\s+(?:new\\s+)?deaths?|\\d+\\s+(?:new\\s+)?deaths?)", s, perl = TRUE)
  conf_clause  <- if (k > 0) substr(s, 1, k - 1) else s
  death_clause <- if (k > 0) substr(s, k, nchar(s)) else ""

  # A bullet may carry a suspected-by-state list before the confirmed one
  # (2018 week 2). The confirmed list follows "new confirmed", so anything
  # before the sentence containing it is not the confirmed clause.
  cc <- regexpr("new confirmed", conf_clause)
  if (cc > 0) {
    lead <- substr(conf_clause, 1, cc - 1)
    dot  <- max(gregexpr("\\.\\s", lead, perl = TRUE)[[1]])
    if (dot > 0) conf_clause <- substr(conf_clause, dot + 1, nchar(conf_clause))
  }

  # Confirmed: the number immediately before "new confirmed".
  m <- regmatches(conf_clause, regexpr("([A-Za-z-]+(?: [A-Za-z-]+){0,3}|\\d+)\\s*(?:\\(\\d+\\))?\\s+new confirmed", conf_clause, perl = TRUE))
  if (length(m)) {
    numtxt <- sub("\\s*(\\(\\d+\\))?\\s+new confirmed$", "", m)
    # Keep only the trailing run of number words: "..., 2018) thirty five" and
    # "Edo state reported one" both end in the number.
    toks <- strsplit(gsub("[-,]", " ", numtxt), "\\s+")[[1]]
    is_numword <- tolower(toks) %in% c(names(.units), names(.tens), "hundred", "and", "no") | grepl("^\\d+$", toks)
    run <- rev(cumprod(rev(is_numword))) == 1
    numtxt <- paste(toks[run], collapse = " ")
    out$confirmed_total <- words_to_int(numtxt)
    if (is.na(out$confirmed_total)) {
      d <- regmatches(m, regexpr("\\((\\d+)\\)", m)); if (length(d)) out$confirmed_total <- as.integer(gsub("\\D", "", d))
    }
  }
  if (grepl("no new confirmed", conf_clause, ignore.case = TRUE)) out$confirmed_total <- 0L

  out$confirmed <- .state_counts(conf_clause)
  # Single-state week with no parenthesised count, in either word order:
  # "... case was reported from Bauchi State" / "Edo state reported one new ...".
  if (length(out$confirmed) == 0 && isTRUE(out$confirmed_total > 0)) {
    nm <- .state_names_only(conf_clause)
    if (length(nm) == 0) {
      m2 <- regmatches(conf_clause, regexpr("([A-Z][a-z]+(?: [A-Z][a-z]+)?)\\s+[Ss]tate\\s+reported", conf_clause, perl = TRUE))
      if (length(m2)) nm <- sub("\\s+[Ss]tate\\s+reported$", "", m2)
    }
    if (length(nm) == 1) out$confirmed <- setNames(out$confirmed_total, nm)
  }

  # Deaths.
  if (nzchar(death_clause)) {
    if (grepl("no (?:new )?deaths?", death_clause, ignore.case = TRUE, perl = TRUE)) {
      out$deaths_total <- 0L
      out$deaths_stratified <- TRUE      # zero for every state: a complete enumeration
    } else {
      m <- regmatches(death_clause, regexpr("(?:(?:with|and)\\s+|,\\s*(?!(?:with|and)\\b))([A-Za-z-]+(?: [A-Za-z-]+){0,2}|\\d+)\\s+(?:new\\s+)?deaths?", death_clause, perl = TRUE))
      if (length(m)) out$deaths_total <- words_to_int(sub("^(?:(?:with|and)\\s+|,\\s*)(.*?)\\s+(?:new\\s+)?deaths?$", "\\1", m, perl = TRUE))
      # "four new deaths, three in confirmed cases from ... and one probable
      # death": only deaths among confirmed cases are recorded (protocol), so
      # the figure qualified by "in confirmed cases" is the total that counts.
      mc <- regmatches(death_clause, regexpr("(\\w+)\\s+in\\s+confirmed\\s+cases", death_clause, perl = TRUE))
      if (length(mc)) {
        n_conf <- words_to_int(sub("\\s+in\\s+confirmed\\s+cases$", "", mc))
        if (!is.na(n_conf)) out$deaths_total <- n_conf
      }
      out$deaths <- .state_counts(death_clause)

      # "N confirmed cases and M probable cases with K deaths were recorded
      # from eight States Edo (11), Ondo (14), ..." - the list follows the
      # deaths figure but belongs to the whole compound subject, and is the
      # confirmed breakdown. Recognised by the confirmed clause having no list
      # of its own while this list sums to the confirmed total, not the deaths
      # total. Deaths are then a national figure only.
      if (length(out$confirmed) == 0 && length(out$deaths) > 0 &&
          isTRUE(sum(out$deaths) == out$confirmed_total) &&
          !isTRUE(sum(out$deaths) == out$deaths_total)) {
        out$confirmed <- out$deaths
        out$deaths    <- setNames(integer(0), character(0))
        out$deaths_stratified <- FALSE
      } else if (length(out$deaths) > 0) {
        out$deaths_stratified <- TRUE
      } else {
        nm <- .state_names_only(death_clause)
        if (length(nm) == 1 && !is.na(out$deaths_total)) {
          out$deaths <- setNames(out$deaths_total, nm)      # "2 new deaths from Ondo state"
          out$deaths_stratified <- TRUE
        } else if (length(nm) > 0 && isTRUE(out$deaths_total == length(nm))) {
          out$deaths <- setNames(rep(1L, length(nm)), nm)   # "two deaths from Ondo and Benue"
          out$deaths_stratified <- TRUE
        } else if (length(nm) > 0) {
          out$deaths_stratified <- NA                        # named but not countable
          out$note <- paste(out$note, "deaths named without counts and total != n states")
        } else {
          out$deaths_stratified <- FALSE                     # national figure only
          # "reported from Edo state with two new deaths": the source does not
          # attribute the deaths, but only one state had cases. The protocol
          # reads this as NA; it is flagged so the operator can decide whether
          # a single reporting state makes the attribution safe.
          if (length(out$confirmed) == 1) out$note <- paste(out$note, "deaths unattributed; single reporting state")
        }
      }
    }
  } else {
    out$deaths_stratified <- FALSE
    out$note <- paste(out$note, "no deaths clause")
  }
  out$note <- trimws(out$note)
  out
}

# --- THE CHECK ----------------------------------------------------------------------

check_ledger_against_highlights <- function(project, year, sitrep_dir = "data/sitreps",
                                            out_dir = "analysis/output") {
  st <- read_project_state(readRDS(project))
  d  <- st$ledger
  d  <- d[grepl(paste0("_", year, "_w"), d$Image_File), ]
  d$num <- suppressWarnings(as.numeric(d$Value))
  stopifnot(nrow(d) > 0)

  summary <- list(); mism <- list()
  add_mism <- function(file, variable, state, ledger, source, kind) {
    mism[[length(mism) + 1]] <<- data.frame(file = file, variable = variable, state = state,
                                            ledger = ledger, source = source, kind = kind,
                                            stringsAsFactors = FALSE)
  }

  for (f in sort(unique(d$Image_File))) {
    # Reports being worked on live in to_extract/ during a session.
    path <- file.path(sitrep_dir, f)
    if (!file.exists(path)) path <- file.path(sitrep_dir, "to_extract", f)
    if (!file.exists(path)) next
    p <- parse_highlights(pdftools::pdf_text(path)[1])
    doc <- d[d$Image_File == f, ]
    nil <- any(is.na(doc$Region_ID))
    regions <- doc[!is.na(doc$Region_ID), ]
    reg_key <- .norm_state(regions$Region_Name)

    row <- data.frame(file = f, week_page = p$week, note = p$note, stringsAsFactors = FALSE)

    for (v in c("confirmed", "deaths")) {
      src   <- if (v == "confirmed") p$confirmed else p$deaths
      total <- if (v == "confirmed") p$confirmed_total else p$deaths_total
      strat <- if (v == "confirmed") TRUE else p$deaths_stratified
      led   <- regions[regions$Variable == v, ]
      led_key <- .norm_state(led$Region_Name)
      src_key <- .norm_state(names(src))

      n_ok <- 0L; n_bad <- 0L

      if (isTRUE(strat)) {
        # Every state in the source must be in the ledger with the same value.
        for (s in seq_along(src)) {
          hit <- which(led_key == src_key[s])
          if (length(hit) == 0) { add_mism(f, v, names(src)[s], NA, src[[s]], "missing from ledger"); n_bad <- n_bad + 1L }
          else if (!isTRUE(led$num[hit[1]] == src[[s]])) { add_mism(f, v, names(src)[s], led$Value[hit[1]], src[[s]], "value differs"); n_bad <- n_bad + 1L }
          else n_ok <- n_ok + 1L
        }
        # Every ledger state with a positive value must be in the source.
        extra <- led[!(led_key %in% src_key) & !is.na(led$num) & led$num > 0, ]
        for (e in seq_len(nrow(extra))) { add_mism(f, v, extra$Region_Name[e], extra$Value[e], NA, "not in source"); n_bad <- n_bad + 1L }
        # Ledger zeros for states the source does not list are fine only when
        # the enumeration is complete (which it is by protocol).
        # Total consistency within the source itself.
        if (!is.na(total) && length(src) > 0 && sum(src) != total) row[[paste0(v, "_source_inconsistent")]] <- TRUE
      } else if (isFALSE(strat)) {
        # National figure only: the ledger should hold NA for every state.
        bad <- led[!is.na(led$num), ]
        for (e in seq_len(nrow(bad))) { add_mism(f, v, bad$Region_Name[e], bad$Value[e], NA, "source not stratified; expected NA"); n_bad <- n_bad + 1L }
        n_ok <- sum(is.na(led$num))
      }

      row[[paste0(v, "_source_total")]] <- total
      row[[paste0(v, "_source_states")]] <- length(src)
      row[[paste0(v, "_ledger_states")]] <- nrow(led)
      row[[paste0(v, "_stratified")]]    <- strat
      row[[paste0(v, "_ok")]]  <- n_ok
      row[[paste0(v, "_bad")]] <- n_bad
    }

    # A nil week: source says no confirmed cases; ledger should carry a nil row.
    if (isTRUE(p$confirmed_total == 0) && !nil) row$note <- paste(row$note, "source nil but no document-level row")
    if (nil && !isTRUE(p$confirmed_total == 0)) row$note <- paste(row$note, "nil row but source reports cases")

    summary[[length(summary) + 1]] <- row
  }

  summary  <- dplyr::bind_rows(summary)
  mismatch <- if (length(mism)) do.call(rbind, mism) else data.frame()

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(summary,  file.path(out_dir, sprintf("check_highlights_%d.csv", year)), row.names = FALSE)
  write.csv(mismatch, file.path(out_dir, sprintf("check_highlights_%d_mismatch.csv", year)), row.names = FALSE)

  cat(sprintf("%d documents checked\n", nrow(summary)))
  for (v in c("confirmed", "deaths")) {
    cat(sprintf("  %-9s %d state-values agree, %d disagree, across %d documents with a discrepancy\n", v,
                sum(summary[[paste0(v, "_ok")]], na.rm = TRUE), sum(summary[[paste0(v, "_bad")]], na.rm = TRUE),
                sum(summary[[paste0(v, "_bad")]] > 0, na.rm = TRUE)))
  }
  cat(sprintf("  parser notes on %d documents\n", sum(nzchar(summary$note))))
  invisible(list(summary = summary, mismatch = mismatch))
}
