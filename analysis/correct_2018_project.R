# analysis/correct_2018_project.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Apply the review corrections to the 2018 extraction project and write a
#   corrected copy alongside a row-level log of every change. The original is
#   never modified. Run from the project root.
#
# WHY A SCRIPT
#   The manuscript reports extraction accuracy, so corrections are evidence and
#   need the same provenance as the rows they change: what was wrong, what it
#   became, and on what reading of the source. Editing rows in the app leaves
#   none of that.
#
# CORRECTIONS (each justified inline below)
#   1. Variable `death` renamed `deaths`, matching every other year and the
#      cross-check scripts.
#   2. `deaths` recorded as NA set to 0 ONLY where the document lists deaths by
#      state: an omitted state in a complete enumeration is a zero. Where the
#      document gives a national deaths figure with no state list (w02, w04,
#      w05), NA is the correct reading and is kept.
#   3. Ondo, page week 3: confirmed NA -> 14. The source reads "Edo (11),
#      Ondo (14), Nasarawa (1) ..."; the 14 had been entered under probable.
#   4. All `suspected` and `probable` rows dropped: 2018 does not stratify
#      either by state after week 2, and the operator decided not to collect
#      them. The week-2 values that were real are listed in the log so they
#      can be restored if ever wanted.
#   5. The page-week-21 document ("no new confirmed case was reported") had
#      been marked by recording Ebonyi = 0. Converted to a document-level nil
#      row, which is what that marker meant.
#   6. Epi_Week set from the week printed on the page (manifest epi_week_pdf),
#      which is authoritative; 2018 file names run one week ahead for all but
#      the last three reports, and a few rows carried a further slip.
# ------------------------------------------------------------------------------

suppressMessages(library(shiny))
source("R/mod_state_manager.R")

IN   <- "C:/Users/dsimo/Downloads/MapLiberator_Project_20260916_0850.rds"
OUT  <- "C:/Users/dsimo/Downloads/MapLiberator_Project_2018_corrected.rds"
LOG  <- "analysis/output/corrections_2018.csv"

state  <- read_project_state(readRDS(IN))
d      <- state$ledger
before <- nrow(d)
log    <- list()
note   <- function(action, rows, detail) {
  if (nrow(rows) == 0) return(invisible(NULL))
  log[[length(log) + 1]] <<- data.frame(
    action = action, Image_File = rows$Image_File, Epi_Week = rows$Epi_Week,
    Region_Name = rows$Region_Name, Variable = rows$Variable,
    Value_before = rows$Value, detail = detail, stringsAsFactors = FALSE)
}

# --- 1. death -> deaths -------------------------------------------------------
d$Variable[d$Variable == "death"] <- "deaths"

# --- 2. deaths NA -> 0, only where the document enumerates deaths by state ----
# Read from the Highlights text of each affected document (see the per-document
# classification in the review); these five list deaths as "State (n)" and the
# listed values sum to the stated total.
stratified <- c("lassa_sitrep_2018_w06_20180204.pdf", "lassa_sitrep_2018_w07_20180211.pdf",
                "lassa_sitrep_2018_w08_20180218.pdf", "lassa_sitrep_2018_w09_20180225.pdf",
                "lassa_sitrep_2018_w10_20180304.pdf")
fix <- d$Variable == "deaths" & is.na(d$Value) & d$Image_File %in% stratified
note("deaths NA -> 0", d[fix, ], "document lists deaths by state; omitted state is zero")
d$Value[fix] <- "0"

kept <- d$Variable == "deaths" & is.na(d$Value)
note("deaths NA kept", d[kept, ], "document gives a national deaths figure only, or no deaths clause")

# --- 3. Ondo, page week 3: confirmed 14 ---------------------------------------
ondo <- d$Image_File == "lassa_sitrep_2018_w04_20180121.pdf" &
        d$Region_Name == "Ondo" & d$Variable == "confirmed"
stopifnot(sum(ondo) == 1, is.na(d$Value[ondo]))
note("confirmed NA -> 14", d[ondo, ], "source: 'Edo (11), Ondo (14), ...'; value had been entered under probable")
d$Value[ondo] <- "14"

# --- 4. drop suspected / probable ----------------------------------------------
drop <- d$Variable %in% c("suspected", "probable")
note("dropped", d[drop & !is.na(d$Value), ], "real value dropped by decision; recoverable from source")
note("dropped", d[drop &  is.na(d$Value), ], "NA placeholder from an earlier schema")
d <- d[!drop, ]

# --- 5. page week 21: Ebonyi marker -> document-level nil row ------------------
w22 <- d$Image_File == "lassa_sitrep_2018_w22_20180527.pdf"
stopifnot(all(d$Region_Name[w22] == "Ebonyi"), "0" %in% d$Value[w22 & d$Variable == "confirmed"])
note("region marker -> nil row", d[w22, ], "source: 'no new confirmed case was reported'")
d$Region_ID[w22]   <- NA_character_
d$Region_Name[w22] <- "Nothing reported"
d$Entry_Mode[w22]  <- "nil"

# --- 6. Epi_Week from the page ---------------------------------------------------
manifest <- read.csv("data/sitreps/manifest.csv", stringsAsFactors = FALSE)
page_wk  <- manifest$epi_week_pdf[match(d$Image_File, manifest$file)]
stopifnot(!any(is.na(page_wk)))
wrong <- d$Epi_Week != page_wk
note("Epi_Week corrected", d[wrong, ], paste0("page reads week ", page_wk[wrong]))
d$Epi_Week <- page_wk

# --- WRITE ----------------------------------------------------------------------
dir.create(dirname(LOG), showWarnings = FALSE, recursive = TRUE)
log_df <- do.call(rbind, log)
write.csv(log_df, LOG, row.names = FALSE)

out <- build_project_state(
  ledger       = d,
  # Schema reflects what the project now actually holds.
  schema_text  = "confirmed, count\ndeaths, count",
  rules_text   = state$rules_text,
  project_name = "2018_extraction",
  blank_zero   = state$blank_zero
)
saveRDS(out, OUT)

# --- REPORT ---------------------------------------------------------------------
cat("rows:", before, "->", nrow(d), "\n")
cat("documents:", length(unique(d$Image_File)), "\n")
cat("variables:", paste(sort(unique(d$Variable)), collapse = ", "), "\n")
cat("\ncorrections by action:\n"); print(table(log_df$action))
cat("\nwritten:", OUT, "\nlog:    ", LOG, "\n")
