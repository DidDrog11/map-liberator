# analysis/merge_projects.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Combine several Map Liberator project files into one, so an extraction
#   carried out on more than one machine (or in more than one sitting under a
#   different project name) becomes a single ledger that the app can load and
#   continue from.
#
# WHY A SCRIPT, NOT AN APP FEATURE
#   Merging is curation: variable names must agree, the same document must not
#   be present twice, and the resulting schema has to be chosen. Those checks
#   want to fail loudly with a report, which is a script's natural shape. The
#   app stays a tool for reading documents (decision record 0011).
#
# USAGE (from the project root)
#   source("analysis/merge_projects.R")
#   merge_projects(
#     paths  = c("path/to/pc_project.rds", "path/to/laptop_project.rds"),
#     out    = "path/to/merged.rds",
#     rename = c(death = "deaths")       # optional: harmonise variable names
#     schema_text = "confirmed, count\n..."   # optional: replace the schema outright
#   )
#
# RULES
#   - Rows are combined as they are; nothing is recomputed.
#   - A (document, region, variable) key present in more than one input is an
#     error, listed in full, because there is no right answer to "which one".
#     Resolve it in the source project and re-run.
#   - The schema, rules, project name and blank-as-zero setting are taken from
#     the first path unless `schema_from` says otherwise. Every variable in the
#     merged ledger must be declared in that schema, or be listed in `extra`
#     (variables that were recorded but are not being collected going forward).
# ------------------------------------------------------------------------------

suppressMessages(library(shiny))
source("R/mod_state_manager.R")
source("R/mod_schema.R")   # parse_schema_text(), to check the merged variables

merge_projects <- function(paths, out, rename = character(0), schema_from = 1L,
                           extra = character(0), project_name = NULL,
                           schema_text = NULL) {
  stopifnot(length(paths) >= 2, all(file.exists(paths)))

  states  <- lapply(paths, function(p) read_project_state(readRDS(p)))
  ledgers <- lapply(seq_along(states), function(i) {
    d <- states[[i]]$ledger
    d$source_project <- basename(paths[i])
    d
  })

  # 1. Harmonise variable names before any comparison.
  for (i in seq_along(ledgers)) {
    v <- ledgers[[i]]$Variable
    hit <- v %in% names(rename)
    if (any(hit)) {
      message(sprintf("%s: renamed %d rows (%s)", basename(paths[i]), sum(hit),
                      paste(sprintf("%s -> %s", names(rename), rename), collapse = ", ")))
      ledgers[[i]]$Variable[hit] <- unname(rename[v[hit]])
    }
  }

  merged <- dplyr::bind_rows(ledgers)

  # 2. The same observation must not appear twice. Document-level (nil) rows
  #    have NA region, so the key uses a placeholder for them.
  key <- paste(merged$Image_File,
               ifelse(is.na(merged$Region_ID), "<document>", merged$Region_ID),
               merged$Variable, sep = " | ")
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(dup)) {
    conflicts <- unique(merged[dup, c("Image_File", "Region_Name", "Variable", "Value", "source_project")])
    conflicts <- conflicts[order(conflicts$Image_File, conflicts$Region_Name, conflicts$Variable), ]
    print(conflicts, row.names = FALSE)
    stop(sprintf("%d observation(s) present in more than one project (listed above). Resolve and re-run.",
                 length(unique(key[dup]))))
  }

  # 3. Overlapping documents that did NOT conflict are still worth knowing
  #    about: the same report extracted twice for different variables.
  docs_by_proj <- lapply(ledgers, function(d) unique(d$Image_File))
  overlap <- Reduce(intersect, docs_by_proj)
  if (length(overlap)) {
    message(sprintf("note: %d document(s) appear in more than one project without conflicting rows: %s",
                    length(overlap), paste(head(overlap, 5), collapse = ", "),
                    if (length(overlap) > 5) " ..." else ""))
  }

  # 4. Schema: every variable in the merged ledger must be accounted for.
  base <- states[[schema_from]]
  if (!is.null(schema_text)) base$schema_text <- schema_text
  declared <- parse_schema_text(base$schema_text)$name
  undeclared <- setdiff(unique(merged$Variable), c(declared, extra))
  if (length(undeclared)) {
    stop(sprintf("variables in the merged ledger not declared in the schema from %s: %s\n  Declare them, or pass them in `extra` if they are not being collected going forward.",
                 basename(paths[schema_from]), paste(undeclared, collapse = ", ")))
  }

  merged$source_project <- NULL
  result <- build_project_state(
    ledger       = merged,
    schema_text  = base$schema_text,
    rules_text   = base$rules_text,
    project_name = if (is.null(project_name)) base$project_name else project_name,
    blank_zero   = base$blank_zero
  )
  saveRDS(result, out)

  # 5. Report.
  yr <- function(f) substr(regmatches(f, regexpr("[0-9]{4}_w", f)), 1, 4)
  cat("\nmerged:", nrow(merged), "rows from", length(paths), "projects ->", out, "\n")
  cat("documents:", length(unique(merged$Image_File)), "\n")
  cat("by year:\n"); print(table(yr(unique(merged$Image_File))))
  cat("variables:", paste(sort(unique(merged$Variable)), collapse = ", "), "\n")
  cat("schema from:", basename(paths[schema_from]), "\n")
  invisible(result)
}
