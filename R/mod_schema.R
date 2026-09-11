# R/mod_schema.R
# ------------------------------------------------------------------------------
# MODULE: Extraction Schema
#
# Lets the operator declare, once per project, which variables are being
# recovered and what shape each value must take. Two things follow from that
# declaration:
#
#   1. THROUGHPUT. A declared variable set can be entered for one region in a
#      single form, rather than requiring a separate pass over the map per
#      variable. For a state-level extraction of five indicators across ~20
#      affected states and 150+ reports, that is the difference between a
#      feasible task and an infeasible one.
#
#   2. CORRECTNESS. A declared type is a constraint that can be enforced at the
#      point of entry: a count cannot be negative or fractional, an ordinal
#      value must be one of a fixed set of levels. Rejections are counted, so
#      "the tool prevents transcription errors" becomes a measured quantity
#      rather than an assertion.
#
# SCHEMA SYNTAX (one variable per line):
#   name, type
#   name, ordinal, level1|level2|level3
#
# TYPES
#   count    non-negative integer          (case counts, deaths)
#   numeric  any finite number             (rates, CFR)
#   binary   0 or 1                        (presence/absence)
#   ordinal  one of the declared levels    (attack-rate bands)
#   text     free text, no constraint      (escape hatch)
#
# RULE SYNTAX (one per line), for cross-field checks:
#   confirmed <= suspected
#   deaths <= confirmed
#
# SECURITY NOTE
#   Rules are parsed structurally into (lhs, operator, rhs) and evaluated by
#   comparison, never with eval(parse(...)). This application is publicly
#   hosted, so operator-supplied text must never reach the R parser.
# ------------------------------------------------------------------------------

SCHEMA_TYPES <- c("count", "numeric", "binary", "ordinal", "text")
RULE_OPS     <- c("<=", ">=", "==", "!=", "<", ">")  # longest first: matters when matching

# --- PARSING ------------------------------------------------------------------

# parse_schema_text(txt)
#   Returns a data.frame with one row per declared variable:
#     name, type, levels (| separated, "" when not ordinal), error ("" when ok)
#   Malformed lines are RETAINED with an error message rather than dropped, so
#   the operator sees what went wrong instead of a silently shorter schema.
parse_schema_text <- function(txt) {
  empty <- data.frame(name = character(0), type = character(0),
                      levels = character(0), error = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(txt) || !nzchar(trimws(txt))) return(empty)

  lines <- strsplit(txt, "\r?\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) == 0) return(empty)

  rows <- lapply(lines, function(ln) {
    # Keep empty fields: position carries meaning, so ", count" must report a
    # missing NAME rather than silently shifting "count" into the name slot.
    parts <- trimws(strsplit(ln, ",", fixed = TRUE)[[1]])

    nm  <- if (length(parts) >= 1) parts[1] else ""
    ty  <- if (length(parts) >= 2) tolower(parts[2]) else ""
    lv  <- if (length(parts) >= 3) paste(parts[-(1:2)], collapse = ",") else ""

    err <- ""
    if (!nzchar(nm)) {
      err <- "missing variable name"
    } else if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", nm)) {
      err <- "name must start with a letter and contain only letters, digits, underscore"
    } else if (!nzchar(ty)) {
      err <- "missing type"
    } else if (!ty %in% SCHEMA_TYPES) {
      err <- paste0("unknown type '", ty, "' (expected: ", paste(SCHEMA_TYPES, collapse = ", "), ")")
    } else if (ty == "ordinal" && !nzchar(lv)) {
      err <- "ordinal requires levels, e.g. 'band, ordinal, low|mid|high'"
    }

    data.frame(name = nm, type = ty, levels = lv, error = err, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)

  # Duplicate names would silently overwrite each other in the ledger.
  dup <- duplicated(out$name) | duplicated(out$name, fromLast = TRUE)
  out$error[dup & !nzchar(out$error)] <- "duplicate variable name"

  out
}

# parse_rules_text(txt)
#   Returns a data.frame of structural comparisons: lhs, op, rhs, rhs_is_var.
#   Anything that is not exactly "<name> <op> <name|number>" is rejected.
parse_rules_text <- function(txt, schema) {
  empty <- data.frame(lhs = character(0), op = character(0), rhs = character(0),
                      rhs_is_var = logical(0), error = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(txt) || !nzchar(trimws(txt))) return(empty)

  lines <- trimws(strsplit(txt, "\r?\n")[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) == 0) return(empty)

  known <- schema$name[!nzchar(schema$error)]

  rows <- lapply(lines, function(ln) {
    op_found <- NA_character_
    for (op in RULE_OPS) {
      if (grepl(op, ln, fixed = TRUE)) { op_found <- op; break }
    }
    if (is.na(op_found)) {
      return(data.frame(lhs = ln, op = "", rhs = "", rhs_is_var = FALSE,
                        error = "no comparison operator found", stringsAsFactors = FALSE))
    }

    sides <- trimws(strsplit(ln, op_found, fixed = TRUE)[[1]])
    if (length(sides) != 2 || !all(nzchar(sides))) {
      return(data.frame(lhs = ln, op = op_found, rhs = "", rhs_is_var = FALSE,
                        error = "rule must be '<variable> <op> <variable or number>'",
                        stringsAsFactors = FALSE))
    }

    lhs <- sides[1]; rhs <- sides[2]
    rhs_is_var <- grepl("^[A-Za-z][A-Za-z0-9_]*$", rhs)

    err <- ""
    if (!lhs %in% known) {
      err <- paste0("unknown variable '", lhs, "'")
    } else if (rhs_is_var && !rhs %in% known) {
      err <- paste0("unknown variable '", rhs, "'")
    } else if (!rhs_is_var && is.na(suppressWarnings(as.numeric(rhs)))) {
      err <- paste0("right side '", rhs, "' is neither a declared variable nor a number")
    }

    data.frame(lhs = lhs, op = op_found, rhs = rhs, rhs_is_var = rhs_is_var,
               error = err, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

# --- VALIDATION ---------------------------------------------------------------

# validate_value(value, type, levels, blank_zero)
#   Returns list(ok, msg, value) with `value` coerced to its canonical string
#   form so the ledger stays a tidy character column.
#   `blank_zero` records an empty numeric field as 0 instead of refusing it.
#   Sources such as the NCDC Table 3 leave zero cells blank, so without it the
#   operator would type "0" into most cells of every table and each slip
#   would count as a validation rejection.
validate_value <- function(value, type, levels = "", blank_zero = FALSE) {
  raw <- if (is.null(value) || length(value) == 0) "" else trimws(as.character(value)[1])

  numeric_types <- c("count", "numeric", "binary")

  if (!nzchar(raw)) {
    if (isTRUE(blank_zero) && type %in% numeric_types) {
      return(list(ok = TRUE, msg = "", value = "0"))
    }
    return(list(ok = FALSE, msg = "is required", value = NA_character_))
  }

  # An explicit "NA" records a missing value: the source did not report this
  # variable at all (a table with no deaths column), which is different from
  # a blank cell in a column that exists (zero). Numeric types only; a text
  # or ordinal field that was not reported is simply left out of the form.
  if (toupper(raw) == "NA" && type %in% numeric_types) {
    return(list(ok = TRUE, msg = "", value = NA_character_))
  }

  if (type == "text") return(list(ok = TRUE, msg = "", value = raw))

  if (type == "ordinal") {
    lv <- trimws(strsplit(levels, "|", fixed = TRUE)[[1]])
    if (!raw %in% lv) {
      return(list(ok = FALSE, msg = paste0("must be one of: ", paste(lv, collapse = ", ")),
                  value = NA_character_))
    }
    return(list(ok = TRUE, msg = "", value = raw))
  }

  num <- suppressWarnings(as.numeric(raw))
  if (is.na(num) || !is.finite(num)) {
    return(list(ok = FALSE, msg = "must be a number", value = NA_character_))
  }

  if (type == "count") {
    if (num < 0)              return(list(ok = FALSE, msg = "cannot be negative", value = NA_character_))
    if (abs(num - round(num)) > .Machine$double.eps^0.5) {
      return(list(ok = FALSE, msg = "must be a whole number", value = NA_character_))
    }
    return(list(ok = TRUE, msg = "", value = as.character(as.integer(round(num)))))
  }

  if (type == "binary") {
    if (!num %in% c(0, 1)) return(list(ok = FALSE, msg = "must be 0 or 1", value = NA_character_))
    return(list(ok = TRUE, msg = "", value = as.character(as.integer(num))))
  }

  list(ok = TRUE, msg = "", value = as.character(num))  # numeric
}

# check_rules(values, rules, schema)
#   `values` is a named character vector of validated entries. Rules referencing
#   a non-numeric variable are skipped rather than failed - a text field simply
#   has no ordering to compare.
check_rules <- function(values, rules, schema) {
  if (nrow(rules) == 0) return(character(0))

  numeric_types <- c("count", "numeric", "binary")
  is_numeric_var <- function(v) {
    i <- match(v, schema$name)
    !is.na(i) && schema$type[i] %in% numeric_types
  }

  violations <- character(0)
  for (i in seq_len(nrow(rules))) {
    r <- rules[i, ]
    if (nzchar(r$error)) next
    if (!is_numeric_var(r$lhs)) next
    if (r$rhs_is_var && !is_numeric_var(r$rhs)) next

    l <- suppressWarnings(as.numeric(values[[r$lhs]]))
    rv <- if (r$rhs_is_var) suppressWarnings(as.numeric(values[[r$rhs]])) else as.numeric(r$rhs)
    if (is.na(l) || is.na(rv)) next

    ok <- switch(r$op,
                 "<=" = l <= rv, ">=" = l >= rv, "==" = l == rv,
                 "!=" = l != rv, "<"  = l <  rv, ">"  = l >  rv, TRUE)

    if (!ok) violations <- c(violations, paste(r$lhs, r$op, r$rhs))
  }
  violations
}

# --- UI -----------------------------------------------------------------------

schema_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textAreaInput(
      ns("schema_text"),
      hint_label(
        "Variables (one per line: name, type)", title = "Declaring variables",
        tags$p("Each line names one value the form will ask for, followed by its type. ",
               "The type decides which entries are accepted:"),
        tags$ul(
          tags$li(tags$code("count"), ": whole numbers, zero or more"),
          tags$li(tags$code("numeric"), ": any number"),
          tags$li(tags$code("binary"), ": present or absent"),
          tags$li(tags$code("ordinal"), ": one of a fixed list, given after a third comma as ",
                  tags$code("low|medium|high")),
          tags$li(tags$code("text"), ": free text")
        ),
        tags$p(class = "mb-0", "Example: ", tags$code("confirmed, count"))
      ),
      value = "status, binary",
      rows = 5, width = "100%",
      placeholder = "confirmed, count\nband, ordinal, none|low|high"
    ),
    div(class = "text-muted", style = "font-size: 11px; margin-top:-8px; margin-bottom:8px;",
        paste("Types:", paste(SCHEMA_TYPES, collapse = ", "))),

    textAreaInput(
      ns("rules_text"),
      hint_label(
        "Consistency rules (optional)", title = "Cross-field checks",
        tags$p("One comparison per line between two declared variables, or a variable and a number. ",
               "A form entry that breaks a rule is refused and counted in the ledger header."),
        tags$p(class = "mb-0", "Examples: ", tags$code("deaths <= confirmed"), ", ",
               tags$code("confirmed <= 5000"))
      ),
      value = "", rows = 2, width = "100%",
      placeholder = "confirmed <= suspected"
    ),

    checkboxInput(
      ns("blank_zero"),
      hint_label("Blank numeric fields record as 0",
                 "Tick this when the source leaves zero cells empty, as the NCDC ",
                 "state tables do. An empty count, numeric or binary field is then ",
                 "recorded as 0 instead of being refused. Text and ordinal fields ",
                 "are still required. Type ", tags$code("NA"), " into a numeric field ",
                 "when the source does not report that variable at all, so it is ",
                 "recorded as missing rather than zero."),
      value = FALSE
    ),

    uiOutput(ns("schema_status"))
  )
}

# --- SERVER -------------------------------------------------------------------

# schema_server(id, restore = NULL)
#   restore  optional reactive yielding list(schema_text, rules_text), used to
#            repopulate the inputs when a saved project is loaded.
#
# Returns a list of reactives:
#   schema       data.frame of declared variables
#   rules        data.frame of parsed comparisons
#   valid        TRUE when every declared line parses cleanly
#   schema_text  raw declaration text, for persistence
#   rules_text   raw rule text, for persistence
#   blank_zero   TRUE when empty numeric fields should be recorded as 0
schema_server <- function(id, restore = NULL) {
  moduleServer(id, function(input, output, session) {

    schema <- reactive(parse_schema_text(input$schema_text))
    rules  <- reactive(parse_rules_text(input$rules_text, schema()))

    # Restoring a saved project rewrites the declarations, so an extraction
    # resumed weeks later commits against exactly the same definitions.
    if (is.function(restore)) {
      observeEvent(restore(), {
        r <- restore()
        req(!is.null(r))
        updateTextAreaInput(session, "schema_text",
                            value = if (is.null(r$schema_text)) "" else r$schema_text)
        updateTextAreaInput(session, "rules_text",
                            value = if (is.null(r$rules_text)) "" else r$rules_text)
        if (!is.null(r$blank_zero)) updateCheckboxInput(session, "blank_zero", value = isTRUE(r$blank_zero))
        showNotification("Schema restored from project file.", type = "message")
      })
    }

    valid <- reactive({
      s <- schema(); r <- rules()
      nrow(s) > 0 && !any(nzchar(s$error)) && (nrow(r) == 0 || !any(nzchar(r$error)))
    })

    output$schema_status <- renderUI({
      s <- schema(); r <- rules()

      if (nrow(s) == 0) {
        return(div(class = "text-danger", style = "font-size: 12px;",
                   icon("triangle-exclamation"), " No variables declared."))
      }

      problems <- c(
        if (any(nzchar(s$error))) paste0(s$name[nzchar(s$error)], ": ", s$error[nzchar(s$error)]),
        if (nrow(r) > 0 && any(nzchar(r$error))) paste0("rule: ", r$error[nzchar(r$error)])
      )

      if (length(problems) > 0) {
        return(div(class = "text-danger", style = "font-size: 12px;",
                   icon("triangle-exclamation"), " ",
                   tags$ul(style = "padding-left: 18px; margin-bottom: 0;",
                           lapply(problems, tags$li))))
      }

      div(class = "text-success", style = "font-size: 12px;",
          icon("circle-check"), " ",
          paste0(nrow(s), " variable(s): ", paste(s$name, collapse = ", ")),
          if (nrow(r) > 0) paste0(" | ", nrow(r), " rule(s)"))
    })

    list(
      schema      = schema,
      rules       = rules,
      valid       = valid,
      schema_text = reactive(input$schema_text),
      rules_text  = reactive(input$rules_text),
      blank_zero  = reactive(isTRUE(input$blank_zero))
    )
  })
}
