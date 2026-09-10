# tests/testthat/test-schema.R
# Declaration parsing, value validation, and cross-field rules.

test_that("a well-formed schema parses cleanly", {
  s <- parse_schema_text("suspected, count\nconfirmed, count\nband, ordinal, none|low|high\nnote, text")

  expect_equal(nrow(s), 4)
  expect_equal(s$name, c("suspected", "confirmed", "band", "note"))
  expect_equal(s$type, c("count", "count", "ordinal", "text"))
  expect_equal(s$levels[3], "none|low|high")
  expect_true(all(!nzchar(s$error)))
})

test_that("blank lines and comments are ignored", {
  s <- parse_schema_text("# indicators\n\nconfirmed, count\n\n# end\n")
  expect_equal(nrow(s), 1)
  expect_equal(s$name, "confirmed")
})

test_that("malformed declarations are retained and explained, not dropped", {
  s <- parse_schema_text("good, count\nbad, wobble\n, count\nband, ordinal\ndup, count\ndup, numeric\n9bad, count")

  # Every line survives, so the operator can see what went wrong.
  expect_equal(nrow(s), 7)
  expect_false(nzchar(s$error[1]))
  expect_match(s$error[2], "unknown type")
  expect_match(s$error[3], "missing variable name")   # ", count" - name slot empty
  expect_match(s$error[4], "ordinal requires levels")
  expect_match(s$error[5], "duplicate")
  expect_match(s$error[6], "duplicate")
  expect_match(s$error[7], "must start with a letter")
})

test_that("an empty schema is an empty frame, not an error", {
  expect_equal(nrow(parse_schema_text("")), 0)
  expect_equal(nrow(parse_schema_text(NULL)), 0)
  expect_equal(nrow(parse_schema_text("   \n\n")), 0)
})

# --- VALUE VALIDATION ---------------------------------------------------------

test_that("count rejects anything that is not a whole number >= 0", {
  expect_true(validate_value("12", "count")$ok)
  expect_equal(validate_value("12", "count")$value, "12")

  expect_false(validate_value("-3", "count")$ok)
  expect_match(validate_value("-3", "count")$msg, "negative")

  expect_false(validate_value("1.5", "count")$ok)
  expect_match(validate_value("1.5", "count")$msg, "whole number")

  expect_false(validate_value("twelve", "count")$ok)
  expect_match(validate_value("twelve", "count")$msg, "must be a number")
})

test_that("empty input is always rejected as required", {
  for (ty in c("count", "numeric", "binary", "text")) {
    res <- validate_value("", ty)
    expect_false(res$ok, info = ty)
    expect_match(res$msg, "required", info = ty)
  }
})

test_that("binary accepts only 0 and 1", {
  expect_true(validate_value("0", "binary")$ok)
  expect_true(validate_value("1", "binary")$ok)
  expect_false(validate_value("2", "binary")$ok)
  expect_false(validate_value("yes", "binary")$ok)
})

test_that("ordinal accepts declared levels exactly, and is case sensitive", {
  lv <- "none|low|high"
  expect_true(validate_value("low", "ordinal", lv)$ok)
  expect_false(validate_value("LOW", "ordinal", lv)$ok)
  expect_false(validate_value("medium", "ordinal", lv)$ok)
  expect_match(validate_value("medium", "ordinal", lv)$msg, "must be one of")
})

test_that("numeric accepts decimals and rejects non-finite values", {
  expect_true(validate_value("0.000427", "numeric")$ok)
  expect_true(validate_value("-2.5", "numeric")$ok)   # rates may be negative
  expect_false(validate_value("Inf", "numeric")$ok)
})

test_that("count values are canonicalised for the ledger", {
  expect_equal(validate_value("007", "count")$value, "7")
  expect_equal(validate_value(" 42 ", "count")$value, "42")
})

# --- RULES --------------------------------------------------------------------

test_that("well-formed rules parse into structural comparisons", {
  s <- parse_schema_text("suspected, count\nconfirmed, count\ndeaths, count")
  r <- parse_rules_text("confirmed <= suspected\ndeaths >= 0", s)

  expect_equal(nrow(r), 2)
  expect_equal(r$lhs, c("confirmed", "deaths"))
  expect_equal(r$op,  c("<=", ">="))
  expect_equal(r$rhs_is_var, c(TRUE, FALSE))
  expect_true(all(!nzchar(r$error)))
})

test_that("rules referencing undeclared variables are rejected", {
  s <- parse_schema_text("confirmed, count")
  r <- parse_rules_text("confirmed <= nonexistent", s)
  expect_match(r$error, "unknown variable")
})

test_that("rule text never reaches the R parser", {
  # The app is publicly hosted, so operator-supplied rule text must be treated
  # as data. These must fail as unparseable rules, not execute.
  s <- parse_schema_text("confirmed, count")

  r <- parse_rules_text('system("echo pwned") <= 1', s)
  expect_true(nzchar(r$error))
  expect_match(r$error, "unknown variable")

  r2 <- parse_rules_text("just nonsense", s)
  expect_match(r2$error, "no comparison operator")

  # A rule whose left side is an expression rather than a bare name is refused.
  r3 <- parse_rules_text("confirmed + 1 <= 5", s)
  expect_true(nzchar(r3$error))
})

test_that("check_rules flags exactly the violated comparisons", {
  s <- parse_schema_text("suspected, count\nconfirmed, count\ndeaths, count")
  r <- parse_rules_text("confirmed <= suspected\ndeaths <= confirmed", s)

  clean <- c(suspected = "101", confirmed = "27", deaths = "9")
  expect_length(check_rules(clean, r, s), 0)

  broken <- c(suspected = "20", confirmed = "27", deaths = "30")
  v <- check_rules(broken, r, s)
  expect_length(v, 2)
  expect_true("confirmed <= suspected" %in% v)
  expect_true("deaths <= confirmed" %in% v)

  # Boundary: equality satisfies <=.
  edge <- c(suspected = "27", confirmed = "27", deaths = "27")
  expect_length(check_rules(edge, r, s), 0)
})

test_that("rules over non-numeric variables are skipped rather than failed", {
  s <- parse_schema_text("note, text\nconfirmed, count")
  r <- parse_rules_text("note <= confirmed", s)
  expect_length(check_rules(c(note = "abc", confirmed = "5"), r, s), 0)
})

test_that("blank_zero records empty numeric fields as 0 but still requires text", {
  for (ty in c("count", "numeric", "binary")) {
    res <- validate_value("", ty, blank_zero = TRUE)
    expect_true(res$ok, info = ty)
    expect_equal(res$value, "0", info = ty)
  }
  expect_false(validate_value("", "text", blank_zero = TRUE)$ok)
  expect_false(validate_value("", "ordinal", "low|high", blank_zero = TRUE)$ok)
  # Off by default: a blank count is refused.
  expect_false(validate_value("", "count")$ok)
  # A typed value is unaffected by the flag.
  expect_equal(validate_value("7", "count", blank_zero = TRUE)$value, "7")
})

test_that("an explicit NA records a missing value for numeric types only", {
  for (ty in c("count", "numeric", "binary")) {
    res <- validate_value("NA", ty)
    expect_true(res$ok, info = ty)
    expect_true(is.na(res$value), info = ty)
  }
  expect_true(validate_value("na", "count")$ok)
  expect_equal(validate_value("NA", "text")$value, "NA")   # text is taken literally
  expect_false(validate_value("NA", "ordinal", "low|high")$ok)
  # Rules involving a missing value are skipped, not violated.
  sch <- parse_schema_text("confirmed, count\ndeaths, count")
  rules <- parse_rules_text("deaths <= confirmed", sch)
  expect_length(check_rules(c(confirmed = "18", deaths = NA_character_), rules, sch), 0)
})
