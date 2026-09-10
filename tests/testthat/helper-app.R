# tests/testthat/helper-app.R
# ------------------------------------------------------------------------------
# Loads the application into the test session.
#
# Map Liberator is a Shiny app, not an R package, so there is no namespace to
# attach. testthat runs with the working directory set to this folder; the
# project root is therefore two levels up. Helper files are sourced before any
# test file, so everything below is available to all of them.
# ------------------------------------------------------------------------------

APP_ROOT <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

suppressMessages(suppressWarnings(source(file.path(APP_ROOT, "global.R"))))

for (mod in list.files(file.path(APP_ROOT, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(mod)
}

# Analysis code lives outside the app and is not sourced by app.R, so load it
# explicitly for the benchmark-parser tests.
source(file.path(APP_ROOT, "analysis", "parse_ncdc_benchmark.R"))

# --- SHARED FIXTURES ----------------------------------------------------------

# A minimal stand-in for a loaded GADM layer. Only `layerId` and a NAME_* column
# are needed: everything downstream reads the attribute table, not the geometry.
test_geometry <- function() {
  sf::st_as_sf(
    data.frame(
      layerId = c("NGA.28_1", "NGA.12_1", "NGA.6_1"),
      NAME_1  = c("Ondo", "Edo", "Bauchi"),
      x = c(5, 6, 10), y = c(7, 6, 10),
      stringsAsFactors = FALSE
    ),
    coords = c("x", "y"), crs = 4326
  )
}

test_metadata <- function(...) {
  modifyList(
    list(project = "test_project", source = "sitrep_52_2025",
         year = "2025", month = "12", day = "28", week = 52,
         var_name = "cases_detected", var_value = "1"),
    list(...)
  )
}

# Assembles the reactive contract mod_workbench expects from mod_controls.
mock_controls <- function(entry_mode = "single",
                          add_trigger = reactive(0),
                          metadata = test_metadata(),
                          schema = parse_schema_text("status, binary"),
                          rules = NULL,
                          geometry = test_geometry()) {
  if (is.null(rules)) rules <- parse_rules_text("", schema)
  list(
    geom_data    = reactive(geometry),
    metadata     = reactive(metadata),
    add_trigger  = add_trigger,
    entry_mode   = reactive(entry_mode),
    schema       = reactive(schema),
    rules        = reactive(rules),
    schema_valid = reactive(TRUE)
  )
}

mock_sidecar <- function(file_name = "wk52.png", seconds_ago = 30) {
  reactive(list(file_name = file_name, loaded_at = Sys.time() - seconds_ago))
}

# Path to the NCDC report bundled for the tutorial; used as a parser fixture.
sitrep_fixture <- function() {
  file.path(APP_ROOT, "www", "An update of Lassa fever outbreak in Nigeria_271225_52.pdf")
}
