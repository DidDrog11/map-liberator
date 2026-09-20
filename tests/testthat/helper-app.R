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
  # Polygons, not points: the map engine redraws a clicked region with
  # addPolygons, which rejects point geometry. Tests that only read the
  # attribute table are unaffected by the shape.
  square <- function(x, y) {
    sf::st_polygon(list(cbind(c(x, x + 1, x + 1, x, x),
                              c(y, y, y + 1, y + 1, y))))
  }
  sf::st_sf(
    layerId = c("NGA.28_1", "NGA.12_1", "NGA.6_1"),
    NAME_1  = c("Ondo", "Edo", "Bauchi"),
    # add_hierarchy_label() supplies this on real layers; the map engine binds
    # it as the Leaflet label, so the fixture has to carry it too.
    Tooltip = c("Ondo", "Edo", "Bauchi"),
    geometry = sf::st_sfc(square(5, 7), square(6, 6), square(10, 10), crs = 4326),
    stringsAsFactors = FALSE
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
                          geometry = test_geometry(),
                          blank_zero = FALSE,
                          nil_trigger = reactive(0)) {
  if (is.null(rules)) rules <- parse_rules_text("", schema)
  list(
    geom_data    = reactive(geometry),
    metadata     = reactive(metadata),
    add_trigger  = add_trigger,
    entry_mode   = reactive(entry_mode),
    schema       = reactive(schema),
    rules        = reactive(rules),
    schema_valid = reactive(TRUE),
    blank_zero   = reactive(blank_zero),
    nil_trigger  = nil_trigger,
    # Needed by mod_map_engine, which redraws on geom_trigger and deselects on
    # clear_trigger. Static here: tests drive selection through clicks.
    context_data  = reactive(list()),
    geom_trigger  = reactive(0),
    clear_trigger = reactive(0)
  )
}

# A sidecar stand-in whose file name can be changed mid-test, to stand for the
# operator loading the next situation report.
mock_sidecar_switchable <- function(file_name = "week_09.pdf") {
  rv <- reactiveVal(list(file_name = file_name, loaded_at = Sys.time(),
                         paused_at = as.POSIXct(NA), paused_secs = 0))
  list(
    reactive = reactive(rv()),
    load     = function(name) rv(list(file_name = name, loaded_at = Sys.time(),
                                      paused_at = as.POSIXct(NA), paused_secs = 0)),
    pause    = function(secs) {
      cur <- rv()
      # Same file, but the reactive changes - as a real pause does.
      rv(list(file_name = cur$file_name, loaded_at = cur$loaded_at,
              paused_at = as.POSIXct(NA), paused_secs = secs))
    }
  )
}

# `paused_secs` is completed pause time; `paused_for` > 0 means the clock is
# currently paused and has been for that many seconds.
mock_sidecar <- function(file_name = "wk52.png", seconds_ago = 30,
                         paused_secs = 0, paused_for = 0) {
  now <- Sys.time()
  reactive(list(
    file_name   = file_name,
    loaded_at   = now - seconds_ago,
    paused_at   = if (paused_for > 0) now - paused_for else as.POSIXct(NA),
    paused_secs = paused_secs
  ))
}

# Path to the NCDC report bundled for the tutorial; used as a parser fixture.
sitrep_fixture <- function() {
  file.path(APP_ROOT, "www", "An update of Lassa fever outbreak in Nigeria_271225_52.pdf")
}
