# setup_data.R (Adaptive Version)
# ------------------------------------------------------------------------------
# PURPOSE: Batch download and cache GADM data with ADAPTIVE simplification.
# ------------------------------------------------------------------------------

library(geodata)
library(sf)
library(dplyr)
library(countrycode)
library(rmapshaper)

# 1. CONFIGURATION
DATA_DIR <- file.path("data", "gadm")
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

TARGET_COUNTRIES <- "ALL" # Can be set as "ALL" to download all, otherwise a list of target countries e.g. c("GBR", "NGA")

# 2. HELPER FUNCTIONS
process_country <- function(iso) {
  
  final_file <- file.path(DATA_DIR, paste0("gadm41_", iso, "_0_pk.rds"))
  if (file.exists(final_file)) {
    message(paste("Skipping", iso, "- already exists."))
    return(NULL)
  }
  
  message(paste("\nProcessing:", iso, "--------------------------"))
  
  raw_path <- tempdir()
  spat_vec <- NULL
  level_found <- 0
  
  # --- DOWNLOAD LOGIC ---
  # Try Level 3 -> 2 -> 1
  try({
    tmp <- gadm(country = iso, level = 3, path = raw_path, version = "latest")
    if (!is.null(tmp)) { spat_vec <- tmp; level_found <- 3 }
  }, silent = TRUE)
  
  if (is.null(spat_vec)) {
    try({
      tmp <- gadm(country = iso, level = 2, path = raw_path, version = "latest")
      if (!is.null(tmp)) { spat_vec <- tmp; level_found <- 2 }
    }, silent = TRUE)
  }
  
  if (is.null(spat_vec)) {
    try({
      tmp <- gadm(country = iso, level = 1, path = raw_path, version = "latest")
      if (!is.null(tmp)) { spat_vec <- tmp; level_found <- 1 }
    }, silent = TRUE)
  }
  
  if (is.null(spat_vec)) {
    message(paste("  ERROR: Could not download any data for", iso))
    return(NULL)
  }
  
  message(paste("  Success! Found Level", level_found))
  
  # Convert to SF
  sf_obj <- tryCatch({
    st_as_sf(spat_vec) |> st_transform(4326)
  }, error = function(e) return(NULL))
  
  if (is.null(sf_obj)) return(NULL)
  
  # 3. GENERATE AND SAVE (Aggregate Upwards)
  if (level_found >= 3) save_layer(sf_obj, iso, 3)
  
  if (level_found >= 2) {
    l2 <- if(level_found > 2) aggregate_layer(sf_obj, 2) else sf_obj
    save_layer(l2, iso, 2)
  }
  
  if (level_found >= 1) {
    l1 <- if(level_found > 1) aggregate_layer(sf_obj, 1) else sf_obj
    save_layer(l1, iso, 1)
  }
  
  l0 <- aggregate_layer(sf_obj, 0)
  save_layer(l0, iso, 0)
  
  message(paste("  Done:", iso))
  
  rm(spat_vec, sf_obj, l0); if(exists("l1")) rm(l1); if(exists("l2")) rm(l2)
  gc() 
}

aggregate_layer <- function(sf_obj, target_level) {
  cols <- character(0)
  if (target_level == 0) cols <- c("GID_0", "NAME_0")
  if (target_level == 1) cols <- c("GID_0", "NAME_0", "GID_1", "NAME_1")
  if (target_level == 2) cols <- c("GID_0", "NAME_0", "GID_1", "NAME_1", "GID_2", "NAME_2")
  
  cols <- intersect(names(sf_obj), cols)
  if (length(cols) == 0) return(sf_obj) 
  
  sf_obj |> 
    group_by(across(all_of(cols))) |> 
    summarise(geometry = st_union(geometry), .groups = "drop")
}

save_layer <- function(sf_obj, iso, level) {
  
  sf_final <- NULL
  sf_pre   <- NULL
  
  # 1. ANALYZE COMPLEXITY
  # Count vertices safely
  total_pts <- tryCatch({
    sum(sapply(st_geometry(sf_obj), function(x) length(unlist(x))/2))
  }, error = function(e) 0)
  
  # --- TIERED STRATEGY DEFINITIONS ---
  # Tier 1: Small (< 500k)   -> No Planar, Keep 50%
  # Tier 2: Heavy (500k-2M)  -> Planar 25m, Keep 50%
  # Tier 3: Huge (2M-10M)    -> Planar 50m, Keep 40%
  # Tier 4: Nuclear (>10M)   -> Planar 150m, Keep 10%
  # Previous settings largest subunit was 3.5Mb
  if (total_pts > 10000000) {
    tier <- "Nuclear (>10M)"
    planar_tol <- 100
    keep_pct <- 0.3
  } else if (total_pts > 2000000) {
    tier <- "Huge (2M-10M)"
    planar_tol <- 50
    keep_pct <- 0.4
  } else if (total_pts > 500000) {
    tier <- "Heavy (500k-2M)"
    planar_tol <- 25
    keep_pct <- 0.50
  } else {
    tier <- "Standard (<500k)"
    planar_tol <- 0 # None
    keep_pct <- 0.50
  }
  
  message(paste0("    [", tier, " | ", format(total_pts, big.mark=","), " pts]"))
  
  # 2. PASS 1: PRE-SIMPLIFICATION
  if (planar_tol > 0) {
    message(paste0("    Running Planar Pre-Simplify (Tol: ", planar_tol, "m)..."))
    
    tryCatch({
      sf_planar <- st_transform(sf_obj, 3857)
      sf_pre_planar <- st_simplify(sf_planar, preserveTopology = FALSE, dTolerance = planar_tol)
      sf_pre <- st_transform(sf_pre_planar, 4326)
      
      new_pts <- sum(sapply(st_geometry(sf_pre), function(x) length(unlist(x))/2))
      message(paste0("    Reduction: ", format(total_pts, big.mark=","), " -> ", format(new_pts, big.mark=",")))
      rm(sf_planar, sf_pre_planar)
    }, error = function(e) {
      sf_pre <- sf_obj # Fallback
    })
    
  } else {
    # Just a tiny clean for standard files
    sf_pre <- st_simplify(sf_obj, preserveTopology = TRUE, dTolerance = 0.001)
  }
  
  # 3. PASS 2: RMAPSHAPER
  if (!is.null(sf_pre)) {
    message(paste0("    Running rmapshaper (Keep: ", keep_pct*100, "%)..."))
    sf_final <- tryCatch({
      rmapshaper::ms_simplify(sf_pre, keep = keep_pct, keep_shapes = TRUE)
    }, error = function(e) {
      message(paste("    rmapshaper failed. Falling back."))
      return(sf_pre) 
    })
  } else {
    sf_final <- sf_obj
  }
  
  # 4. FINAL CLEANUP
  if (is.null(sf_final)) sf_final <- sf_pre
  
  if (any(st_is_empty(sf_final))) sf_final <- sf_final[!st_is_empty(sf_final), ]
  if (any(!st_is_valid(sf_final))) sf_final <- st_make_valid(sf_final)
  
  f_name <- file.path(DATA_DIR, paste0("gadm41_", iso, "_", level, "_pk.rds"))
  saveRDS(sf_final, f_name)
  
  size_mb <- file.size(f_name) / 1024 / 1024
  message(paste("    Saved Level", level, "to", f_name, "(Disk Size:", round(size_mb, 2), "MB)"))
  
  rm(sf_final, sf_pre)
  gc()
}

# 3. EXECUTION
if (length(TARGET_COUNTRIES) == 1 && TARGET_COUNTRIES == "ALL") {
  TARGET_COUNTRIES <- countrycode::codelist$iso3c[!is.na(countrycode::codelist$iso3c)]
}

for (iso in TARGET_COUNTRIES) {
  tryCatch({
    process_country(iso)
  }, error = function(e) {
    message(paste("CRITICAL FAILURE on", iso, ":", e$message))
  })
}

message("\nSetup Complete!")