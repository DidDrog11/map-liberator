# global.R - CORRECTED VERSION
# ------------------------------------------------------------------------------
# PURPOSE: Global configuration, Libraries, and Robust Data Loading
# ------------------------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(leaflet)
library(bslib)
library(sf)
library(terra)
library(geodata)
library(dplyr)
library(shinyjs)
library(countrycode)
library(shinycssloaders)
library(DT)
library(base64enc)

# 1. DEFINE CACHE PATH
LOCAL_CACHE_DIR <- file.path(getwd(), "data", "gadm")

# 2. HELPER: Robust GADM Loader WITH VALIDATION
load_gadm_locally <- function(country_iso, level) {
  
  # 1. Try to find the EXACT level file first (Fastest)
  f_name_exact <- paste0("gadm41_", country_iso, "_", level, "_pk.rds")
  f_path_exact <- file.path(LOCAL_CACHE_DIR, f_name_exact)
  
  if (file.exists(f_path_exact)) {
    spat_vec <- readRDS(f_path_exact)
    sf_obj <- st_as_sf(spat_vec) |> st_transform(4326)
    return(validate_and_repair(sf_obj, level))  # <-- VALIDATE BEFORE RETURN
  }
  
  # 2. If exact missing, try Level 3 and aggregate (Slower fallback)
  f_name_l3 <- paste0("gadm41_", country_iso, "_3_pk.rds")
  f_path_l3 <- file.path(LOCAL_CACHE_DIR, f_name_l3)
  
  if (file.exists(f_path_l3)) {
    spat_vec <- readRDS(f_path_l3)
    sf_obj <- st_as_sf(spat_vec) |> st_transform(4326)
    
    if (level == 3) {
      return(validate_and_repair(sf_obj, level))  # <-- VALIDATE
    }
    
    # Aggregation Logic
    if (level == 2) {
      cols <- intersect(names(sf_obj), c("GID_0", "NAME_0", "COUNTRY", "GID_1", "NAME_1", "GID_2", "NAME_2"))
      if(length(cols) > 0) {
        sf_obj <- sf_obj |> 
          group_by(across(all_of(cols))) |> 
          summarise(geometry = st_union(geometry), .groups = "drop")
      }
    }
    else if (level == 1) {
      cols <- intersect(names(sf_obj), c("GID_0", "NAME_0", "COUNTRY", "GID_1", "NAME_1"))
      if(length(cols) > 0) {
        sf_obj <- sf_obj |> 
          group_by(across(all_of(cols))) |> 
          summarise(geometry = st_union(geometry), .groups = "drop")
      }
    }
    else if (level == 0) {
      cols <- intersect(names(sf_obj), c("GID_0", "NAME_0", "COUNTRY"))
      if(length(cols) > 0) {
        sf_obj <- sf_obj |> 
          group_by(across(all_of(cols))) |> 
          summarise(geometry = st_union(geometry), .groups = "drop")
      }
    }
    
    return(validate_and_repair(sf_obj, level))  # <-- VALIDATE
  }
  
  # 3. If we get here, no files found
  stop(paste("No GADM files found for", country_iso, "at level", level))
}

# HELPER: Validation and Repair
validate_and_repair <- function(sf_obj, level) {
  
  # Check for required columns
  required_cols <- paste0(c("GID_", "NAME_"), 0:level)
  missing_cols <- setdiff(required_cols, names(sf_obj))
  
  if(length(missing_cols) > 0) {
    warning(paste(
      "Missing expected columns:", 
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Check for invalid geometries
  invalid <- which(!st_is_valid(sf_obj))
  if(length(invalid) > 0) {
    warning(paste(length(invalid), "invalid geometries detected. Attempting repair..."))
    sf_obj <- st_make_valid(sf_obj)
  }
  
  return(sf_obj)
}

# 3. Static Data
# Handle different column names in countrycode package versions
c_name_col <- "country.name.en"
if (!"country.name.en" %in% names(countrycode::codelist)) {
  c_name_col <- "country.name"
}

# Create the full list
country_df <- countrycode::codelist |> 
  dplyr::filter(!is.na(iso3c), !is.na(.data[[c_name_col]])) |> 
  dplyr::select(name = all_of(c_name_col), code = iso3c) |> 
  dplyr::arrange(name)

country_vec <- setNames(as.list(country_df$code), country_df$name)

# 4. Helper: Create Rich Tooltip
add_hierarchy_label <- function(sf_obj) {
  # Pre-allocate character vector
  n <- nrow(sf_obj)
  tooltips <- vector("character", n)
  
  # Build tooltip components vectorized
  # Country (always present)
  country <- if("NAME_0" %in% names(sf_obj)) {
    sf_obj$NAME_0
  } else if("COUNTRY" %in% names(sf_obj)) {
    sf_obj$COUNTRY
  } else {
    rep("N/A", n)
  }
  
  # Start with country
  tooltips <- paste0("<strong>Country:</strong> ", country, "<br>")
  
  # Add Admin 1 if present
  if("NAME_1" %in% names(sf_obj)) {
    tooltips <- paste0(tooltips, "<strong>ADM1:</strong> ", sf_obj$NAME_1, "<br>")
  }
  
  # Add Admin 2 if present
  if("NAME_2" %in% names(sf_obj)) {
    tooltips <- paste0(tooltips, "<strong>ADM2:</strong> ", sf_obj$NAME_2, "<br>")
  }
  
  # Add Admin 3 if present
  if("NAME_3" %in% names(sf_obj)) {
    tooltips <- paste0(tooltips, "<strong>ADM3:</strong> ", sf_obj$NAME_3)
  }
  
  # Convert to HTML and add to sf object
  sf_obj$Tooltip <- lapply(tooltips, htmltools::HTML)
  
  return(sf_obj)
}