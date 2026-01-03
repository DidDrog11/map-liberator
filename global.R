# global.R
# ------------------------------------------------------------------------------
# PURPOSE: Global configuration, Libraries, and Robust Data Loading
# ------------------------------------------------------------------------------

library(shiny)
library(leaflet)
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
LOCAL_CACHE_DIR <- "C:/Users/ucbtds4/R_Repositories/arenavirus_hantavirus/data/gadm"

# Fallback
if (!dir.exists(LOCAL_CACHE_DIR)) {
  LOCAL_CACHE_DIR <- file.path("data", "cache")
  if (!dir.exists(LOCAL_CACHE_DIR)) dir.create(LOCAL_CACHE_DIR, recursive = TRUE)
}

# 2. HELPER: Robust GADM Loader
load_gadm_locally <- function(country_iso, level) {
  
  # 1. Try to find the EXACT level file first (Fastest)
  # Pattern: gadm41_NGA_1_pk.rds
  f_name_exact <- paste0("gadm41_", country_iso, "_", level, "_pk.rds")
  f_path_exact <- file.path(LOCAL_CACHE_DIR, f_name_exact)
  
  if (file.exists(f_path_exact)) {
    # message(paste("Loading exact level:", f_path_exact))
    spat_vec <- readRDS(f_path_exact)
    return(st_as_sf(spat_vec) |> st_transform(4326))
  }
  
  # 2. If exact missing, try Level 3 and aggregate (Slower fallback)
  f_name_l3 <- paste0("gadm41_", country_iso, "_3_pk.rds")
  f_path_l3 <- file.path(LOCAL_CACHE_DIR, f_name_l3)
  
  if (file.exists(f_path_l3)) {
    # message(paste("Loading L3 fallback for:", level))
    spat_vec <- readRDS(f_path_l3)
    sf_obj <- st_as_sf(spat_vec) |> st_transform(4326)
    
    if (level == 3) return(sf_obj)
    
    # Aggregation Logic
    if (level == 2) {
      cols <- intersect(names(sf_obj), c("GID_0", "NAME_0", "COUNTRY", "GID_1", "NAME_1", "GID_2", "NAME_2"))
      if(length(cols)>0) return(sf_obj |> group_by(across(all_of(cols))) |> summarise(geometry = st_union(geometry), .groups="drop"))
    }
    if (level == 1) {
      cols <- intersect(names(sf_obj), c("GID_0", "NAME_0", "COUNTRY", "GID_1", "NAME_1"))
      if(length(cols)>0) return(sf_obj |> group_by(across(all_of(cols))) |> summarise(geometry = st_union(geometry), .groups="drop"))
    }
    if (level == 0) {
      cols <- intersect(names(sf_obj), c("GID_0", "NAME_0", "COUNTRY"))
      if(length(cols)>0) return(sf_obj |> group_by(across(all_of(cols))) |> summarise(geometry = st_union(geometry), .groups="drop"))
    }
    return(sf_obj)
  }
  
  stop(paste("No GADM files found for", country_iso))
}

# 3. Static Data
available_files <- list.files(LOCAL_CACHE_DIR, pattern = "gadm41_.*_3_pk\\.rds")
available_isos <- unique(substr(available_files, 8, 10))

# Create the Named List for Selectize (This enables the typing search)
country_df <- countrycode::codelist |> 
  filter(iso3c %in% available_isos) |> 
  select(name = country.name.en, code = iso3c) |> 
  arrange(name)

# Format as a list: list("Benin" = "BEN", "Nigeria" = "NGA")
country_vec <- setNames(as.list(country_df$code), country_df$name)

# 4. Helper: Create Rich Tooltip
add_hierarchy_label <- function(sf_obj) {
  # Dynamically build a label string based on available columns
  # We use HTML for formatting
  
  sf_obj |> 
    rowwise() |> 
    mutate(
      Tooltip = paste0(
        "<strong>Country:</strong> ", ifelse("NAME_0" %in% names(sf_obj), NAME_0, ifelse("COUNTRY" %in% names(sf_obj), COUNTRY, "N/A")), "<br>",
        ifelse("NAME_1" %in% names(sf_obj), paste0("<strong>ADM1:</strong> ", NAME_1, "<br>"), ""),
        ifelse("NAME_2" %in% names(sf_obj), paste0("<strong>ADM2:</strong> ", NAME_2, "<br>"), ""),
        ifelse("NAME_3" %in% names(sf_obj), paste0("<strong>ADM3:</strong> ", NAME_3), "")
      ) |> lapply(htmltools::HTML)
    ) |> 
    ungroup()
}