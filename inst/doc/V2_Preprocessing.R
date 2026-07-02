## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi       = 100,
  out.width = "95%"
)

## -----------------------------------------------------------------------------
library(TemporalModelR)
library(terra)
library(sf)

raw_dir  <- system.file("extdata/rasters_raw", package = "TemporalModelR")

pts_file <- system.file("extdata/points/synthetic_occurrence_points.csv",
                        package = "TemporalModelR")

ref_file <- file.path(raw_dir, "elevation.tif")
study_crs <- sf::st_crs(terra::rast(ref_file))

## ----eval=FALSE---------------------------------------------------------------
# aligned_dir <- file.path(tempdir(), "rasters_aligned")
# 
# raster_align(
#   input_dir        = raw_dir,
#   output_dir       = aligned_dir,
#   reference_raster = ref_file,
#   resample_method  = "bilinear",
#   overwrite        = TRUE
# )

## -----------------------------------------------------------------------------
aligned_dir <- system.file("extdata/rasters_aligned", package = "TemporalModelR")

list.files(aligned_dir, pattern = "\\.tif$")[1:6]

## -----------------------------------------------------------------------------
rare_dir <- file.path(tempdir(), "rarefied")

rare_out <- spatiotemporal_rarefaction(
  points_sp        = pts_file,
  output_dir       = rare_dir,
  reference_raster = ref_file,
  time_cols        = c("year", "season"),
  xcol             = "x",
  ycol             = "y",
  points_crs       = study_crs,
  output_prefix    = "Pts_seasonal",
  verbose          = FALSE
)

rare_out$input_points

rare_out$spatial_points

rare_out$spatiotemporal_points

## -----------------------------------------------------------------------------
rare_out$files_created

## -----------------------------------------------------------------------------
rare_dir <- file.path(tempdir(), "rarefied")

rare_out_annual <- spatiotemporal_rarefaction(
  points_sp        = pts_file,
  output_dir       = rare_dir,
  reference_raster = ref_file,
  time_cols        = "year",
  xcol             = "x",
  ycol             = "y",
  points_crs       = study_crs,
  output_prefix    = "Pts_ann",
  verbose          = FALSE
)

rare_out_annual$input_points

rare_out_annual$spatial_points

rare_out_annual$spatiotemporal_points

## -----------------------------------------------------------------------------
ext_dir <- file.path(tempdir(), "extracted")

ext_out <- temporally_explicit_extraction(
  points_sp           = rare_out$files_created$spatiotemporal,
  raster_dir          = aligned_dir,
  variable_patterns   = c(
    "elevation"    = "elevation",
    "forest_cover" = "forest_cover_YEAR",
    "prseas"       = "prseas_YEAR_SEASON"
  ),
  time_cols           = c("year", "season"),
  xcol                = "X",
  ycol                = "Y",
  points_crs          = study_crs,
  output_dir          = ext_dir,
  output_prefix       = "extracted_seasonal",
  save_raw            = TRUE,
  save_scaled         = TRUE,
  save_scaling_params = TRUE,
  verbose             = FALSE
)

head(ext_out$raw_values)

## -----------------------------------------------------------------------------
ext_out$files_created

## -----------------------------------------------------------------------------
ext_out$scaling_params

## ----eval=FALSE---------------------------------------------------------------
# scaled_dir <- file.path(tempdir(), "rasters_scaled")
# 
# scale_rasters(
#   input_dir           = aligned_dir,
#   output_dir          = scaled_dir,
#   scaling_params_file = ext_out$files_created$scaling_params,
#   variable_patterns   = c(
#     "elevation"    = "elevation",
#     "forest_cover" = "forest_cover_YEAR",
#     "prseas"       = "prseas_YEAR_SEASON"
#   ),
#   time_cols           = c("year", "season"),
#   overwrite           = TRUE,
#   verbose             = FALSE
# )

## -----------------------------------------------------------------------------
scaled_dir <- system.file("extdata/rasters_scaled", package = "TemporalModelR")

list.files(scaled_dir, pattern = "\\.tif$")[1:6]

## -----------------------------------------------------------------------------
ext_scaled_file <- system.file(
  "extdata/points/extracted_seasonal_Scaled_Values.csv",
  package = "TemporalModelR"
)

study_area_sf <- sf::st_as_sf(sf::st_as_sfc(
  sf::st_bbox(c(xmin = 0, xmax = 3000, ymin = 0, ymax = 1500),
              crs = study_crs)
))

partition <- spatiotemporal_partition(
  reference_shapefile_path = study_area_sf,
  points_file_path         = ext_scaled_file,
  xcol                     = "x",
  ycol                     = "y",
  points_crs               = study_crs,
  time_cols                = "year",
  n_spatial_folds          = 2,
  n_temporal_folds         = 2,
  max_attempts = 10,
  max_imbalance = 0.15,
  create_plot              = TRUE,
  verbose                  = FALSE
)

partition$summary

## -----------------------------------------------------------------------------
absences <- generate_absences(
  partition_result         = partition,
  reference_shapefile_path = study_area_sf,
  raster_dir               = scaled_dir,
  variable_patterns        = c(
    "elevation"    = "elevation",
    "forest_cover" = "forest_cover_YEAR",
    "prseas"       = "prseas_YEAR_SEASON"
  ),
  method                   = "buffer",
  buffer_distance          = 300,
  ratio                    = 2,
  time_cols                = c("year", "season"),
  create_plot              = TRUE,
  plot_by_fold             = TRUE,
  verbose                  = FALSE
)

absences$summary

## -----------------------------------------------------------------------------
user_pts_file <- system.file(
  "extdata/points/synthetic_user_presences.csv",
  package = "TemporalModelR"
)

user_pts <- utils::read.csv(user_pts_file)

head(user_pts)

nrow(user_pts)

## -----------------------------------------------------------------------------
user_rare_dir <- file.path(tempdir(), "rarefied_user")

user_rare_out <- spatiotemporal_rarefaction(
  points_sp        = user_pts_file,
  output_dir       = user_rare_dir,
  reference_raster = ref_file,
  time_cols        = c("year", "season"),
  xcol             = "x",
  ycol             = "y",
  points_crs       = study_crs,
  output_prefix    = "Pts_user",
  verbose          = FALSE
)

user_rare_out$input_points

user_rare_out$spatiotemporal_points

## -----------------------------------------------------------------------------
absences_user <- generate_absences(
  partition_result         = partition,
  reference_shapefile_path = study_area_sf,
  raster_dir               = scaled_dir,
  variable_patterns        = c(
    "elevation"    = "elevation",
    "forest_cover" = "forest_cover_YEAR",
    "prseas"       = "prseas_YEAR_SEASON"
  ),
  method                   = "user_data",
  user_absence_data        = user_rare_out$files_created$spatiotemporal,
  xcol                     = "X",
  ycol                     = "Y",
  points_crs               = study_crs,
  time_cols                = c("year", "season"),
  create_plot              = TRUE,
  plot_by_fold             = TRUE,
  verbose                  = FALSE
)

absences_user$summary

