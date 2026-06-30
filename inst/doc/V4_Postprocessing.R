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

pred_dir <- system.file("extdata/predictions", package = "TemporalModelR")

list.files(pred_dir, pattern = "\\.tif$")[1:5]

## -----------------------------------------------------------------------------
summary_out <- summarize_raster_outputs(
  predictions_dir = pred_dir,
  output_dir      = file.path(tempdir(), "Binary"),
  consensus       = 2,
  overwrite       = TRUE,
  verbose         = FALSE
)

names(summary_out)

## ----fig.width=10, fig.height=12----------------------------------------------
binary_stack   <- summary_out$consensus_stack
frequency_rast <- summary_out$frequency_raster

names(binary_stack) <- paste0("Y", 1:15, "_Spring")

terra::plot(binary_stack, nr = 5, nc = 3,
            mar = c(1.5, 0.5, 1.5, 0.5), legend = FALSE)

## ----fig.width=8, fig.height=4------------------------------------------------
terra::plot(frequency_rast,
            main = "Proportion of years pixel was suitable",
            mar  = c(2.5, 2.5, 2.5, 5.0))

## ----eval=FALSE---------------------------------------------------------------
# install.packages("fastcpd")

## -----------------------------------------------------------------------------
time_steps <- expand.grid(
  year             = 1:15,
  season           = "Spring",
  stringsAsFactors = FALSE
)

patterns <- analyze_temporal_patterns(
  binary_stack            = binary_stack,
  summary_raster          = frequency_rast,
  time_steps              = time_steps,
  output_dir              = file.path(tempdir(), "Patterns"),
  spatial_autocorrelation = TRUE,
  alpha                   = 0.05,
  estimate_time           = FALSE,
  overwrite               = TRUE,
  verbose                 = FALSE
)

names(patterns)

## -----------------------------------------------------------------------------
study_crs <- sf::st_crs(binary_stack)

zones_sf <- rbind(
  sf::st_sf(ZONE = "West",
            geometry = sf::st_sfc(sf::st_polygon(list(
              matrix(c(0, 0, 1500, 1500, 0,
                       0, 1500, 1500, 0, 0), ncol = 2)
            )), crs = study_crs)),
  sf::st_sf(ZONE = "East",
            geometry = sf::st_sfc(sf::st_polygon(list(
              matrix(c(1500, 1500, 3000, 3000, 1500,
                       0,    1500, 1500, 0,    0),    ncol = 2)
            )), crs = study_crs))
)

## -----------------------------------------------------------------------------
zone_summary <- analyze_trends_by_spatial_unit(
  shapefile_path       = zones_sf,
  name_field           = "ZONE",
  binary_stack         = binary_stack,
  pattern_raster       = patterns$pattern,
  time_decrease_raster = patterns$time_decrease,
  time_increase_raster = patterns$time_increase,
  time_steps           = time_steps,
  output_dir           = file.path(tempdir(), "ZoneSummary"),
  create_plot          = FALSE,
  verbose              = FALSE
)

names(zone_summary)

## -----------------------------------------------------------------------------
zone_summary$overall_summary

## -----------------------------------------------------------------------------
head(zone_summary$timestep_summary)

## -----------------------------------------------------------------------------
head(zone_summary$change_by_timestep)

## -----------------------------------------------------------------------------
zone_plots <- analyze_trends_by_spatial_unit(
  shapefile_path       = zones_sf,
  name_field           = "ZONE",
  binary_stack         = binary_stack,
  pattern_raster       = patterns$pattern,
  time_decrease_raster = patterns$time_decrease,
  time_increase_raster = patterns$time_increase,
  time_steps           = time_steps,
  output_dir           = file.path(tempdir(), "ZoneSummary"),
  create_plot          = TRUE,
  verbose              = FALSE
)

