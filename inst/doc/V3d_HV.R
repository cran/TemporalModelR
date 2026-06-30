## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi       = 100,
  out.width = "95%"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("hypervolume")

## -----------------------------------------------------------------------------
library(TemporalModelR)
library(terra)

data(tmr_partition, package = "TemporalModelR")

## -----------------------------------------------------------------------------
hv_out <- build_temporal_hv(
  partition_result   = tmr_partition,
  model_vars         = c("elevation", "forest_cover", "prseas"),
  method             = "gaussian",
  hypervolume_params = list(),
  output_dir         = file.path(tempdir(), "Hypervolume_Models"),
  create_plot        = TRUE,
  verbose            = FALSE
)

## -----------------------------------------------------------------------------
class(hv_out)

names(hv_out)

## -----------------------------------------------------------------------------
hv_out$fold_test_metrics

## ----eval=FALSE---------------------------------------------------------------
# scaled_dir <- system.file("extdata/rasters_scaled", package = "TemporalModelR")
# 
# time_steps <- expand.grid(
#   year             = 1:15,
#   season           = c("Spring", "Summer", "Autumn", "Winter"),
#   stringsAsFactors = FALSE
# )
# 
# preds <- generate_spatiotemporal_predictions(
#   partition_result  = tmr_partition,
#   model_result      = hv_out,
#   raster_dir        = scaled_dir,
#   variable_patterns = c(
#     "elevation"    = "elevation",
#     "forest_cover" = "forest_cover_YEAR",
#     "prseas"       = "prseas_YEAR_SEASON"
#   ),
#   time_cols         = c("year", "season"),
#   time_steps        = time_steps,
#   output_dir        = file.path(tempdir(), "HV_Predictions"),
#   overwrite         = TRUE,
#   verbose           = FALSE
# )

## ----echo=FALSE---------------------------------------------------------------
preds <- readRDS(system.file("extdata/precomputed/hv/preds.rds",
                             package = "TemporalModelR"))
preds$prediction_files <- list.files(
  system.file("extdata/precomputed/hv/pred_tifs", package = "TemporalModelR"),
  pattern = "\\.tif$", full.names = TRUE
)

## ----fig.width=10, fig.height=5-----------------------------------------------
pred_stack <- terra::rast(preds$prediction_files)

pred_names    <- basename(preds$prediction_files)
pred_seasons  <- sub(".*_(Spring|Summer|Autumn|Winter)\\.tif$", "\\1", pred_names)
pred_years    <- as.numeric(sub(".*_(\\d+)_(Spring|Summer|Autumn|Winter)\\.tif$",
                                "\\1", pred_names))
season_levels <- c("Spring", "Summer", "Autumn", "Winter")
stack_order   <- order(pred_years, match(pred_seasons, season_levels))

pred_stack <- pred_stack[[stack_order]]
ordered_years   <- pred_years[stack_order]
ordered_seasons <- pred_seasons[stack_order]
names(pred_stack) <- paste0("Y", ordered_years, "_", ordered_seasons)

block1 <- which(ordered_years %in%  1:4)
block2 <- which(ordered_years %in%  5:8)
block3 <- which(ordered_years %in%  9:12)
block4 <- which(ordered_years %in% 13:15)

## ----fig.width=10, fig.height=5-----------------------------------------------
vote_cols <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")

terra::plot(pred_stack[[block1]], nr = 4, nc = 4,
            mar = c(1.0, 1.0, 1.5, 3.0), legend = FALSE,
            range = c(0, 4), type = "classes",
            levels = 0:4, col = vote_cols)

## ----fig.width=10, fig.height=5-----------------------------------------------
terra::plot(pred_stack[[block2]], nr = 4, nc = 4,
            mar = c(1.0, 1.0, 1.5, 3.0), legend = FALSE,
            range = c(0, 4), type = "classes",
            levels = 0:4, col = vote_cols)

## ----fig.width=10, fig.height=5-----------------------------------------------
terra::plot(pred_stack[[block3]], nr = 4, nc = 4,
            mar = c(1.0, 1.0, 1.5, 3.0), legend = FALSE,
            range = c(0, 4), type = "classes",
            levels = 0:4, col = vote_cols)

## ----fig.width=10, fig.height=4-----------------------------------------------
terra::plot(pred_stack[[block4]], nr = 3, nc = 4,
            mar = c(1.0, 1.0, 1.5, 3.0), legend = FALSE,
            range = c(0, 4), type = "classes",
            levels = 0:4, col = vote_cols)

## -----------------------------------------------------------------------------
head(preds$timestep_metrics)

## -----------------------------------------------------------------------------
preds$overall_summary

## -----------------------------------------------------------------------------
plot_model_assessment(
  predictions         = preds,
  time_column         = c("year", "season"),
  secondary_time_mode = "combine",
  model_result        = hv_out
)

## ----fig.height=20------------------------------------------------------------
plot_model_assessment(
  predictions         = preds,
  time_column         = c("year", "season"),
  secondary_time_mode = "facet",
  model_result        = hv_out,
  cbp_threshold       = 0.001
)

