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

raw_dir <- system.file("extdata/rasters_raw",
                       package = "TemporalModelR")

## ----fig.height=3-------------------------------------------------------------
elev <- rast(file.path(raw_dir, "elevation.tif"))

plot(elev, main = "Elevation (m)")

## ----fig.height=14------------------------------------------------------------
years_to_plot <- seq(1, 15, by = 2)

forest_files  <- file.path(raw_dir,
                           paste0("forest_cover_", years_to_plot, ".tif"))
pr_ann_files  <- file.path(raw_dir,
                           paste0("pr_ann_",      years_to_plot, ".tif"))

### Interleave forest and precip so each row of the plot grid is one year
forest_pr_paths        <- c(rbind(forest_files, pr_ann_files))
forest_pr_stack        <- rast(forest_pr_paths)
names(forest_pr_stack) <- c(rbind(paste("Forest_yr", years_to_plot),
                                  paste("Pr_ann_yr", years_to_plot)))

plot(forest_pr_stack, nc = 2)

## ----fig.height=6-------------------------------------------------------------
season_names <- c("Spring", "Summer", "Autumn", "Winter")

prseas_y1_stack <- rast(file.path(raw_dir,
                                  paste0("prseas_1_",
                                         season_names, ".tif")))

names(prseas_y1_stack) <- season_names

plot(prseas_y1_stack,
     range = c(0, max(values(prseas_y1_stack), na.rm = TRUE)))

## -----------------------------------------------------------------------------
pts_file <- system.file("extdata/points/synthetic_occurrence_points.csv",
                        package = "TemporalModelR")
pts <- utils::read.csv(pts_file)

head(pts)


nrow(pts)


table(pts$year, pts$season)

## ----fig.height=20------------------------------------------------------------
seasons <- c("Spring", "Summer", "Autumn")
study_extent <- ext(0, 3000, 0, 1500)

opar <- par(no.readonly = TRUE)

par(mfrow = c(15, 3),
    mar   = c(1.5, 1.5, 1.5, 0.5),
    oma   = c(2, 2, 2, 1))

for (yr in 1:15) {
  for (sea in seasons) {
    sub <- pts[pts$year == yr & pts$season == sea, ]

    plot(NULL,
         xlim = c(0, 3000), ylim = c(0, 1500),
         asp  = 1, xaxt = "n", yaxt = "n",
         xlab = "", ylab = "",
         main = paste0("Year ", yr, " - ", sea),
         cex.main = 0.9)

    rect(0, 0, 3000, 1500, border = "grey70")

    if (nrow(sub) > 0) {
      points(sub$x, sub$y, pch = 19, cex = 0.7, col = "darkblue")
    }
  }
}

par(opar)

