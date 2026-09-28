# Grizzly Bear Denning Resource Selection Function

This repository contains the data and R code used to fit the grizzly bear denning resource selection function (RSF), generate covariate response plots, and produce the spatial relative selection strength (RSS) map associated with the Ecosphere manuscript.

## Repository Contents

### Analysis

`Denning_RSF_Models_logRSS_Plots_RSS_Map.R`

R script used to:

- fit the final denning RSF;
- generate log-relative selection strength (log-RSS) response curves and confidence intervals;
- predict relative selection strength across the study area; and
- generate the spatial RSS map.

### Data

The `data/` directory contains the analysis-ready data and spatial layers required by the R script.

`denning_rsf_data.csv`

Analysis-ready used and available data for fitting the denning RSF. Sensitive identifiers and spatial coordinates have been removed. The file contains the response variable and environmental covariates required to reproduce the final model.

`covariate_means.rds`

Means used to standardize continuous covariates.

`covariate_sds.rds`

Standard deviations used to standardize continuous covariates.

`denning_raster_stack.tiff`

30-m environmental raster stack used to generate spatial predictions from the fitted RSF. The raster contains four layers:

- distance to road;
- elevation;
- slope; and
- aspect.

The raster is stored using Git Large File Storage (Git LFS).

`study_area.gpkg`

Study-area boundary used to crop and mask spatial predictions.

`recovery_zones.gpkg`

Grizzly bear recovery-zone boundaries used in the final map.

## Software

The analysis was conducted in R. Required packages are loaded at the beginning of `Denning_RSF_Models_logRSS_Plots_RSS_Map.R`.

The primary packages used include:

- `tidyverse`
- `sf`
- `terra`
- `cowplot`
- `ggspatial`
- `scales`

## Reproducing the Analysis

Clone or download the repository while retaining the existing directory structure:

    grizzly-bear-denning-rsf/
    |
    |-- Denning_RSF_Models_logRSS_Plots_RSS_Map.R
    |
    `-- data/
        |-- denning_rsf_data.csv
        |-- covariate_means.rds
        |-- covariate_sds.rds
        |-- denning_raster_stack.tiff
        |-- recovery_zones.gpkg
        `-- study_area.gpkg

Run `Denning_RSF_Models_logRSS_Plots_RSS_Map.R` from the repository root directory.

Because `denning_raster_stack.tiff` is stored using Git LFS, users cloning the repository should have Git LFS installed so that the complete raster is downloaded rather than only its LFS pointer.

## Sensitive Location Data

Raw grizzly bear den coordinates, animal identifiers, and other information that could reveal den locations are not included in this repository because of the sensitivity of grizzly bear location data.

The shared `denning_rsf_data.csv` contains the covariate values necessary to reproduce the final RSF without providing den coordinates.

Spatial k-fold cross-validation required the original den locations and therefore cannot be reproduced using the publicly archived data. Those sensitive coordinates are intentionally withheld.

## Coordinate Reference System

Spatial data and the environmental prediction raster are provided in NAD83 / Conus Albers (EPSG:5070). The final mapping workflow reprojects the predicted RSS surface to UTM Zone 11N (EPSG:32611) for visualization.

## Citation

Please cite the associated Ecosphere article when using these data or code.

Full citation will be added following publication.
