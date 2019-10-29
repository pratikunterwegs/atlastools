# WATLAS Utilities

**Functions to handle data from the Wadden Sea ATLAS project**

<!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/pratikunterwegs/watlasUtils?branch=devbranch&svg=true)](https://ci.appveyor.com/project/pratikunterwegs/watlasUtils) [![Build Status](https://travis-ci.org/pratikunterwegs/watlasUtils.svg?branch=master)](https://travis-ci.org/pratikunterwegs/watlasUtils) [![codecov.io](https://codecov.io/github/pratikunterwegs/watlasUtils/coverage.svg?branch=master)](https://codecov.io/github/pratikunterwegs/watlasUtils/branch/master)
<!-- badges: end -->

`watlasUtils` is an `R` package with functions that process high-resolution shorebird tracking data collected by the [Wadden Sea ATLAS project](https://www.nioz.nl/en/about/cos/coastal-movement-ecology/shorebird-tracking/watlas-tracking-regional-movements). WATLAS is part of the [Coastal Movement Ecology (C-MovE)](https://www.nioz.nl/en/about/cos/coastal-movement-ecology) group at the Royal Netherlands Institute for Sea Research's Department of Coastal Systems. This package is written and maintained by [Pratik Gupte](https://www.rug.nl/staff/p.r.gupte), at the [University of Groningen's Theoretical Biology Group](https://www.rug.nl/research/gelifes/tres/).

For more information on the system, contact WATLAS PI [Allert Bijleveld (COS-NIOZ)](https://www.nioz.nl/en/about/organisation/staff/allert-bijleveld).

## Installation

```r
# This package can be installed using devtools
install.packages("devtools")

# library("devtools")
devtools::install_github("pratikunterwegs/watlasUtils")
```

## Current functions

The package currently has the following main functions:

  - `funcSegPath` manual segmentation of movement data with an option to infer residence patches based on gaps in the data.
  - `funcGetResPatches` construction of `sf`-based residence patches and calculation of patch- and trajectory-specific metrics.

### funcSegPath

An exported function from the namespace, which takes as arguments two dataframes:


  - `revdata` A dataframe with recurse data.
  - `htdata` A dataframe with times since high tide.

Further arguments are:

 - `resTimeLimit`: A cut-off value of residence time, which is used to classify positions as either stationary or travelling.
 - `travelSeg`: The number of positions over which the smoother is applied.
 - `inferPatches`: Whether to infer patches from missing data.
 - `infPatchTimeDiff`: The time difference between two poistions after which the intervening time is considered as a missing residence patch.
 - `infPatchSpatDiff`: The maximum distance between two positions to be considered the ends of an inferred patches.

**Function procedure**

1. `revdata` and `htdata` are converted to `data.table`, and merged. The number of positions in the DT are printed to screen.

2. When `inferPatches = TRUE`, the dataframe is assigned two new columns, `timediff`, which is the time difference between positions, and `spatdiff`, which is the spatial distance between positions. Time difference is calculated as diff<sub>1</sub> = t<sub>1</sub> - t<sub>2</sub>; this is to identify the point _preceding_ the patch.

3. Inferred patches are identified where `timediff` AND `spatdiff` satisfy conditions. The first two points (t<sub>1</sub>, t<sub>2</sub>) of inferred patches are selected and the inferred positions between them are created. t<sub>1</sub> and t<sub>1</sub> are chronologically the first and last points of the inferred patch. The inferred positions have times with an interval of 3 seconds. The x and y coordinates assigned are the mean of t<sub>1</sub> and t<sub>2</sub>. Each coordinate is assigned a residence time equal to the `resTime` specified in the function arguments.

4. A new column `type` is created in both the inferred and real data; this column has values "inferred" or "real".

5. Inferred data are merged with real data, slotting into temporal gaps in the real data.

6. The combined data are scanned for values of residence time below function arguments, and smoothed over the number of positions specified by function arguments. When the smoothed TRUE/FALSE value falls below 0.5, a new residence patch is begun.

7. Redundant columns are removed, a message printed if the resulting dataframe has fewer than 5 rows, and the data are returned.


## Planned functions

It may be a good idea to split off some components of the two main functions to make them more modular, and to add some smaller diagnostic functions.

### Modularity in main functions

- `funcInferPatches` a function to detect temporal gaps in the data and convert these to 'inferred patches' if the data on either side of the gap satisfies some criteria. To be split off from `funcSegPath`, which already has this functionality.
- `funcGetPatchSpatials` a function to construct `sf` `MULTIPOLYGON` objects from patch data, and export to local file.
- `funcVisPatches` a function taking data with id and tidal-cycle information, and visualising (and optionally saving) the output as a map-like image.
