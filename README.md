# WATLAS Utilities

## Master branch

**Functions to handle data from the Wadden Sea ATLAS project**

<!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/pratikunterwegs/watlasUtils?branch=master&svg=true)](https://ci.appveyor.com/project/pratikunterwegs/watlasUtils) [![Build Status](https://travis-ci.org/pratikunterwegs/watlasUtils.svg?branch=master)](https://travis-ci.org/pratikunterwegs/watlasUtils) [![codecov.io](https://codecov.io/github/pratikunterwegs/watlasUtils/coverage.svg?branch=master)](https://codecov.io/github/pratikunterwegs/watlasUtils/branch/master)
<!-- badges: end -->

`watlasUtils` is an `R` package written and maintained by [Pratik Gupte](https://www.rug.nl/staff/p.r.gupte), at the [University of Groningen's Theoretical Biology Group](https://www.rug.nl/research/gelifes/tres/), with functions that process high-resolution shorebird tracking data collected by the [Wadden Sea ATLAS project](https://www.nioz.nl/en/about/cos/coastal-movement-ecology/shorebird-tracking/watlas-tracking-regional-movements). WATLAS is part of the [Coastal Movement Ecology (C-MovE)](https://www.nioz.nl/en/about/cos/coastal-movement-ecology) group at the Royal Netherlands Institute for Sea Research's Department of Coastal Systems.

For more information on the system, contact WATLAS PI [Allert Bijleveld (COS-NIOZ)](https://www.nioz.nl/en/about/organisation/staff/allert-bijleveld).

---

## Installation

```r
# This package can be installed using devtools
install.packages("devtools")

# library("devtools")
# installation of the simplified branch
devtools::install_github("pratikunterwegs/watlasUtils", ref = "master")
```

![](https://github.com/pratikunterwegs/watlasUtils/raw/master/screenshot_app.png)

## Functions

The package currently has the following functions:

  - `funcInferResidence` Inference of presumed residence patches from gaps in the tracking data.
  - `funcClassifyPath` Manual classification of empirical and inferred positions into stationary or travelling segments; filtering for residence points.
  - `funcGetResPatch` Non-`sf` based calculation of residence points into contiguous and spatio-temporally independent residence patches. Returns a nested dataframe containing spatial patch outlines and underlying point data.
  - `funcGetPatchData` Retrieves either spatial or point data from the calculated patch object.
  - `funcBwPatchDistance` Calculation of the distance between the first and last points of a series of patches.
  - `funcDistance` Fast Euclidean-distance calculation between a series of points.
  - `funcPatchTraj` Construction of `sf`-based spatial data representing distance between patches.
  - `runTweakApp` Runs a local `shiny` app that visualises the effect of tweaking parameters on suitable data. 
 
---