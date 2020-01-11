# WATLAS Utilities

**Functions to handle data from the Wadden Sea ATLAS project**

<!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/pratikunterwegs/watlasUtils?branch=master&svg=true)](https://ci.appveyor.com/project/pratikunterwegs/watlasUtils) 
  [![Build Status](https://travis-ci.org/pratikunterwegs/watlasUtils.svg?branch=master)](https://travis-ci.org/pratikunterwegs/watlasUtils) 
  [![codecov.io](https://codecov.io/github/pratikunterwegs/watlasUtils/coverage.svg?branch=master)](https://codecov.io/github/pratikunterwegs/watlasUtils/branch/master)
  [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
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

![](https://github.com/pratikunterwegs/watlasUtils/raw/master/inst/screenshot_app.png)

## Shiny application

Package functionality is demonstrated by the inbuilt `shiny` application.

```r
# run the app from the package
library(watlasUtils)
wat_run_patch_vis_app()
```

## Wiki

Read the [wiki](https://github.com/pratikunterwegs/watlasUtils/wiki) for a description of the functions' intended usage.
