# WATLAS Utilities

**Functions to handle data from the Wadden Sea ATLAS project**

[![Build Status](https://travis-ci.org/pratikunterwegs/watlasUtils.svg?branch=master)](https://travis-ci.org/pratikunterwegs/watlasUtils) [![codecov.io](https://codecov.io/github/pratikunterwegs/watlasUtils/coverage.svg?branch=master)](https://codecov.io/github/pratikunterwegs/watlasUtils/branch/master)

`watlasUtils` is an `R` package with functions that process high-resolution shorebird tracking data collected by the [Wadden Sea ATLAS project](https://www.nioz.nl/en/about/cos/coastal-movement-ecology/shorebird-tracking/watlas-tracking-regional-movements). WATLAS is part of the [Coastal Movement Ecology (C-MovE)](https://www.nioz.nl/en/about/cos/coastal-movement-ecology) group at the [Royal Netherlands Institute for Sea Research's Department of Coastal Systems](https://www.nioz.nl/en/about/cos).

For more information on the system, contact WATLAS PI [Allert Bijleveld](https://www.nioz.nl/en/about/organisation/staff/allert-bijleveld). 


Its most important functions are
  - `funcSegPath` manual segmentation of movement data with an option to infer residence patches based on gaps in the data.
  - `funcGetResPatches` construction of `sf`-based residence patches and calculation of patch- and trajectory-specific metrics.
