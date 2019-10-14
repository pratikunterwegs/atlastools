# WATLAS Utilities

**Utilities to handle data from the WATLAS project**

[![Build Status](https://travis-ci.org/pratikunterwegs/watlasUtils.svg?branch=master)](https://travis-ci.org/pratikunterwegs/watlasUtils) [![codecov.io](https://codecov.io/github/pratikunterwegs/watlasUtils/coverage.svg?branch=master)](https://codecov.io/github/pratikunterwegs/watlasUtils/branch/master)

`watlasUtils` is an `R` package with functions that process high-resolution shorebird tracking data collected by the [WATLAS project](https://www.nioz.nl/en/about/cos/coastal-movement-ecology/shorebird-tracking/watlas-tracking-regional-movements).

Its most important functions are
  - `funcSegPath` manual segmentation of movement data with an option to infer residence patches based on gaps in the data.
  - `funcGetResPatches` construction of `sf`-based residence patches and calculation of patch- and trajectory-specific metrics.
