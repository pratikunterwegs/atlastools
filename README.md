# WATLAS Utilities

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

![](https://github.com/pratikunterwegs/watlasUtils/raw/master/inst/screenshot_app.png)

## Main functions

### Shiny application

Package functionality is demonstrated by the inbuilt `shiny` application.

```r
# run the app from the package
library(watlasUtils)
runTweakApp()
```

The app runs locally, and accepts data of the kind that can be found in the `tests/testthat/testdata` folder of the cloned repository. NB: This folder is not available when installing using `devtools` as shown above.

The application visualises the effect of tweaking the important parameters used in the main functions. The app takes two input files (for historical reasons; this is liable to change):

1. Revisit data: A dataframe of recurse analysis, or must include, in addition to X, Y and time columns, a residence time column named resTime, id, and tidalcycle.
2. Tidal data: A dataframe output of tidal cycle finding analysis, or must include, in addition to X, Y and time columns, a tidaltime column named tidaltime; also id, and tidalcycle for merging.

These two files are merged together during operation. Operation begins on clicking the `Run` button.

### Residence point inference

WATLAS sometimes has difficulty tracking birds on the mudflats, but not in flight, leading to missing data between flight trajectories. The `funcInferResidence` fills in gaps which satisfy certain parameters with aritifical (inferred) localisations:

1. The duration of the missing data is greater than the threshold `infPatchTimeDiff`.
2. The distance between the start and end points of the missing-data phase is less than `infPatchSpatDiff`.

The inferred localisations all have an X and Y coordinate value that is the mean of the start and end points of the missing data phase. The time assigned to each point is from a linear sequence with an interval of 3s, starting from the last location before the missing data phase. The residence time assigned to them is the minimum required to be considered a residence point (see next function). All inferred points are clearly marked 'inferred' in the resulting data frame.

### Path classification

Individual trajectories are classified by `funcClassifyPath` into residence or travelling points depending on the residence time `resTimeLimit` calculated using the _recurse_ package [(Bracis et al. 2018)](https://onlinelibrary.wiley.com/doi/abs/10.1111/ecog.03618) -- ideally, this is based in the biology of the tracked animal. The travelling points are removed, and the residence points returned in a data frame.

### Residence patch construction

The individual trajectory retaining only the presumed residence points is clustered into groups based on spatial and temporal proximity using `funcGetResPatch`. Summary statistics on these 'residence patches' form the major output of this package.

The procedure adopted here is as follows:

1. The distance between consecutive locations is calculated, and compared against 2 * `bufferSize`, which has a default value of 10m [^1]. Points within 20m of the previous point are clustered together.
2. The function counts the number of locations in each of these preliminary clusters, and removes entire clusters which have fewer than `minFixes` locations.

3. The first, last, and mean value of time, X coordinate, Y coordinate, and residence time of each of the clusters is counted. These are used to assess spatio-temporal independence between successive clusters as follows:
    1. The time between the last timestamp of cluster _N_ and the first timestamp of _N+1_ is compared against `tempIndepLim` [^2].
    2. The distance between the last position of cluster _N_ and the first position of _N+1_ is compared against `spatIndepLim` [^2].
    3. The absolute difference in mean residence times between _N_ and _N+1_ is compared against `restIndepLim`.
    4. If any of the three comparisons satisfies a greater-than predicate, the clusters are considered independent.
    5. If not, cluster _N+1_ is merged with _N_.

4. These merged clusters are called residence patches, and the first, last, and mean of each of the following are calculated: _time, X coordinate, Y coordinate, time since high tide, and residence time_.

5. Spatial metrics are added in the form of the _distance in the patch_ (the distance between successive points classified into a residence patch), and the _displacement in a patch_ (the linear distance between the first and last coordinates), and _distance between patches_ for successive patches only.

6. Quality metrics are added: the _number of locations_, _duration_, and _proportion of fixes_ in a patch.

7. The patches are converted into `sf` based spatial polygons by constructing and dissolving buffers of `bufferSize` around each point. From these, _patch area_ and _circularity_  measures are added. The Polsby-Popper metric of roundness [(Polsby and Popper. 1991)](https://pdfs.semanticscholar.org/0524/95555a23d961a674ccf1c82ceb475ac21821.pdf) is used: _4 * pi * patch area / patch perimeter squared_. A perfect circle would have a value of 1.0 using this method.

8. A column for patch _type_ is added, indicating whether the patch consists only of empirical `real` points, `inferred` points, or `mixed` points.

9. Finally, patches with a mean _time since high tide_ outside the tidal limits specified in `tideLims` are removed, the remaining patches are renumbered, and the resulting `sf` object is returned.

## Auxiliary functions

### Getting residence patch data

The `sf` class object generated above can be stripped of the underlying point locations using `funcGetPatchData`, or conversely, the spatial object can be removed to return the component points with associated patch summary data.

### Fast functions for patch distances

There are three functions that are used in the main methods that implement fast distance calculations, with the caveat that Euclidean space is assumed. These are `funcDistance` for distances between consecutive points, `funcBwPatchDistance` for distances between any two points, and typically used for between patch distances, and `funcPatchTraj` to convert the linear path between patches (last point of _N_ to first point of _N+1_) into `sf MULTILINESTRING` objects for mapping.


## Footnotes

[^1]: The distance of the first point from the previous point is considered unknown, and assigned `Inf` for calculation. 2 * `bufferSize` is the maximum distance possible between the centres of two circles for buffers of radius `bufferSize` m around them to just touch.

[^2]: As before, the first cluster's time and spatial difference to the cluster prior is unknown and set to `Inf`.
