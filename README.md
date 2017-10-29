
<!-- README.md is generated from README.Rmd. Please edit that file -->

------------------------------------------------------------------------

[![CRAN version](http://www.r-pkg.org/badges/version/RcmdrPlugin.BioStat)](https://cran.rstudio.com/web/packages/RcmdrPlugin.BioStat/index.html) [![GitHub version](https://img.shields.io/badge/GitHub-v0.0.1-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.BioStat) [![Last-update](https://img.shields.io/badge/last%20update-2017--10--29-yellowgreen.svg)](/commits/master)

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.BioStat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

`RcmdrPlugin.BioStat` -- an *R Commander* plugin for `BioStat` package
======================================================================

The package is still in its development and some functions and menus are not fully functional yet.

Documentation and more information available at <http://gegznav.github.io/RcmdrPlugin.BioStat/>

Install the package
-------------------

To install a developement version of the package from `GitHub`:

``` r
if (!"devtools" %in% installed.packages())
        install.packages("devtools")

devtools::install_github("GegznaV/RcmdrPlugin.BioStat")
```

------------------------------------------------------------------------

Other related packages
======================

Other related packages:

1.  **BioStat** is an `R` package that contains a collection of functions that do some common statistical routines without writing to much code ([homepage](https://gegznav.github.io/BioStat/));
2.  **RcmdrPlugin.EZR.2** -- an *R Commander* plugin for the most common statistical analyses (the same as *RcmdrPlugin.EZR*, except that *RcmdrPlugin.EZR.2* does not modify original *Rcmdr* menu so dramatically);
3.  **RcmdrPlugin.KMggplot2** -- an *R Commander* plugin for *ggplot2* graphics.

To install these packages, use the following code:

``` r
# BioStat
devtools::install_github("GegznaV/BioStat")

# RcmdrPlugin.EZR.2
devtools::install_github("GegznaV/RcmdrPlugin.EZR@unmodified_Rcmdr_menu")

# RcmdrPlugin.KMggplot2
install.packages("RcmdrPlugin.KMggplot2")
```

------------------------------------------------------------------------

<!-- [![Travis-CI Build Status]
(https://travis-ci.org/GegznaV/RcmdrPlugin.BioStat.png?branch=master)]
(https://travis-ci.org/GegznaV/RcmdrPlugin.BioStat) -->
<!-- [![codecov.io]
(https://codecov.io/github/GegznaV/RcmdrPlugin.BioStat/coverage.svg?branch=master)]
(https://codecov.io/github/GegznaV/RcmdrPlugin.BioStat?branch=master) -->
<!-- * * * -->
<!--  <p align="right"> </p>     -->
