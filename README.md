
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
version](http://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.rstudio.com/web/packages/RcmdrPlugin.biostat/index.html)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.22-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![Travis-CI Build
Status](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat.png?branch=master)](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2018--12--12-yellowgreen.svg)](/commits/master)

-----

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

<!-- 
An *R Commander* Plugin for Basic (Bio)Statistical Routines  
-->

# An R Commander Plug-in for Basic Data Management and Analysis Tasks

**RcmdrPlugin.biostat** is an
[**Rcmdr**](https://cran.r-project.org/web/packages/Rcmdr/index.html)
plug-in for the most common data wrangling, visualisation and analysis
tasks using “tidyverse” family functions as well as functions from other
packages.

Documentation and more information available at
<http://gegznav.github.io/RcmdrPlugin.biostat>

## Install the package

To install a developement version of the package from `GitHub`:

``` r
if (!"devtools" %in% installed.packages())  install.packages("devtools")

devtools::install_github("GegznaV/biostat")
devtools::install_github("GegznaV/RcmdrPlugin.biostat")
```

# Other related packages

Other related packages:

1.  **RcmdrPlugin.EZR.2** – an *R Commander* plugin for the most common
    statistical analyses (the same as *RcmdrPlugin.EZR*, except that
    *RcmdrPlugin.EZR.2* does not modify original *Rcmdr* menu so
    dramatically);
2.  **RcmdrPlugin.KMggplot2** – an *R Commander* plugin for *ggplot2*
    graphics.

To install these packages, use the following code:

``` r
# RcmdrPlugin.EZR.2
devtools::install_github("GegznaV/RcmdrPlugin.EZR@unmodified_Rcmdr_menu")

# RcmdrPlugin.KMggplot2
install.packages("RcmdrPlugin.KMggplot2")
```

# Important

Some of the functions in the package are based on and modified from
functions in packages *Rcmdr*, *RcmdrPlugin.EZR* and
*RcmdrPlugin.KMggplot2*. All of those packages are under GPL-2/GPL-3
license.

-----
