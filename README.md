
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.r-project.org/package=RcmdrPlugin.biostat)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.25-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![Travis-CI Build
Status](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat.png?branch=master)](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2019--03--31-yellowgreen.svg)](/commits/master)

-----

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

<!-- 
An *R Commander* Plugin for Basic (Bio)Statistical Routines  
-->

# R Commander Plug-in for Basic Data Management and Analysis Tasks

**RcmdrPlugin.biostat** is an
[**Rcmdr**](https://CRAN.R-project.org/package=Rcmdr) plug-in for the
most common data wrangling, visualisation and analysis tasks using
“tidyverse” family functions as well as functions from other packages.

Documentation and more information available at
<http://gegznav.github.io/RcmdrPlugin.biostat>

## Install the package

To install a developement version of the package from `GitHub`:

``` r
if (!"devtools" %in% installed.packages())  install.packages("devtools")

devtools::install_github("GegznaV/RcmdrPlugin.biostat")
```

## Previous versions

If you need previous versions of `RcmdrPlugin.biostat`, you can install
them from `GitHub`:

  - BioStat’18
(2018):

<!-- end list -->

``` r
devtools::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat18")
```

-----

# Other useful packages

Other useful **Rcmdr** plug-ins:

1.  **RcmdrPlugin.EZR.as.menu** – an *R Commander* plugin for the most
    common statistical analyses (functionality is the same as in
    **RcmdrPlugin.EZR**, except that **RcmdrPlugin.EZR.as.menu** creates
    a separate “EZR” menu in a menu bar and does not reorganize the
    original **Rcmdr** menus in the menu bar);
2.  **RcmdrPlugin.KMggplot2** – an *R Commander* plugin for **ggplot2**
    graphics.

To install these packages, use the following code:

``` r
# RcmdrPlugin.EZR.as.menu
devtools::install_github("GegznaV/RcmdrPlugin.EZR@ezr_as_menu")

# RcmdrPlugin.KMggplot2
install.packages("RcmdrPlugin.KMggplot2")
```

# Important

Some of the functions in the package are based on and modified from
functions in packages *Rcmdr*<!--, *RcmdrPlugin.KMggplot2*-->, and
*RcmdrPlugin.EZR*. All of those packages are under GPL-2/GPL-3 license.

-----

<!-- * * * -->

<!--  <p align="right"> </p>     -->
