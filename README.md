
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.r-project.org/package=RcmdrPlugin.biostat)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.32-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![Travis-CI Build
Status](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat.png?branch=master)](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/GegznaV/RcmdrPlugin.biostat?branch=master&svg=true)](https://ci.appveyor.com/project/GegznaV/rcmdrplugin-biostat)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2019--04--27-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

-----

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

<!-- 
An *R Commander* Plugin for Basic (Bio)Statistical Routines  
-->

# R Commander Plug-in for Basic Data Management and Analysis Tasks

**RcmdrPlugin.biostat** is an
[**Rcmdr**](https://CRAN.R-project.org/package=Rcmdr) plug-in for the
most common data wrangling, visualization and analysis tasks using
**tidyverse** family functions as well as functions from other packages.

Documentation and more information available at
<http://gegznav.github.io/RcmdrPlugin.biostat>

## Install the package

To install the development version of the package from “GitHub”:

``` r
if (!"devtools" %in% installed.packages())  install.packages("devtools")

devtools::install_github("GegznaV/RcmdrPlugin.biostat")
```

## Load the plug-in correctly

To load the plug-in correctly, you should follow the following steps.
You can choose a manual or an automatic loading.

### Load manually

1.  Never load the plug-in directly by loading package
    **RcmdrPlugin.biostat**. Instead, load package **Rcmdr** first
    either by using R code:

<!-- end list -->

``` r
library(Rcmdr)
```

Or by selecting **Rcmdr** in RStudio „Packages“ menu:
<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/01--load-rcmdr-in-rs.png" style="display: block; margin: auto;" />

2.  Then load the plug-in through `Rcmdr` menu bar:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/02--load-plugin--biostat.png" style="display: block; margin: auto;" />

3.  If `RcmdrPlugin.biostat` is loaded, you can access its functionality
    through `BioStat'19` menu in the menu bar:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/03--biostat-plugin-loaded.png" style="display: block; margin: auto;" />

### Load automatically

To load **RcmdrPlugin.biostat** automatically, run the code:

``` r
options(Rcmdr = list(plugins = c("RcmdrPlugin.biostat"), console.output = FALSE))
library(Rcmdr)
```

### Activate *BioStat* (green) mode

A more convenient way to use the plug-in is to activate the *BioStat*
mode (green icon):

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/04--activate-biostat-mode.png" style="display: block; margin: auto;" />

In this mode, additional buttons, which enable access to the most common
functions, are added below the menu bar. The meaning of these buttons:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/05--buttons-in-biostat-mode.png" style="display: block; margin: auto;" />

Note, that if active dataset is not selected, some buttons are inactive

### Close *BioStat* mode

To close this green mode, simply restart the *R Commander*:

  - R Commander menu bar → BioStat’19 → Session → Restart R Commander

## Install previous versions of **RcmdrPlugin.biostat**

If you need previous versions of **RcmdrPlugin.biostat**, you can
install them from `GitHub`:

  - BioStat’19 (2019):

<!-- end list -->

``` r
devtools::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19")
```

  - BioStat’18 (2018):

<!-- end list -->

``` r
devtools::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat18")
```

-----

# The other useful packages

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
devtools::install_github("GegznaV/RcmdrPlugin.EZR", ref = "ezr_as_menu")

# RcmdrPlugin.KMggplot2
install.packages("RcmdrPlugin.KMggplot2")
```

After the packages are installed, you can load them the automatically:

``` r
options(Rcmdr = list(plugins = c(
    "RcmdrPlugin.KMggplot2",
    "RcmdrPlugin.EZR.as.menu",
    "RcmdrPlugin.biostat",
    NULL
    ), 
    console.output = FALSE))

library(Rcmdr)
```

# Important

Some of the functions in the package are based on and modified from
functions in packages *Rcmdr*<!--, *RcmdrPlugin.KMggplot2*-->, and
*RcmdrPlugin.EZR*. All of those packages are under GPL-2/GPL-3 license.

-----

<!-- * * * -->

<!--  <p align="right"> </p>     -->
