
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.r-project.org/package=RcmdrPlugin.biostat)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.59-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/hm4h2rjb8ayr2df1/branch/master?svg=true)](https://ci.appveyor.com/project/GegznaV/rcmdrplugin-biostat/branch/master)
[![Travis-CI Build
Status](https://travis-ci.com/GegznaV/RcmdrPlugin.biostat.png?branch=master)](https://travis-ci.com/GegznaV/RcmdrPlugin.biostat)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2020--01--28-yellowgreen.svg)]()
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
# Install required packages
if (!require("remotes"))  install.packages("remotes")

# Install RcmdrPlugin.biostat
remotes::install_github("GegznaV/RcmdrPlugin.biostat", upgrade = TRUE)
```

In case you face installation issues, try removing `upgrade = TRUE`.

## Load the plug-in correctly

You should **never** load the **RcmdrPlugin.biostat** directly as you do
with other an R packages. Instead, you should load it manually by using
the R Commander tool to load the plug-ins or automatically by using a
configuration script. Please find the instructions in the following
sections.

### For Mac users only

Before continuing, open [*XQuartz*](http://www.xquartz.org/). Otherwise
“R Commander” will not open on your Mac. More information on
[installing R Commander for Mac
users"](https://murraystate.instructure.com/courses/1252125/pages/installing-r-commander-on-your-mac).

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

Wait until *R Commander* window opens.

2.  Then load the plug-in through `Tools` → `Load Rcmdr plug-in(s)` in
    the menu bar.

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/02--load-plugin--biostat.png" style="display: block; margin: auto;" />

If you need more than one plug-in to be loaded, hold <kbd>Ctrl</kbd> key
while selecting the plug-ins. To load the plug-ins, *R Commander* must
restart.

> NOTE: be aware that some plug-ins are incompatible one to the other
> and some combinations of plug-ins may break R Commander (e.g.,
> “RcmdrPlugin.EZR” and “RcmdrPlugin.EZR.as.menu”). In this kind of
> situations, you should restart R session and try the other combination
> of plugins.

3.  If **RcmdrPlugin.biostat** is loaded, you can access its
    functionality through `BioStat'19` menu in the menu bar:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/03--biostat-plugin-loaded.png" style="display: block; margin: auto;" />

### Load by using script

To load **RcmdrPlugin.biostat** automatically, run the code:

``` r
options(Rcmdr = list(plugins = "RcmdrPlugin.biostat", console.output = FALSE))
library(Rcmdr)
```

### Activate *BioStat* (green) mode

A more convenient way to use the plug-in is to activate the *BioStat*
mode. This mode uses the green icon instead of the blue one.

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/04--activate-biostat-mode.png" style="display: block; margin: auto;" />

In this mode, additional buttons, which enable access to the most common
functions, are added below the menu bar:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/05--buttons-in-biostat-mode.png" style="display: block; margin: auto;" />

Note: if active dataset is not selected, some buttons are inactive.

### Close *BioStat* mode

To close this mode, simply restart the *R Commander*:

  - R Commander menu bar → BioStat’19 → Session → Restart R Commander

## Install previous versions of **RcmdrPlugin.biostat**

If you need previous versions of **RcmdrPlugin.biostat**, you can
install them from `GitHub`:

  - BioStat’19 (Autumn 2019):

<!-- end list -->

``` r
remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19r")
```

(r – lith. “ruduo”)

  - BioStat’19 (Spring 2019):

<!-- end list -->

``` r
remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19")
```

  - BioStat’18 (2018):

<!-- end list -->

``` r
remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat18")
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
remotes::install_github("GegznaV/RcmdrPlugin.EZR", ref = "ezr_as_menu")

# RcmdrPlugin.KMggplot2
install.packages("RcmdrPlugin.KMggplot2")
```

After the packages are installed, you can load them automatically:

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

If package **Rcmdr** is loaded **and R Commander window is closed**,
instead of `library(Rcmdr)` use:

``` r
Commander()
```

# Important

Some functions in the package are based on and modified from functions
in packages *Rcmdr*<!--, *RcmdrPlugin.KMggplot2*-->, and
*RcmdrPlugin.EZR*. All of those packages are under GPL-2/GPL-3 license.

-----

<!-- * * * -->

<!--  <p align="right"> </p>     -->
