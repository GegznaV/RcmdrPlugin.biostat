
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
version](http://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.rstudio.com/web/packages/RcmdrPlugin.biostat/index.html)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.14-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![Travis-CI Build
Status](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat.png?branch=master)](https://travis-ci.org/GegznaV/RcmdrPlugin.biostat)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2018--05--05-yellowgreen.svg)](/commits/master)

-----

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

<!-- 
An *R Commander* Plugin for Basic (Bio)Statistical Routines  
-->

# An R Commander Plug-in for Basic Data Analysis Tasks

<center>

<font color="red"> The package ***RcmdrPlugin.biostat*** is still in its
**development stage** and some functions are for demonstration and test
purposes only as they may change in the future. Some menus are not fully
functional yet. </font>

</center>

Documentation and more information available at
<http://gegznav.github.io/RcmdrPlugin.biostat/>

## Install the package

To install a developement version of the package from `GitHub`:

``` r
if (!"devtools" %in% installed.packages())
        install.packages("devtools")

devtools::install_github("GegznaV/RcmdrPlugin.biostat")
```

-----

# Other related packages

Other related packages:

1.  **biostat** is an `R` package that contains a collection of
    functions that do some common statistical routines without writing
    too much code ([homepage](https://gegznav.github.io/biostat/));
2.  **RcmdrPlugin.EZR.2** – an *R Commander* plugin for the most common
    statistical analyses (the same as *RcmdrPlugin.EZR*, except that
    *RcmdrPlugin.EZR.2* does not modify original *Rcmdr* menu so
    dramatically);
3.  **RcmdrPlugin.KMggplot2** – an *R Commander* plugin for *ggplot2*
    graphics.

To install these packages, use the following code:

``` r
# biostat
devtools::install_github("GegznaV/biostat")

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

<!-- [![Travis-CI Build Status]
(https://travis-ci.org/GegznaV/RcmdrPlugin.biostat.png?branch=master)]
(https://travis-ci.org/GegznaV/RcmdrPlugin.biostat) -->

<!-- [![codecov.io]
(https://codecov.io/github/GegznaV/RcmdrPlugin.biostat/coverage.svg?branch=master)]
(https://codecov.io/github/GegznaV/RcmdrPlugin.biostat?branch=master) -->

<!-- * * * -->

<!--  <p align="right"> </p>     -->
