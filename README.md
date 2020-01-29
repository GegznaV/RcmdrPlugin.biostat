
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.r-project.org/package=RcmdrPlugin.biostat)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.60-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/hm4h2rjb8ayr2df1/branch/master?svg=true)](https://ci.appveyor.com/project/GegznaV/rcmdrplugin-biostat/branch/master)
[![Travis-CI Build
Status](https://travis-ci.com/GegznaV/RcmdrPlugin.biostat.png?branch=master)](https://travis-ci.com/GegznaV/RcmdrPlugin.biostat)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2020--01--29-yellowgreen.svg)]()
<!-- badges: end -->

-----

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

<!-- 
An *R Commander* Plugin for Basic (Bio)Statistical Routines  
-->

# R Commander Plug-in for Basic Data Management and Analysis Tasks

**RcmdrPlugin.biostat** is an
[**Rcmdr**](https://CRAN.R-project.org/package=Rcmdr) plug-in for the
most common data wrangling, visualization, and analysis tasks. In the
package, **tidyverse** family functions, as well as functions from other
packages, are used. The documentation is available at
[gegznav.github.io/RcmdrPlugin.biostat](https://gegznav.github.io/RcmdrPlugin.biostat).

## Install the package

### Preparation

Before you begin installation or updating, make sure that only a single
*R*/*RStudio* session is running and restart the current *R* session (to
unload the packages). It is also recommended to close the current
*RStudio* project if you use one. *Windows* users need
[*Rtools*](https://cloud.r-project.org/bin/windows/Rtools/) and *Mac*
users need [*XQuartz*](http://www.xquartz.org/) to be installed (see
also [Notes for *Mac* users only](#notes-for-mac-users-only)).

### Installation

To install the development version of the package from “GitHub”:

``` r
# Update all necessary CRAN packages
update.packages(ask = "graphics")

# Install required packages
if (!require("remotes"))  install.packages("remotes")

# Install RcmdrPlugin.biostat
remotes::install_github("GegznaV/RcmdrPlugin.biostat", upgrade = TRUE)
```

In case you face installation issues, follow the instructions in the
error message. You may also try removing `upgrade = TRUE` and manually
choose the packages to update. It is always recommended to restart *R*
session before the next installation too.

> **NOTE:** the most common installation issue is related to *R* package
> **rlang**. Usually, you have to delete the indicated “*00LOCK*”
> directory (you may use
> [`pacman::p_unlock()`](https://trinker.github.io/pacman/p_unlock.html),
> if you have package [**pacman**](https://trinker.github.io/pacman)
> installed or delete the directory manually), restart the current *R*
> session and try to install again.

### Previous versions

If you need a previous version of the package, find the section
“[Previous versions of
**RcmdrPlugin.biostat**](#previous-versions-of-rcmdrpluginbiostat)”
below.

## Load the plug-in correctly

Package **RcmdrPlugin.biostat** can be loaded programmatically or by
using menus. *Mac* users should also read “[Notes for *Mac* users
only](#notes-for-mac-users-only)”.

1.  *Option 1.* Run code:
    
    ``` r
    library(RcmdrPlugin.biostat)
    restart_commander()
    ```
    
    A pop-up window appears. Agree to restart.

2.  *Option 2.* Run code:
    
    ``` r
    options(Rcmdr = list(plugins = "RcmdrPlugin.biostat", console.output = FALSE))
    library(Rcmdr)
    ```

3.  *Option 3:* use *RStudio* and *R Commander* menus (see section
    “[Load by using menus](#load-by-using-menus)”).

<br>

**Next**, the *BioStat* mode should be turned on (see section “[Turn on
*BioStat* (green) mode](#turn-on-biostat-green-mode)”).

### Load by using menus

1.  Load package **Rcmdr** by either selecting **Rcmdr** in *RStudio*
    „Packages“ menu:
    
    <img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/01--load-rcmdr-in-rs.png" style="display: block; margin: auto;" />
    Or by using R code:
    
    ``` r
    library(Rcmdr)
    ```
    
    Wait until *R Commander* window opens. <br>

2.  Then load the plug-in (as well as the other necessary plug-ins)
    through `Tools` → `Load Rcmdr plug-in(s)...` in the *R Commander*
    menu bar.
    
    <img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/02--load-plugin--biostat.png" style="display: block; margin: auto;" />
    If you need more than one plug-in to be loaded, hold <kbd>Ctrl</kbd>
    key while selecting the plug-ins. To load the plug-ins, *R
    Commander* must restart.

> **NOTE:** be aware that some plug-ins are incompatible one to the
> other and some combinations of plug-ins may break *R Commander* (e.g.,
> “RcmdrPlugin.EZR” and “RcmdrPlugin.EZR.as.menu”). In this kind of
> situations, you should restart *R* session and try the other
> combinations of plugins.

> **NOTE:** if “RcmdrPlugin.biostat” is not between the options of
> Plugins, it means that the plugin is already chosen but not included
> in the menus of *R Commander*. This means that you should restart the
> *R Commander* by using function `restart_commander()`.

<br>

3.  When **RcmdrPlugin.biostat** is loaded, you can access its
    functionality through `BioStat'20` menu in the menu bar:
    
    <img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/03--biostat-plugin-loaded.png" style="display: block; margin: auto;" />

### Turn on *BioStat* (green) mode

A more convenient way to use the plug-in is to turn on the *BioStat*
mode. This mode uses the green icon instead of the blue one.

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/04--activate-biostat-mode.png" style="display: block; margin: auto;" />

<br>

In this mode, additional buttons, which enable access to the most common
functions, are added below the menu bar:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/05--buttons-in-biostat-mode.png" style="display: block; margin: auto;" />

**NOTE:** If an active dataset is not selected, some buttons are shaded
in gray and are inactive.

<br>

### Close *BioStat* mode

To close this mode, simply restart the *R Commander*:

  - R Commander menu bar → BioStat’20 → Session → Restart R Commander

You may use the following command as well:

``` r
restart_commander()
```

### Notes for *Mac* users only

For *Mac* users, there at least 2 things to pay attention to:

1)  On *Mac*, *R Commander* opens only if
    [*XQuartz*](http://www.xquartz.org/) is opened. So, the first step
    of using *R Commander* is to make sure that *XQuartz* is opened.
    More information on [“Installing R Commander for *Mac*
    users”](https://murraystate.instructure.com/courses/1252125/pages/installing-r-commander-on-your-mac).

2)  Some *Mac* users also report that it is impossible to switch between
    languages while *R Commander* is running. This means, that some
    users might not use numbers if in the chosen language the same keys
    mean some non-English letters. Because of this, it is recommended to
    choose an appropriate language (usually English) before opening
    *R*/*RStudio*.

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

After the packages are installed, you can load them programmatically:

``` r
options(Rcmdr = list(
  plugins = c(
    "RcmdrPlugin.KMggplot2",
    "RcmdrPlugin.EZR.as.menu",
    "RcmdrPlugin.biostat",
    NULL
  ), 
  console.output = FALSE
))

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
*RcmdrPlugin.EZR*. All of those packages are under the GPL-2/GPL-3
license.

-----

# Previous versions of **RcmdrPlugin.biostat**

If you need a previous version of **RcmdrPlugin.biostat**, you can
install them from `GitHub`:

  - BioStat’19 (Autumn 2019):
    
    ``` r
    remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19r")
    ```
    
    (r – lith. “ruduo”)

  - BioStat’19 (Spring 2019):
    
    ``` r
    remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19")
    ```

  - BioStat’18 (2018):
    
    ``` r
    remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat18")
    ```

**NOTE:** these versions are no longer maintained and may not work
properly.

<!-- * * * -->

<!--  <p align="right"> </p>     -->
