
<!-- README.md is generated from README.Rmd. Please edit that file -->

------------------------------------------------------------------------

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/RcmdrPlugin.biostat)](https://cran.r-project.org/package=RcmdrPlugin.biostat)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.71-brightgreen.svg)](https://github.com/GegznaV/RcmdrPlugin.biostat)
[![R build
status](https://github.com/GegznaV/RcmdrPlugin.BioStat/workflows/R-CMD-check/badge.svg)](https://github.com/GegznaV/RcmdrPlugin.BioStat/actions)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Documentation](https://img.shields.io/badge/Documentation-2023--04--29-yellowgreen.svg)]()
<!-- badges: end -->

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

<!-- 
An *R Commander* Plugin for Basic (Bio)Statistical Routines  
-->

# R Commander Plug-in for Basic Data Management and Analysis Tasks

<br>
<center>
<font color = "red"><b> This package is still experimental.<br> Some
features may change. </b></font>
</center>

<br>

**RcmdrPlugin.biostat** is an
[**Rcmdr**](https://CRAN.R-project.org/package=Rcmdr) plug-in for the
most common data wrangling, visualization, and analysis tasks. It
provides so-called *BioStat* (green) mode. In this mode, additional
button bar appears:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/05--buttons-in-biostat-mode.png" style="display: block; margin: auto;" />

**NOTE:** If an active dataset is not selected, some buttons are shaded
in gray and are inactive.

This **BioStat** mode enables access to menus that use **tidyverse**
family functions, as well as functions from other packages to perform
some basic data wrangling, cleaning, plotting, preview and analysis
tasks.

The documentation is available at
<a href="https://gegznav.github.io/RcmdrPlugin.biostat"
target="_blank">gegznav.github.io/RcmdrPlugin.biostat</a>.

<br>

------------------------------------------------------------------------

## Install the package

### Preparation

Before you begin installation or updating, make sure that only a single
*R*/*RStudio* session is running and restart the current *R* session (to
unload the packages). It is also recommended to close the current
*RStudio* project if you use one. *Mac* users need
<a href="http://www.xquartz.org/" target="_blank"><em>XQuartz</em></a>
to be installed (see also [Notes for *Mac* users
only](#notes-for-mac-users-only)) and opened during the installation.
*Windows* users need
<a href="https://cloud.r-project.org/bin/windows/Rtools/"
target="_blank"><em>Rtools</em></a> if they want to install the package
form GitHub.

### Installation (Recommended)

The most convenient way is to install the pacage form CRAN-like
repository:

``` r
repos <- c("https://mokymai.github.io/download/", getOption("repos"))
install.packages("RcmdrPlugin.biostat", repos = repos)
```

### Installation from GitHub

If you want to install the development version of the package from
“GitHub”, use:

``` r
# Update all necessary CRAN packages
update.packages(ask = "graphics")

# Install required packages
if (!require("remotes")) install.packages("remotes")

# Install RcmdrPlugin.biostat
remotes::install_github(
  "GegznaV/RcmdrPlugin.biostat",
  dependencies = TRUE, upgrade = TRUE
)
```

In case you face installation issues, follow the instructions in the
error message. You may also try removing `upgrade = TRUE` and manually
choose the packages to update. It is always recommended to restart *R*
session before the next installation too.

> **NOTE:** the most common installation issue is related to *R* package
> **rlang**. Usually, you have to delete the indicated “*00LOCK*”
> directory (you may use
> <a href="https://trinker.github.io/pacman/p_unlock.html"
> target="_blank"><code
> class="sourceCode r">pacman<span class="sc">::</span><span class="fu">p_unlock</span>()</code></a>,
> if you have package <a href="https://trinker.github.io/pacman"
> target="_blank"><strong>pacman</strong></a> installed, or delete the
> directory manually), restart the current *R* session and try to
> install again.

<!-- ### Check for updates -->
<!-- To get the most recent version of **RcmdrPlugin.biostat** and to check if all its dependency packages are installed, use a function from package [**bio**](https://mokymai.github.io/bio/): -->
<!-- ```{r update-biostat eval=FALSE} -->
<!-- bio::update_pkg_rcmdr_biostat(upgrade = TRUE) -->
<!-- ``` -->
<!-- If you do not have package **bio**, install it: -->
<!-- ```{r install-bio, eval=FALSE} -->
<!-- repos <- c("https://mokymai.github.io/download/", getOption("repos")) -->
<!-- install.packages("bio", repos = repos) -->
<!-- ``` -->
<!-- ### Previous versions -->
<!-- If you need a previous version of the package, read section "[Previous versions of **RcmdrPlugin.biostat**](#previous-versions-of-rcmdrpluginbiostat)" below. -->
<!-- <br> -->

------------------------------------------------------------------------

## Load the plug-in

The easiest way to load **RcmdrPlugin.biostat** in *BioStat* (green)
mode is to use RStudio addin:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/00-addin-to-load-biostat-mode.png" style="display: block; margin: auto;" />

Alternatively, use code:

``` r
library(Rcmdr)
library(RcmdrPlugin.biostat)
load_rcmdr_biostat_mode()
```

*Mac* users should also read “[Notes for *Mac* users
only](#notes-for-mac-users-only)”.

<!-- Package **RcmdrPlugin.biostat** can be loaded programmatically or by using menus: -->
<!-- a) *Option 1.* Run code: -->
<!--     ```{r eval=FALSE} -->
<!--     library(RcmdrPlugin.biostat) -->
<!--     load_rcmdr_biostat_mode() -->
<!--     ``` -->
<!-- b) *Option 2.* Run code: -->
<!--     ```{r eval=FALSE} -->
<!--     options(Rcmdr = list(plugins = "RcmdrPlugin.biostat", console.output = FALSE)) -->
<!--     library(Rcmdr) -->
<!--     set_biostat_mode() -->
<!--     ``` -->
<!-- c) *Option 3:* use *RStudio* and *R Commander* menus (see sections "[Load by using menus](#load-by-using-menus)" and "[Turn on *BioStat* (green) mode](#turn-on-biostat-green-mode)"). -->
<!-- <br> -->
<!-- *Mac* users should also read "[Notes for *Mac* users only](#notes-for-mac-users-only)". -->
<!-- **Next**, for some of the options above, the *BioStat* mode should be turned on by using commander (see section "[Turn on *BioStat* (green) mode](#turn-on-biostat-green-mode)"). -->
<!-- ### Load by using menus {#load-by-menu} -->
<!-- 1. Load package **Rcmdr** by either selecting **Rcmdr** in *RStudio* „Packages“ menu: -->
<!--     ```{r, echo=FALSE, fig.cap=CAPTION} -->
<!--     knitr::include_graphics(paste0(fig_dir, "01--load-rcmdr-in-rs.png"))  -->
<!--     CAPTION = "" # Figure caption/description. -->
<!--     ``` -->
<!--     Or by using *R* code:  -->
<!--     ```{r eval=FALSE} -->
<!--     library(Rcmdr) -->
<!--     ``` -->
<!--     Wait until *R Commander* window opens. -->
<!--     <br> -->
<!-- 2. Then load the plug-in (as well as the other necessary plug-ins) through  -->
<!-- `Tools` → `Load Rcmdr plug-in(s)...` in the *R Commander* menu bar. -->
<!--     ```{r, echo=FALSE, fig.cap=CAPTION} -->
<!--     knitr::include_graphics(paste0(fig_dir, "02--load-plugin--biostat.png"))  -->
<!--     CAPTION = "" # Figure caption/description. -->
<!--     ``` -->
<!--     If you need more than one plug-in to be loaded, hold <kbd>Ctrl</kbd> key while selecting the plug-ins. -->
<!-- To load the plug-ins, *R Commander* must restart. -->
<!-- > **NOTE:** be aware that some plug-ins are incompatible one to the other and  -->
<!--         some combinations of plug-ins may break *R Commander*  -->
<!--         (e.g., "RcmdrPlugin.EZR" and "RcmdrPlugin.EZR.as.menu"). -->
<!--         In this kind of situations, you should restart *R* session and try the  -->
<!--         other combinations of plugins. -->
<!-- > **NOTE:** if "RcmdrPlugin.biostat" is not between the options of Plugins, it  -->
<!--         means that the plugin is already chosen but not included in the menus of  -->
<!--         *R Commander*. This means that you should restart the *R Commander* by  -->
<!--         using function `restart_commander()`{.r}. -->
<!-- <br> -->

When **RcmdrPlugin.biostat** is loaded, you can access its functionality
through either the *BioStat* button bar (find figure above) or through
`BioStat'20` menu in the menu bar (shown as `BioStat'19` in the
example):

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/03--biostat-plugin-loaded.png" style="display: block; margin: auto;" />

### Turn on *BioStat* (green) mode

If you restarted R Commander and “BioStat” menu is present but button
bar is missing, you may re-enable *BioStat* mode by selecting option
“Mode: BioStat”:

<img src="https://raw.githubusercontent.com/GegznaV/RcmdrPlugin.biostat/master/inst/etc/fig/04--activate-biostat-mode.png" style="display: block; margin: auto;" />

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
    <a href="http://www.xquartz.org/" target="_blank"><em>XQuartz</em></a>
    is opened. So, the first step of using *R Commander* is to make sure
    that *XQuartz* is opened. More information on <a
    href="https://murraystate.instructure.com/courses/1252125/pages/installing-r-commander-on-your-mac"
    target="_blank">“Installing R Commander for <em>Mac</em> users”</a>.

2)  Some *Mac* users also report that it is impossible to switch between
    languages while *R Commander* is running. This means, that some
    users might not use numbers if in the chosen language the same keys
    mean some non-English letters. Because of this, it is recommended to
    choose an appropriate language (usually English) before opening
    *R*/*RStudio*.

------------------------------------------------------------------------

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
instead of ~~`library(Rcmdr)`~~, use this code **exactly once**:

``` r
Commander()
```

# Important

Some functions in the package are based on and modified from functions
in packages *Rcmdr*<!--, *RcmdrPlugin.KMggplot2*-->, and
*RcmdrPlugin.EZR*. All of those packages are under the GPL-2/GPL-3
license.

------------------------------------------------------------------------

<!-- # Previous versions of **RcmdrPlugin.biostat** {#previous-versions-of-rcmdrpluginbiostat} -->
<!-- If you need a previous version of **RcmdrPlugin.biostat**, you can install them from `GitHub`: -->
<!-- - BioStat'19 (Autumn 2019): -->
<!--     ```{r Install BioStat-19r, eval=FALSE} -->
<!--     remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19r") -->
<!--     ``` -->
<!--     ("r" -- stands for "ruduo" in Lithuanian). -->
<!-- - BioStat'19 (Spring 2019): -->
<!--     ```{r Install BioStat-19, eval=FALSE} -->
<!--     remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19") -->
<!--     ``` -->
<!-- - BioStat'18 (2018): -->
<!--     ```{r Install BioStat-18, eval=FALSE} -->
<!--     remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat18") -->
<!--     ``` -->
<!-- **NOTE:** these versions are no longer maintained and may not work properly. -->
<!-- * * * -->
<!--  <p align="right"> </p>     -->
