#' Scatter Plot Matrix Subclass
#'
#' \code{gscatmat} class is a subclass for a scatter plot matrix.
#'
#' This class is a subclass which show dialog boxes of a scatter plot matrix for graphics editing.
#'
#' @section Fields:
#' \describe{
#' \item{\code{top}: }{\code{tkwin} class object; parent of widget window.}
#' \item{\code{alternateFrame}: }{\code{tkwin} class object; a special frame for some GUI parts.}
#' \item{\code{vbbox1}: }{\code{variableboxes} class object; the frame to select variables.}
#' \item{\code{lbbox1}: }{\code{textfields} class object; the frame to set the main title.}
#' \item{\code{rbbox1}: }{\code{radioboxes} class object; the frame to set the smoothing type.}
#' \item{\code{tbbox1}: }{\code{toolbox} class object; the frame to set the font, the colour set, other option, and the theme.}
#' }
#' @section Contains:
#' NULL
#' @section Methods:
#' \describe{
#' \item{\code{plotWindow()}: }{Create the window that make plots.}
#' \item{\code{savePlot(plot)}: }{Save the plot.}
#' \item{\code{registRmlist(object)}: }{Register deletable temporary objects.}
#' \item{\code{removeRmlist()}: }{Remove registered temporary objects.}
#' \item{\code{setFront()}: }{Set front parts of frames.}
#' \item{\code{setBack()}: }{Set back parts of frames.}
#' \item{\code{getWindowTitle()}: }{Get the title of the window.}
#' \item{\code{getHelp()}: }{Get the title of the help document.}
#' \item{\code{getParms()}: }{Get graphics settings parameters.}
#' \item{\code{checkTheme(index)}: }{Check themes.}
#' \item{\code{checkVariable(var)}: }{Check a variable length.}
#' \item{\code{checkError(parms)}: }{Check errors.}
#' \item{\code{setDataframe(parms)}: }{Set data frames.}
#' \item{\code{getGgplot(parms)}: }{Get \code{ggplot}.}
#' \item{\code{getGeom(parms)}: }{Get \code{geom}.}
#' \item{\code{getScale(parms)}: }{Get \code{scale}.}
#' \item{\code{getCoord(parms)}: }{Get \code{coord}.}
#' \item{\code{getFacet(parms)}: }{Get \code{facet}.}
#' \item{\code{getXlab(parms)}: }{Get \code{xlab}.}
#' \item{\code{getYlab(parms)}: }{Get \code{ylab}.}
#' \item{\code{getZlab(parms)}: }{Get \code{zlab}.}
#' \item{\code{getMain(parms)}: }{Get the main label.}
#' \item{\code{getTheme(parms)}: }{Get \code{theme}.}
#' \item{\code{getOpts(parms)}: }{Get other \code{opts}.}
#' \item{\code{getPlot(parms)}: }{Get the plot object.}
#' \item{\code{getMessage()}: }{Get the plot error message.}
#' \item{\code{commandDoIt(command)}: }{An wrapper function for command execution.}
#' }
#' @family plot
#'
#' @name gscatmat-class
#' @aliases gscatmat
#' @rdname plot-gscatmat
#' @docType class
#' @keywords hplot
#' @export gscatmat
gscatmat <- setRefClass(

  Class = "gscatmat",

  fields = c("vbbox1", "vbbox2", "lbbox1", "rbbox1", "tbbox1"),

  contains = c("plot_base"),

  methods = list(

    setFront = function() {

      vbbox1 <<- variableboxes$new()
      vbbox1$front(
        top       = top, 
        types     = list(nonFactors(), Factors()),
        titles    = list(
          gettext_Bio("Select variables (three or more)"),
          gettext_Bio("Stratum variable")
        ),
        modes     = list("multiple", "single"),
        initialSelection = list(0:2, FALSE)
      )

      lbbox1 <<- textfields$new()
      lbbox1$front(
        top        = top,
        initValues = list("<auto>", ""),
        titles     = list(
          gettext_Bio("Legend label"),
          gettext_Bio("Title")
        )
      )

      rbbox1 <<- radioboxes$new()
      rbbox1$front(
        top    = top,
        labels = list(
          gettext_Bio("None"),
          gettext_Bio("Smoothing with C.I. (linear regression)"),
          gettext_Bio("Smoothing without C.I. (linear regression)"),
          gettext_Bio("Smoothing with C.I. (loess or gam)"),
          gettext_Bio("Smoothing without C.I. (loess or gam)")
        ),
        title  = gettext_Bio("Smoothing type")
      )

      tbbox1 <<- toolbox$new()
      tbbox1$front(top)

    },

    setBack = function() {

      vbbox1$back()
      lbbox1$back()
      rbbox1$back()
      tbbox1$back()

    },

    getWindowTitle = function() {
      
      gettext_Bio("Scatter plot matrix")
      
    },
    
    getHelp = function() {
      
      "plotmatrix"
      
    },

    getParms = function() {

      x      <- getSelection(vbbox1$variable[[1]])
      y      <- character(0)
      z      <- getSelection(vbbox1$variable[[2]])

      s      <- character(0)
      t      <- character(0)

      xlab   <- ""
      xauto  <- ""
      ylab   <- ""
      yauto  <- ""
      zlab   <- tclvalue(lbbox1$fields[[1]]$value)
      main   <- tclvalue(lbbox1$fields[[2]]$value)

      size   <- tclvalue(tbbox1$size$value)
      family <- getSelection(tbbox1$family)
      colour <- getSelection(tbbox1$colour)
      save   <- tclvalue(tbbox1$goption$value[[1]])
      theme  <- checkTheme(getSelection(tbbox1$theme))
      
      options(
        Bio_FontSize   = tclvalue(tbbox1$size$value),
        Bio_FontFamily = seq_along(tbbox1$family$varlist)[tbbox1$family$varlist == getSelection(tbbox1$family)] - 1,
        Bio_ColourSet  = seq_along(tbbox1$colour$varlist)[tbbox1$colour$varlist == getSelection(tbbox1$colour)] - 1,
        Bio_SaveGraph  = tclvalue(tbbox1$goption$value[[1]]),
        Bio_Theme      = seq_along(tbbox1$theme$varlist)[tbbox1$theme$varlist == getSelection(tbbox1$theme)] - 1
      )
      
      smoothType   <- tclvalue(rbbox1$value)

      list(
        x = x, y = y, z = z, s = s, t = t,
        xlab = xlab, xauto = xauto, ylab = ylab, yauto = yauto, zlab = zlab, main = main,
        size = size, family = family, colour = colour, save = save, theme = theme,
        smoothType = smoothType
      )

    },

    checkError = function(parms) {

      if (length(parms$x) < 3) {
        errorCondition(
          recall  = windowScatter,
          message = gettext_Bio("Please select more than 3 variables.")
        )
        errorCode <- TRUE
      } else {
        errorCode <- FALSE
      }
      errorCode

    },

    setDataframe = function(parms) {

      command <- do.call(paste, c(parms$x, list(sep = "\", \"")))
      command <- paste0(".df <- ", ActiveDataSet(), "[c(\"", command, "\")]")
      
      commandDoIt(command)
      
      command <- paste0(
        ".grid <- expand.grid(x = 1:ncol(.df), y = 1:ncol(.df))\n",
        ".grid <- subset(.grid, x != y)\n",
        ".all <- do.call(\"rbind\", lapply(1:nrow(.grid), function(i) {\n",
        "  xcol <- .grid[i, \"x\"]; \n",
        "  ycol <- .grid[i, \"y\"]; \n",
        "  data.frame(xvar = names(.df)[ycol], yvar = names(.df)[xcol],\n",
        "             x = .df[, xcol], y = .df[, ycol], .df)\n",
        "}))\n",
        ".all$xvar  <- factor(.all$xvar, levels = names(.df))\n",
        ".all$yvar  <- factor(.all$yvar, levels = names(.df))\n",
        ".densities <- do.call(\"rbind\", lapply(1:ncol(.df), function(i) {\n",
        "  .tmp <- as.data.frame(density(x = .df[, i])[c(\"x\", \"y\")]); \n",
        "  .tmp$y <- .tmp$y/max(.tmp$y)*diff(range(.tmp$x)) + min(.tmp$x); \n",
        "  data.frame(xvar = names(.df)[i], yvar = names(.df)[i],\n",
        "            x = .tmp$x, y = .tmp$y)\n",
        "}))"
      )
      commandDoIt(command)
      
      if (length(parms$z) != 0) {
        command <- paste0(
          ".all <- data.frame(.all, z = rep(",
          ActiveDataSet(), "$", parms$z, ", ",
          "length = nrow(.all)))\n",
          ".densities$z <- NA"
        )
        commandDoIt(command)
      }
      
      registRmlist(.grid)
      registRmlist(.all)
      registRmlist(.densities)

    },

    getGgplot = function(parms) {

      if (length(parms$z) == 0) {
        ggplot <- paste0(
          "ggplot(.all, aes(x = x, y = y)) + \n  ",
          "facet_grid(xvar ~ yvar, scales = \"free\") + \n  ",
          "geom_point() + \n  ",
          "geom_line(aes(x = x, y = y), data = .densities) + \n  "
        )
      } else {
        ggplot <- paste0(
          "ggplot(.all, aes(x = x, y = y, colour = z, shape = z)) + \n  ",
          "facet_grid(xvar ~ yvar, scales = \"free\") + \n  ",
          "geom_point() + \n  ",
          "geom_line(aes(x = x, y = y), data = .densities, colour = \"black\") + \n  "
        )
      }
      ggplot

    },

    getGeom = function(parms) {

      if (length(parms$z) == 0) {
        aes <- ""
      } else if (parms$smoothType == "4") {
        aes <- "aes(fill = z)"
      } else {
        aes <- "aes(fill = z), "
      }
      
      if (parms$smoothType == "1") {
        geom <-  ""
      } else if (parms$smoothType == "2") {
        geom <- paste0("stat_smooth(", aes, "method = \"lm\") + \n  ")
      } else if (parms$smoothType == "3") {
        geom <- paste0("stat_smooth(", aes, "method = \"lm\", se = FALSE) + \n  ")
      } else if (parms$smoothType == "4") {
        geom <- paste0("stat_smooth(", aes, ") + \n  ")
      } else if (parms$smoothType == "5") {
        geom <- paste0("stat_smooth(", aes, "se = FALSE) + \n  ")
      }
      geom

    },
    
    getScale = function(parms) {
      
      scale <- "scale_y_continuous(expand = c(0.01, 0)) + \n  "
      if (length(parms$z) != 0) {
        if (parms$colour == "Default") {
        } else if (parms$colour == "Hue") {
          scale <- paste0(scale, "scale_colour_hue() + \n  ")
        } else if (parms$colour == "Grey") {
          scale <- paste0(scale, "scale_colour_grey() + \n  ")
        } else {
          scale <- paste0(scale, "scale_colour_brewer(palette = \"", parms$colour, "\") + \n  ")
        }
        if (parms$smoothType != "1") {
          if (parms$colour == "Default") {
            scale <- ""
          } else if (parms$colour == "Hue") {
            scale <- paste0(scale, "scale_fill_hue() + \n  ")
          } else if (parms$colour == "Grey") {
            scale <- paste0(scale, "scale_fill_grey() + \n  ")
          } else {
            scale <- paste0(scale, "scale_fill_brewer(palette = \"", parms$colour, "\") + \n  ")
          }
        }
      }
      scale
      
    },
    
    getZlab = function(parms) {
      
      if (length(parms$z) == 0) {
        zlab <- ""
      } else if (nchar(parms$zlab) == 0) {
        zlab <- ""
      } else if (parms$zlab == "<auto>") {
        if (parms$smoothType == "1") {
          zlab <- paste0("labs(colour = \"", parms$z, "\", shape = \"", parms$z, "\") + \n  ")
        } else {
          zlab <- paste0("labs(colour = \"", parms$z, "\", shape = \"", parms$z, "\", fill = \"", parms$z, "\") + \n  ")
        }
      } else {
        if (parms$smoothType == "1") {
          zlab <- paste0("labs(colour = \"", parms$zlab, "\", shape = \"", parms$zlab, "\") + \n  ")
        } else {
          zlab <- paste0("labs(colour = \"", parms$zlab, "\", shape = \"", parms$zlab, "\", fill = \"", parms$zlab, "\") + \n  ")
        }
      }
      zlab
      
    },
    
    getOpts = function(parms) {

      opts <- list()
      if (length(parms$s) != 0 || length(parms$t) != 0) {
        opts <- c(opts, "panel.spacing = unit(0.3, \"lines\")")
      }

      if (length(parms$z) != 0 && nchar(parms$zlab) == 0) {
        opts <- c(opts, "legend.position = \"right\"", "legend.title = element_blank()")
      } else if (length(parms$z) != 0 && nchar(parms$zlab) != 0) {
        opts <- c(opts, "legend.position = \"right\"")
      }
      
      if (length(opts) != 0) {
        opts <- do.call(paste, c(opts, list(sep = ", ")))
        opts <- paste0(" + \n  theme(", opts, ")")
      } else {
        opts <- ""
      }
      opts

    },

    getMessage = function() {

      gettext_Bio("Smoothing failed.  Please try another smoothing type, or check the data and variables.")

    }

  )
)



#' Wrapper Function of Scatter Plot Matrix Subclass
#'
#' \code{windowScattermat} function is a wrapper function of \code{gscatmat} class for the R-commander menu bar.
#'
#' @rdname plot-gscatmat-windowScattermat
#' @keywords hplot
#' @export
windowScattermat <- function() {

  Scattermat <- RcmdrPlugin.BioStat::gscatmat$new()
  Scattermat$plotWindow()

}
