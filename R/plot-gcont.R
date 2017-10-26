#' Contour Plot Subclass
#'
#' \code{gcont} class is a subclass for contour plots.
#'
#' This class is a subclass which show dialog boxes of contour plots for graphics editing.
#'
#' @section Fields:
#' \describe{
#' \item{\code{top}: }{\code{tkwin} class object; parent of widget window.}
#' \item{\code{alternateFrame}: }{\code{tkwin} class object; a special frame for some GUI parts.}
#' \item{\code{vbbox1}: }{\code{variableboxes} class object; the frame to select variables.}
#' \item{\code{vbbox2}: }{\code{variableboxes} class object; the frame to select facet variables.}
#' \item{\code{lbbox1}: }{\code{textfields} class object; the frame to set axis labels, the legend label, and the main title.}
#' \item{\code{rbbox1}: }{\code{radioboxes} class object; the frame to set the decoration type.}
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
#' @name gcont-class
#' @aliases gcont
#' @rdname plot-gcont
#' @docType class
#' @keywords hplot
#' @importFrom RColorBrewer brewer.pal
#' @export gcont
gcont <- setRefClass(

  Class = "gcont",

  fields = c("vbbox1", "vbbox2", "lbbox1", "rbbox1", "tbbox1"),

  contains = c("plot_base"),

  methods = list(

    setFront = function() {

      vbbox1 <<- variableboxes$new()
      vbbox1$front(
        top       = top, 
        types     = list(nonFactors(), nonFactors(), Numeric()),
        titles    = list(
          gettext_Bio("X variable (pick one)"),
          gettext_Bio("Y variable (pick one)"),
          gettext_Bio("Z variable (pick one)")
        ),
        initialSelection = list(0, 1, 2)
      )

      vbbox2 <<- variableboxes$new()
      vbbox2$front(
        top       = top, 
        types     = list(Factors(), Factors()),
        titles    = list(
          gettext_Bio("Facet variable in rows"),
          gettext_Bio("Facet variable in cols")
        )
      )

      lbbox1 <<- textfields$new()
      lbbox1$front(
        top        = top,
        initValues = list("<auto>", "<auto>", "<auto>", ""),
        titles     = list(
          gettext_Bio("Horizontal axis label"),
          gettext_Bio("Vertical axis label"),
          gettext_Bio("Legend label"),
          gettext_Bio("Title")
        )
      )

      rbbox1 <<- radioboxes$new()
      rbbox1$front(
        top    = top,
        labels = list(
          gettext_Bio("None"),
          gettext_Bio("Coloured lines"),
          gettext_Bio("Heat map")
        ),
        title  = gettext_Bio("Options")
      )

      tbbox1 <<- toolbox$new()
      tbbox1$front(top)

    },

    setBack = function() {

      vbbox1$back()
      vbbox2$back()
      lbbox1$back()
      rbbox1$back()
      tbbox1$back()

    },

    getWindowTitle = function() {
      
      gettext_Bio("Contour plot")
      
    },
    
    getHelp = function() {
      
      "stat_smooth"
      
    },

    getParms = function() {

      x      <- getSelection(vbbox1$variable[[1]])
      y      <- getSelection(vbbox1$variable[[2]])
      z      <- getSelection(vbbox1$variable[[3]])

      s      <- getSelection(vbbox2$variable[[1]])
      t      <- getSelection(vbbox2$variable[[2]])

      x      <- checkVariable(x)
      y      <- checkVariable(y)
      z      <- checkVariable(z)
      s      <- checkVariable(s)
      t      <- checkVariable(t)

      xlab   <- tclvalue(lbbox1$fields[[1]]$value)
      xauto  <- x
      ylab   <- tclvalue(lbbox1$fields[[2]]$value)
      yauto  <- y
      zlab   <- tclvalue(lbbox1$fields[[3]]$value)
      main   <- tclvalue(lbbox1$fields[[4]]$value)

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
      
      decoType <- tclvalue(rbbox1$value)

      list(
        x = x, y = y, z = z, s = s, t = t,
        xlab = xlab, xauto = xauto, ylab = ylab, yauto = yauto, zlab = zlab, main = main,
        size = size, family = family, colour = colour, save = save, theme = theme,
        decoType = decoType
      )

    },

    checkError = function(parms) {

      if (length(parms$x) == 0) {
        errorCondition(
          recall  = windowContour,
          message = gettext_Bio("X variable is not selected")
        )
        errorCode <- TRUE
      } else if (length(parms$y) == 0) {
        errorCondition(
          recall  = windowContour,
          message = gettext_Bio("Y variable is not selected")
        )
        errorCode <- TRUE
      } else if (length(parms$z) == 0) {
        errorCondition(
          recall  = windowContour,
          message = gettext_Bio("Z variable is not selected")
        )
        errorCode <- TRUE
      } else if (parms$x == parms$y || parms$x == parms$z || parms$y == parms$z) {
        errorCondition(
          recall  = windowContour,
          message = gettext_Bio("Each variable must be different.")
        )
        errorCode <- TRUE
      } else {
        errorCode <- FALSE
      }
      errorCode

    },

    getGgplot = function(parms) {

      "ggplot(data = .df, aes(x = x, y = y, z = z)) + \n  "

    },

    getGeom = function(parms) {

      if (parms$decoType == "1") {
        geom <- "stat_contour(size = 1) + \n  "
      } else if (parms$decoType == "2") {
        geom <- "stat_contour(aes(colour = ..level..), size = 1) + \n  "
      } else {
        geom <- "geom_tile(aes(fill = z)) + \n  stat_contour(size = 1) + \n  "
      }
      geom

    },

    getScale = function(parms) {
      
      scale <- paste0(
        "scale_x_continuous(expand = c(0, 0)) + \n  ",
        "scale_y_continuous(expand = c(0, 0)) + \n  "
      )

      if (parms$decoType == "2") {
        if (parms$colour == "Default") {
        } else if (parms$colour == "Hue") {
          scale <- paste0(
            scale,
            "scale_colour_gradient(low = scale_color_hue()$palette(2)[1], high = scale_color_hue()$palette(2)[2]) + \n  "
          )
        } else if (parms$colour == "Grey") {
          scale <- paste0(
            scale,
            "scale_colour_gradient(low = scale_color_grey()$palette(2)[1], high = scale_color_grey()$palette(2)[2]) + \n  "
          )
        } else {
          scale <- paste0(
            scale, "scale_colour_gradient(",
            "low = RColorBrewer::brewer.pal(3, \"", parms$colour,  "\")[2], ",
            "high = RColorBrewer::brewer.pal(3, \"", parms$colour, "\")[1]) + \n  "
          )
        }
      } else if (parms$decoType == "3") {
        if (parms$colour == "Default") {
        } else if (parms$colour == "Hue") {
          scale <- paste0(
            scale,
            "scale_fill_gradient(low = scale_color_hue()$palette(2)[1], high = scale_color_hue()$palette(2)[2]) + \n  "
          )
        } else if (parms$colour == "Grey") {
          scale <- paste0(
            scale,
            "scale_fill_gradient(low = scale_color_grey()$palette(2)[1], high = scale_color_grey()$palette(2)[2]) + \n  "
          )
        } else {
          scale <- paste0(
            scale, "scale_fill_gradient(",
            "low = RColorBrewer::brewer.pal(3, \"", parms$colour,  "\")[2], ",
            "high = RColorBrewer::brewer.pal(3, \"", parms$colour, "\")[1]) + \n  "
          )
        }
      }
      scale
      
    },

    getZlab = function(parms) {

      if (parms$decoType == "1") {
        zlab <- "stat_contour(size = 1) + \n  "
      } else if (parms$decoType == "2") {
        if (parms$zlab == "<auto>") {
          zlab <- paste0("labs(colour = \"", parms$z, "\") + \n  ")
        } else {
          zlab <- paste0("labs(colour = \"", parms$zlab, "\") + \n  ")
        }
      } else {
        if (parms$zlab == "<auto>") {
          zlab <- paste0("labs(fill = \"", parms$z, "\") + \n  ")
        } else {
          zlab <- paste0("labs(fill = \"", parms$zlab, "\") + \n  ")
        }
      }
      zlab

    },

    getOpts = function(parms) {

      opts <- list()
      if (length(parms$s) != 0 || length(parms$t) != 0) {
        opts <- c(opts, "panel.spacing = unit(0.3, \"lines\")")
      }

      if (parms$decoType != "1") {
        if (nchar(parms$zlab) == 0) {
          opts <- c(opts, "legend.position = \"right\"", "legend.title = element_blank()")
        } else {
          opts <- c(opts, "legend.position = \"right\"")
        }
      }

      if (length(opts) != 0) {
        opts <- do.call(paste, c(opts, list(sep = ", ")))
        opts <- paste0(" + \n  theme(", opts, ")")
      } else {
        opts <- ""
      }
      opts

    }

  )
)



#' Wrapper Function of Contour Plot Subclass
#'
#' \code{windowContour} function is a wrapper function of \code{gcont} class for the R-commander menu bar.
#'
#' @rdname plot-gcont-windowContour
#' @keywords hplot
#' @export
windowContour <- function() {

  Contour <- RcmdrPlugin.BioStat::gcont$new()
  Contour$plotWindow()

}
