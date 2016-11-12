#' Density plot Subclass
#'
#' \code{gDensity} class is a subclass for density plots.
#'
#' This class is a subclass which show dialog boxes of density plots for graphics editing.
#'
#' @section Fields:
#' \describe{
#' \item{\code{top}: }{\code{tkwin} class object; parent of widget window.}
#' \item{\code{alternateFrame}: }{\code{tkwin} class object; a special frame for some GUI parts.}
#' \item{\code{vbbox1}: }{\code{variableboxes} class object; the frame to select variables.}
#' \item{\code{vbbox2}: }{\code{variableboxes} class object; the frame to select facet variables.}
#' \item{\code{lbbox1}: }{\code{textfields} class object; the frame to set axis labels and the main title.}
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
#' @name gDensity-class
#' @aliases gDensity
#' @rdname plot-gDensity
#' @docType class
#' @keywords hplot
#' @importFrom scales percent_format
#' @importFrom RColorBrewer brewer.pal
#' @export gDensity
gDensity <- setRefClass(
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Class = "gDensity",
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fields = c("vbbox1", "vbbox2", "lbbox1", "adjust_box1", "tbbox1"),
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    contains = c("plot_base"),
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    methods = list(

        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        setFront = function() {
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
            vbbox1 <<- variableboxes$new()
            vbbox1$front(
                top       = top,
                types     = list(nonFactors(),
                                 Factors(),
                                 c("black","gray","red", "blue", "green4","white")
                                 ),
                titles    = list(gettext_Bio("Variable (pick one)"),
                                 gettext_Bio("Stratum variable"),
                                 gettext_Bio("Color (for single group only)")
                                 ),
                initialSelection = list(0, FALSE, 0)
            )
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
            vbbox2 <<- variableboxes$new()
            vbbox2$front(
                top       = top,
                types     = list(Factors(), Factors()),
                titles    = list(
                    gettext_Bio("Facet variable in rows"),
                    gettext_Bio("Facet variable in cols")
                )
            )
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
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
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
            adjust_box1 <<- textfields$new()
            adjust_box1$front(
                top        = top,
                initValues = list("1"),
                titles     = list(
                    gettext_Bio("Bandwidth adjustment coef. Use 0-1 to make narower or 1-100 to broaden.")
                )
            )
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
            tbbox1 <<- toolbox$new()
            tbbox1$front(top)
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        setBack = function() {

            vbbox1$back()
            vbbox2$back()
            lbbox1$back()
            adjust_box1$back()

            # boxlist <- c(
            #     list(vbbox3$frame),
            #     list(labelRcmdr(alternateFrame, text="    ")),
            #     list(cbbox1$frame),
            #     list(labelRcmdr(alternateFrame, text="    ")),
            #     list(rbbox1$frame)
            # )
            # do.call(tkgrid, c(vbbox3$back_list, list(sticky="nw")))
            # do.call(tkgrid, c(boxlist, list(sticky="nw")))
            tkgrid(alternateFrame, stick="nw")
            tkgrid(labelRcmdr(alternateFrame, text="    "), stick="nw")

            tbbox1$back()

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getWindowTitle = function() {
            gettext_Bio("Density plot")
        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getHelp = function() {
            "geom_density"
        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getParms = function() {

            x      <- getSelection(vbbox1$variable[[1]])
            # y      <- ""
            z        <- getSelection(vbbox1$variable[[2]])
            z_single <- getSelection(vbbox1$variable[[3]])

            s      <- getSelection(vbbox2$variable[[1]])
            t      <- getSelection(vbbox2$variable[[2]])

            x      <- checkVariable(x)
            s      <- checkVariable(s)
            t      <- checkVariable(t)

            xlab   <- tclvalue(lbbox1$fields[[1]]$value)
            xauto  <- x
            ylab   <- tclvalue(lbbox1$fields[[2]]$value)

            yauto  <- "Density"

            zlab   <- tclvalue(lbbox1$fields[[3]]$value)
            zauto  <- z
            main   <- tclvalue(lbbox1$fields[[4]]$value)

            adjust <- tclvalue(adjust_box1$fields[[1]]$value)

            size   <- tclvalue(tbbox1$size$value)
            family <- getSelection(tbbox1$family)
            colour <- getSelection(tbbox1$colour)
            save   <- tclvalue(tbbox1$goption$value[[1]])
            theme  <- checkTheme(getSelection(tbbox1$theme))
            # ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~   ~~~
            options(
                Bio_FontSize   = tclvalue(tbbox1$size$value),

                Bio_FontFamily = seq_along(tbbox1$family$varlist)[
                    tbbox1$family$varlist == getSelection(tbbox1$family)
                    ] - 1,

                Bio_ColourSet  = seq_along(tbbox1$colour$varlist)[
                    tbbox1$colour$varlist == getSelection(tbbox1$colour)
                    ] - 1,

                Bio_SaveGraph  = tclvalue(tbbox1$goption$value[[1]]),

                Bio_Theme      = seq_along(tbbox1$theme$varlist)[
                    tbbox1$theme$varlist == getSelection(tbbox1$theme)
                    ] - 1
            )

            list(
                x = x,
                # y = y,
                z = z,
                z_single = z_single,
                s = s,
                t = t,

                xlab  = xlab,
                xauto = xauto,
                ylab  = ylab,
                yauto = yauto,
                zlab  = zlab,
                zauto = zauto,
                main  = main,

                size = size,
                family = family,
                colour = colour,
                save = save,
                theme = theme,

                adjust = adjust
                )

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        checkError = function(parms) {

            if (length(parms$x) == 0) {
                errorCondition(
                    recall  = windowDensity,
                    message = gettext_Bio("Variable is not selected")
                )
                errorCode <- TRUE
            } else {
                errorCode <- FALSE
            }
            errorCode

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        setDataframe = function(parms) {

            var <- list()
            if (length(parms$x) != 0) {
                var <- c(var, paste0("x = ", ActiveDataSet(), "$", parms$x))
            }
            if (length(parms$s) != 0) {
                var <- c(var, paste0("s = ", ActiveDataSet(), "$", parms$s))
            }
            if (length(parms$t) != 0) {
                var <- c(var, paste0("t = ", ActiveDataSet(), "$", parms$t))
            }
            if (length(parms$z) != 0) {
                var <- c(var, paste0("z = ", ActiveDataSet(), "$", parms$z))
            }
            command <- do.call(paste, c(var, list(sep = ",\n")))
            command <- paste0(".df <- data.frame(", command, ")")

            commandDoIt(command)
            registRmlist(.df)

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getGgplot = function(parms) {

            if (length(parms$z) == 0) {
                ggplot <- "ggplot(data = .df, aes(x = x)) + \n"
            } else {
                ggplot <- paste0("ggplot(data = .df, aes(x = x, color = z, fill = z)) + \n")}

            ggplot

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getGeom = function(parms) {

            # Select binwidth adjustment coef.
            bw_adjustment <- parms$adjust
            command <- paste0(".bw_adjust <- ", bw_adjustment)


            commandDoIt(command)
            registRmlist(.bw_adjust)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Geom

            if (length(parms$z) == 0) {
                geom <- paste0("   geom_density(adjust = .bw_adjust,\n",
                               tab(3),"size = 1, alpha = 0.3,\n",
                               tab(3),"color = \"", parms$z_single, "\",",
                                     " fill = \"",  parms$z_single, "\") + \n")
                parms$z_single
            } else {
                geom <- paste0("   geom_density(adjust = .bw_adjust,\n",
                               tab(3),"size = 1, alpha = 0.3) + \n")
            }

            geom

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getScale = function(parms) {
            scale <-  paste0(
                tab(), "scale_y_continuous(expand = c(0.01, 0)) + \n",
                tab(), "scale_fill_brewer(palette = \"",   parms$colour, "\") + \n",
                tab(), "scale_colour_brewer(palette = \"", parms$colour, "\") + \n"
                )
            scale
        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getZlab = function(parms) {

            if (length(parms$z) == 0) {
                zlab <- ""
            } else if (parms$zlab == "<auto>") {
                zlab <- paste0("labs(fill = \"", parms$z, "\", colour = \"", parms$z, "\") + \n")
                }
            zlab

        },
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        getOpts = function(parms) {

            opts <- list()
            if (length(parms$s) != 0 || length(parms$t) != 0) {
                opts <- c(opts, "panel.spacing = unit(0.3, \"lines\")")
            }

            if (length(opts) != 0) {
                opts <- do.call(paste, c(opts, list(sep = ",\n")))
                opts <- paste0(" + theme(", opts, ")\n")
            } else {
                opts <- ""
            }
            opts

        }

    )
)



#' Wrapper Function of Density plot Subclass
#'
#' \code{windowDensity} function is a wrapper function of \code{gDensity} class for the R-commander menu bar.
#'
#' @rdname plot-gDensity-windowDensity
#' @keywords hplot
#' @export
windowDensity <- function() {

    Dens <- RcmdrPlugin.BioStat::gDensity$new()
    Dens$plotWindow()

}

# =============================================================================
#' Instert tab symbol
#'
#' @param n (integer) number of tabs to be inserted
#'
#' @return A string.
#' @export
#'
#' @examples
#'
#' tab()
#' # "\t"
#'
#' tab(3)
#' # "\t\t\t"
#'
tab <- function(n = 1){
    paste(rep("\t", n), collapse = "")
}
