#' window_ggThemeAssist
#'
#' This function creates an example code for `ggThemeAssist`.
#' @export
#' @keywords internal
window_ggThemeAssist <- function() {

    logger(paste0(
        "# # Example code to use `ggThemeAssist`:\n",
        "# \n",
        "# library(ggplot2) \n",
        "# gg <- ggplot(trees, aes(x = Girth, y = Height, color = Volume))) + \n",
        "#     geom_point() \n",
        "# \n",
        "# ggThemeAssist::ggThemeAssist('gg') \n",
        "# \n"
        ))
}