#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_desc <- function() {

    # window_plots_parameters
        ask_before_new_plot <- devAskNewPage()

        # Before drawing the next plot:
        # - Wait for user's permission (e.g., mouse click)
        # - Automatically draw all the plots

        # record_plot_history <- windows.options()$record
        # # Record plot history:
        # # - Yes
        # # - No

        # windows.options(record = TRUE)
        # devAskNewPage(ask = TRUE)

        scipen <- options()$scipen

        # scipen:
        #     integer. A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider.

        Fmt(abs = as.fmt(digits = 0, big.mark = ""))

        options(scipen = 8)
        DescTools::Desc(iris)
}