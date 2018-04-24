# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Variable" menu related functions ===========================================

# Manage variables -----------------------------------------------------------


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_all_chr_to_fctr <- function() {
    # Library("biostat")

    # doItAndPrint(glue::glue(
    #     "{ActiveDataSet()} <- biostat::all_chr_to_factor({ActiveDataSet()})"))

    # Library(c("tidyverse", "forcats", "dplyr"))
    Library("tidyverse")
    Library("forcats")
    Library("dplyr")

    doItAndPrint(style_cmd(glue::glue(
        "{ActiveDataSet()} <- {ActiveDataSet()} %>% \n",
        "dplyr::mutate_if(is.character, as_factor)")))

    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
