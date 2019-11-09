# "Models" menu related functions ===========================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_std_lm_coeffs <- function() {
    .ds  <- active_dataset()
    .mod <- activeModel()

    obj <-
        str_glue("{.ds}_{.mod}") %>%
        str_trunc(width = 40, ellipsis = "") %>%
        unique_obj_names(suffix = "_std")

    Library("tidyverse")
    Library("lm.beta")
    # Library("broom")

    str_glue(
        "## Statistical findings of the model \n",
        "## with standardized coefficients\n\n",

        "{obj} <- \n {.mod} %>% \n ",
        "lm.beta::lm.beta() \n\n",
        "summary({obj})"
    ) %>%
        style_cmd() %>%
        doItAndPrint()

    doItAndPrint(str_glue(
        "## Standardized coefficients\n",
        "coef({obj})"))

    # # Does not work without `quick = TRUE`:
    # doItAndPrint(str_glue("broom::tidy({obj}, quick = TRUE)"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # "## Statistical findings of the model\n",
    # "# std_estimate \u2014 standardized coefficients\n",
    #
    # "{obj} <- \n {.mod} %>% \n ",
    # "lm.beta::l)m.beta() %>% \n ",
    # "broom::tidy() \n\n",
    # "{obj}"
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # str_glue(
    #     "## Standardized regression coefficients\n",
    #     "# (rounded to 3 decimal places)\n\n",
    #
    #     "lm.beta::lm.beta({.mod}) %>% \n ",
    #     "coef() %>% as.data.frame() %>% set_names('std_coef') %>% \n",
    #     "rownames_to_column('term') %>% ",
    #     "dplyr::mutate(influence_rank = min_rank(-abs(std_coef)),\n",
    #     "              std_coef = round(std_coef, digits = 3))"
    # ) %>%
    #     style_cmd() %>%
    #     doItAndPrint()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_get_class <- function() {
    doItAndPrint(str_glue(
        "## The class of the model\n",
        "class({ActiveModel()})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_print <- function() {
    doItAndPrint(str_glue(
        "## The basic results of the model\n",
        "print({ActiveModel()})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_summary <- function() {

    .mod <- activeModel()

    doItAndPrint(str_glue(
        "## The summary of the model (base R style)\n",
        "summary({.mod})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_glance <- function() {

    .ds  <- active_dataset()
    .mod <- activeModel()

    obj <-
        str_glue("{.ds}_{.mod}") %>%
        str_trunc(width = 40, ellipsis = "") %>%
        str_c("_glance") %>%
        unique_obj_names()

    Library("tidyverse")
    Library("broom")

    doItAndPrint(str_glue(
        "## One-row summary of the model (as data frame)\n",
        "{obj} <- broom::glance({.mod})\n",
        "as.data.frame({obj})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_tidy <- function() {

    .ds  <- active_dataset()
    .mod <- activeModel()

    obj <-
        str_glue("{.ds}_{.mod}") %>%
        str_trunc(width = 40, ellipsis = "") %>%
        str_c("_tidy") %>%
        unique_obj_names()

    Library("tidyverse")
    Library("broom")

    doItAndPrint(str_glue(
        "## Statistical findings of the model (as data frame)\n",
        "{obj} <- broom::tidy({.mod})\n",
        "{obj}"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_augment <- function() {

    .ds  <- active_dataset()
    .mod <- activeModel()

    Library("tidyverse")
    Library("broom")

    doItAndPrint(str_glue(
        "## Add data from model to original dataset\n",
        "{.ds} <- broom::augment({.mod}, data = {.ds})\n"))

    # Refresh data
    command_dataset_refresh_0(flushModel = FALSE)
}
