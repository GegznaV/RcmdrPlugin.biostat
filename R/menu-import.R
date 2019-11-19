
# ============================================================================
# Import dataset =============================================================
# ============================================================================

# installed.packages()

# decreasing: FALSE, TRUE, NULL


#' @param which `"loaded"`, `"installed"`)
#' @param decreasing `FALSE`, `TRUE`, `NULL`
#' @param ... Arguments passed to [stringr::str_sort()]
#'
#' @md
#' @noRd
#' @keywords internal
#' @examples
#' list_packages()
#' list_packages("installed")
#'
list_packages <- function(which = c("loaded", "installed"), decreasing = FALSE, ...) {
  which <- match.arg(which, several.ok = FALSE)

  pkgs <- switch(
    which,
    "loaded"    = (.packages(all.available = FALSE)),
    "installed" = (.packages(all.available = TRUE)),
    stop("Unknown option of `which` (type of packages): ", which)
  )

  if (!is.null(decreasing)) {
    str_sort(pkgs, decreasing = decreasing, ...)

  } else {
    pkgs
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get number of datasets in a package
#'
#' @param package String with valid name of inst6alled package
#'
#' @return Number with number of datasets in a package
#'
#' @md
#' @noRd
#'
#' @examples
#' get_n_datasets("datasets")
#' get_n_datasets("purrr")

get_n_datasets <- function(package) {
  ds_in_pkg <- data(package = package)$results
  nrow(ds_in_pkg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get either dimensions or length of a dataset
#'
#' @param obj An R object (dataset).
#'
#' @return A string wiht dimensions of matrix-like or array-like oblect and
#'         length of other objects.
#' @md
#' @noRd
#'
#' @examples
#' get_obj_dims(iris)
#'
get_obj_dims <- function(obj, x_symbol = " \u00D7 ") {
  dim_obj <- dim(obj)

  if (is.null(dim_obj)) {
    as.character(length(obj))
  } else {
    stringr::str_c(dim_obj, collapse = x_symbol) # "\u00D7" - times symbol
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get information about dataset
#'
#' @param str sting with datasets name, e.g, "datasets::iris"
#'
#' @return Dataframe with size, class and some other information about a dataset.
#'
#' @md
#' @noRd
#'
#' @examples
#' get_ds_info("datasets::iris")

get_ds_info <- function(str) {

  # ds <- eval(parse(text = str))
  ds <- get_ds_data(str)

  data.frame(
    size      = get_obj_dims(ds),
    is_df     = is.data.frame(ds),
    is_matrix = is.matrix(ds),
    is_list   = is.list(ds),
    class     = str_c(class(ds), collapse = ", "),

    stringsAsFactors = FALSE
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get data of a dataset in R package
#'
#' @param str sting with datasets name, e.g,
#'     `"iris"` (if package with data is loaded) or `"datasets::iris"`.
#'
#' @return The contents of the dataset.
#'
#' @md
#' @noRd
#'
#' @examples
#' get_ds_data("datasets::BOD")
#'
#' get_ds_data("BOD")
#'
#' str <- "datasets::BOD"
#' get_ds_data(str)

get_ds_data <- function(str) {
  ds <- try(eval(parse(text = str)), silent = TRUE)

  # If dataset is not experted
  if (inherits(ds, "try-error")) {
    tmp_envir <- new.env()

    if (isTRUE(stringr::str_detect(str, ":{2,3}"))) {

      names <- stringr::str_split(str, ":{2,3}", 2)[[1]]
      ds_name <- data(list = names[2], package = names[1], envir = tmp_envir)

    } else {
      ds_name <- data(list = str, envir = tmp_envir)
    }

    ds <- tmp_envir[[ds_name]]
  }

  # Return:
  ds
}



#' Get information about dataset and its variables
#'
#' @param str sting with datasets name, e.g, "datasets::iris"
#'
#' @return Dataframe with size and variable type frequency
#'
#' @md
#' @noRd
#'
#' @examples
#' str <- "datasets::iris"
#' get_ds_info_2("datasets::iris")
#' get_ds_info_2("BOD")
#'


get_ds_info_2 <- function(str) {

  ds <- get_ds_data(str)

  ds_size = get_obj_dims(ds)

  if (is.data.frame(ds)) {
    n_variables = ncol(ds)
    n_numeric   = sum(purrr::map_int(ds, is.numeric))
    n_factor    = sum(purrr::map_int(ds, is.factor))
    n_logical   = sum(purrr::map_int(ds, is.logical))
    n_character = sum(purrr::map_int(ds, is.character))
    n_other     = n_variables - n_character - n_logical - n_factor - n_numeric


  } else {
    n_variables = n_numeric = n_factor = n_logical = n_character =
      n_other   = n_variables = NA
  }

  tibble::tibble(
    size = ds_size,
    is_data_frame = is.data.frame(ds),
    # n_vars = n_variables,
    n_num   = n_numeric ,
    n_fct   = n_factor  ,
    n_lgl   = n_logical ,
    n_chr   = n_character,
    n_other = n_other,
    class   = str_c(class(ds), collapse = ", ")
  )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param package
#'
#' @md
#' @noRd
#'
#' @examples
#' get_info_about_datasets("ggplot2")

get_info_about_datasets <- function(package = NULL) {

  # library(tidyverse)
  #
  # package  <- "ggplot2"
  # all_pkgs <- FALSE
  #
  # pkgs <- setdiff(.packages(all.available = all_pkgs), c("base", "stats"))
  # package <- pkgs
  #
  # # package <- "purrr"
  # package   <- "ggplot2"

  res <-
    package %>%
    purrr::map_dfr(~tibble::as_tibble(data(package = .)$results)) %>%
    dplyr::select(-LibPath) %>%
    dplyr::mutate(
      # removes unnecessary information
      Item = stringr::str_trim(stringr::str_replace(Item, " .*$", "")),
      pkg_ds = stringr::str_c(Package, "::", Item),
      code_to_load = stringr::str_glue('data({Item}, package = "{Package}")'),
      info = purrr::map(pkg_ds, ~purrr::safely(get_ds_info_2)(.)$result)
    ) %>%
    tidyr::unnest(info) %>%
    dplyr::rename(Dataset = Item) %>%
    dplyr::select(-pkg_ds, -code_to_load, dplyr::everything(), code_to_load) %>%
    dplyr::arrange(Package, Dataset)

  # If no datasets are present
  if (nrow(res) < 1) {
    res <- tibble::tibble(
      "Package"       = character(),
      "Dataset"       = character(),
      "Title"         = character(),
      "size"          = character(),
      "is_data_frame" = logical(),
      "n_num"         = integer(),
      "n_fct"         = integer(),
      "n_lgl"         = integer(),
      "n_chr"         = integer(),
      "n_other"       = integer(),
      "class"         = character(),
      # "pkg_ds"        = character(),
      "code_to_load"  = character()
    )

  }

  res

  # View(res)


  # Issue:
  # (data(BJsales.lead, package = "datasets"))

}

list_datasets_in_package <- function(package) {
  data(package = package)$results[ ,"Item"]
}

get_ds_info_as_sring <- function(str) {
  str %>%
    get_ds_info_2() %>%
    knitr::kable(format = "pandoc") %>%
    str_c(collapse = "\n")
}


# pkgs <- c( "sandwich", "datasets")
# get_ds_list(pkgs)
get_ds_list <- function(pkgs) {
  pkgs %>%
    purrr::map_dfr(~tibble::as_tibble(data(package = .)$results)) %>%
    dplyr::mutate(Item = stringr::str_trim(stringr::str_replace(Item, " .*$", ""))) %>%
    dplyr::pull(Item)
}
