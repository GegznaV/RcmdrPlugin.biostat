# ~ Tcl/Tk text tags ---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name add-tag
#' @title Add tags to Tk text widget.
#' @description
#' Add TK text tags to Tk text widgets:
#' `tktag_add()` adds tags to the places of text that match a pattern.
#' `tktag_add_first()` *in each row*, adds tags to the *first* occurance that
#'  matches a pattern.
#' `tktag_add_row()` adds text tags to the **rows** that match a pattern.
#'
#' @param obj Tk text widget (Tk object).
#' @param tag (string) The namo of a tag to add.
#' @param pattern (string) Text pattern to add text at.
#' @param row_ind (integer) Row indices.
#' @param all (logical) If `TRUE`, adds tag to all matches, if `FALSE`, adds tag
#'        to the first match only.
#'
#' @seealso
#' [tcltk::tktag.add()]
#'
#' @md
#' @export
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add-tag
#' @export
tktag_add_row  <- function(obj, pattern, tag) {

  str <- stringr::str_split(get_values(obj), "\n")[[1]]

  # Find row
  info_row <- str_which(str, pattern)
  if (length(info_row) == 0)
    return()

  # Indices
  pos_start <- stringr::str_glue("{info_row}.0")
  pos_end   <- stringr::str_glue("{info_row}.0 + 1 line")

  # Set tags
  for (i in 1:length(info_row))
    tcltk::tktag.add(obj, tag, pos_start[i], pos_end[i])
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname add-tag
#' @export
tktag_add <- function(obj, tag, pattern, row_ind = NULL, all = TRUE) {

  str <- stringr::str_split(get_values(obj), "\n")[[1]]

  if (!is.null(row_ind)) {
    str <- str[row_ind]
  }

  if (isTRUE(all)) {
    mat <-
      stringr::str_locate_all(str, pattern) %>%
      setNames(seq_along(.)) %>%
      purrr::discard(~nrow(.) < 1) %>%
      purrr::map(as.data.frame) %>%
      dplyr::bind_rows(.id = "row")

  } else {
    mat <-
      stringr::str_locate(str, pattern) %>%
      as.data.frame() %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::filter(!is.na(start))
  }

  if (nrow(mat) == 0) {
    return()
  }

  pos <-
    mat %>%
    dplyr::mutate(start = start - 1) %>%
    dplyr::transmute(
      start = stringr::str_glue("{row}.{start}"),
      end   = stringr::str_glue("{row}.{end}")
    )

  # Set tags
  for (i in 1:nrow(pos)) {
    tcltk::tktag.add(obj, tag, pos$start[i], pos$end[i])
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname add-tag
#' @export
tktag_add_first <- function(obj, pattern, tag, row_ind = NULL) {

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tcltk::tktag_add(obj = obj, pattern = pattern, tag = tag, row_ind = row_ind,
    all = FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

