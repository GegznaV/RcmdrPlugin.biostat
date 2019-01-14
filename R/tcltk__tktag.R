# ~ TCL/TK tags ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add tk text tags to rows that match pattern

# @param str (character vector) strings to check.
# @param pattern (string) Pattern to search for.
# @param obj Tcl/Tk object.
# @param tag (string) Tag to add.

tktag_add_row  <- function(obj, pattern, tag) {

    str <- stringr::str_split(get_values(obj), "\n")[[1]]

    # Find row
    info_row <- str_which(str, pattern)
    if (length(info_row) == 0)
        return()

    # Indices
    pos_start <- str_glue("{info_row}.0")
    pos_end   <- str_glue("{info_row}.0 + 1 line")

    # Set tags
    for (i in 1:length(info_row))
        tktag.add(obj, tag, pos_start[i], pos_end[i])
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add tk text tags to places that match pattern
tktag_add <- function(obj, tag, pattern, row_ind = NULL, all = TRUE) {
    str <- stringr::str_split(get_values(obj), "\n")[[1]]

    if (!is.null(row_ind)) {
        str <- str[row_ind]
    }

    if (isTRUE(all)) {
        mat <-
            str_locate_all(str, pattern) %>%
            setNames(seq_along(.)) %>%
            discard(~nrow(.) < 1) %>%
            map(as.data.frame) %>%
            dplyr::bind_rows(.id = "row")

    } else {
        mat <-
            str_locate(str, pattern) %>%
            as.data.frame() %>%
            dplyr::mutate(row = dplyr::row_number()) %>%
            dplyr::filter(!is.na(start))
    }

    if (nrow(mat) == 0)
        return()

    pos <-
        mat %>%
        dplyr::mutate(start = start - 1) %>%
        dplyr::transmute(
            start = str_glue("{row}.{start}"),
            end   = str_glue("{row}.{end}"))

    # Set tags
    for (i in 1:nrow(pos))
        tktag.add(obj, tag, pos$start[i], pos$end[i])
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add tk text tags to places that match pattern
# First occurance in each row only.
tktag_add_first <- function(obj, pattern, tag, row_ind = NULL) {
    tktag_add(obj = obj, pattern = pattern, tag = tag,
              row_ind = row_ind, all = FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add tags to highlight tab symbols in input window
highlight_input_tabs <- function() {
    tktag_add_obj(f3_input$text, "\t", tag = "tab")
}