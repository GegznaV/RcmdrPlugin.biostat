# ~ TCL/TK tags ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add tk text tags to rows that match pattern

# @param str (character vector) strings to check.
# @param pattern (string) Pattern to search for.
# @param obj Tcl/Tk object.
# @param tag (string) Tag to add.

tktag_add_row  <- function(str, pattern, obj, tag) {
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
tktag_add <- function(str, pattern, obj, tag, all = TRUE) {
    # Find row
    # info_row <-

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

# Add tk text tags to places that match pattern
# obj - Tcl/Tk text object
tktag_add_obj <- function(obj, tag, pattern, all = TRUE) {

    str <- get_values(obj) %>% str_split("\n") %>% .[[1]]

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
tktag_add_first <- function(str, pattern, obj, tag) {
    tktag_add(str, pattern, obj, tag, all = FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add tags to highlight tab symbols in input window
highlight_input_tabs <- function() {
    tktag_add_obj(f3_input$text, tag = "tab", "\t")
}