#
# if (is_nothing_to_import()) {
#     clear_dataset_window()
#     return()
# }
#
# input       <- get_input_by_mode()
#
# # Get data from input
# suppressWarnings({
#     ds_contents <- try(
#         do_fread(str_c(input, collapse =  "\n")),
#         silent = TRUE)
# })
#
# widget <- f3_dataset
#
# preview_type     <- get_selection(f3_box_type)
# nrow_preview_ds  <- get_nrows_preview_ds()
# expect_more_rows <- possibly_more_rows()
#
#
# refresh_dataset_window_0(widget, ds_contents, preview_type, nrow_preview_ds, expect_more_rows)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update contents of dataset preview window
#
# Function that updates contents in dataset preview window.
# For use in data import menus.
#
# @param widget Object of class `bs_text`
# @param ds_contents A 'data.frame' object.
# @param preview_type The type op dataset's preview.
#                     One of "Tibble", "Data table", "Structure".
# @param nrow_preview_ds Integer or Inf. Number of rows to preview.
# @param expect_more_rows (logical) Flag indicating if more rows are expected
# in the file than there are in `ds_contents`.
# @param err_msg_default (character) String with default error message to be
# displayed in dataset's preview window.


refresh_dataset_window_0 <- function(
    widget, ds_contents, preview_type, nrow_preview_ds, expect_more_rows = FALSE,
    err_msg_default = NULL) {

    # Check arguments
    preview_type <- match.arg(preview_type,
                              choices = c("Tibble", "Data table", "Structure"))

    # `widget` should be object of class `bs_text`

    text_widget <- widget$text

    # Functions:
    write_to_widgets_window <-  function(contents, ...) {
        set_values(text_widget, values = contents, ...)
    }

    # Format fread error for display
    parse_file_read_error <- function(err) {
        err %>%
            str_replace("Error in .*\n", "") %>%
            str_replace("(does not exist)", "\n\\1") %>%
            str_replace("\\. ", ".\n") %>%
            str_trim()
    }

    # Default text
    txt_trunc   <- "... Other rows are not shown ..."
    txt_not_all <- "... More rows may be present in the file ..."

    if (is.null(err_msg_default)) {
        err_msg_default <- str_c(
            "Possible reasons:\n",
            " - file name is incorrent or missing;\n",
            " - incorrect file format;\n",
            " - file is empty;\n",
            " - import options are incorrect.")
    }

    # Patterns
    pattern_num <- "(?<=\\<)(num|int|dbl)(?=\\>)"
    pattern_chr <- "(?<=\\<)cha?r(?=\\>)"

    # Check errors
    err_msg <- NULL

    if (inherits(ds_contents, "try-error")) {
        err_msg <- parse_file_read_error(ds_contents)

    } else {

        nrows_from_file <- nrow(ds_contents)

        # Get contents to preview
        switch(
            preview_type,
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Tibble" = {

                op <- options(width = 10000)
                ds_preview <-
                    capture.output(
                        print(tibble::as_tibble(ds_contents),
                              width = Inf,
                              n = nrow_preview_ds)
                    ) %>%
                    str_subset("^(?!# A tibble.*)") %>%
                    str_replace( "^# \\.\\.\\. with.*", txt_trunc)

                options(op)

                if (expect_more_rows) {
                    ds_preview <- c(ds_preview, txt_not_all)
                }

            },
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Data table" = {

                topn <-
                    if (is.infinite(nrow_preview_ds)) {
                        nrows_from_file
                    } else {
                        floor(nrow_preview_ds/2)
                    }

                op <- options(width = 10000)
                ds_preview <-
                    capture.output(
                        print(data.table::as.data.table(ds_contents),
                              col.names = "top",
                              class = TRUE,
                              topn  = topn,
                              nrows = nrows_from_file)
                    )
                options(op)

                if (expect_more_rows) {
                    ds_preview[length(ds_preview)] <- txt_not_all
                }

            },
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Structure" = {
                ds_preview <- capture.output(glimpse(ds_contents, width = 74))

                if (expect_more_rows)  {
                    ds_preview <- str_replace(
                        ds_preview,
                        "(?<=^Observations: )(.*)", "\\1 or more")
                }
            }
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(ds_preview) <= 1) {err_msg <- err_msg_default}
    }

    if (!is.null(err_msg)) {
        # If no preview available:
        write_to_widgets_window(str_c("Error! \n\n", err_msg))

        # Red font:
        tktag.add(text_widget, "bold",  "1.0", "2.0")
        tktag.add(text_widget, "error", "1.0", "end")

    } else {
        # Write contents:
        write_to_widgets_window(str_c(ds_preview, collapse = "\n"))

        # Format contents:

        # Info messages
        tktag_add_row(text_widget, txt_trunc,   tag = "info")
        tktag_add_row(text_widget, txt_not_all, tag = "info")

        switch(
            preview_type,
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Tibble" = ,
            "Data table" = {
                # Variable names
                tktag.add(text_widget, "var_names", "1.0", "2.0")

                # Variable types
                tktag.add(text_widget, "var_types", "2.0", "3.0")
                tktag_add(text_widget, pattern_chr, 1:2, tag =  "chr")
                tktag_add(text_widget, pattern_num, 1:2, tag =  "num")

                # Separator
                tktag_add_row(text_widget, "^\\s*---\\s*$", "red")
            },

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Structure" = {
                # Variable names
                tktag_add_first(text_widget, "(?<=\\$).*?(?=\\<)", "var_names")

                # Variable types
                tktag_add_first(text_widget, "^\\$",       "var_types")
                tktag_add_first(text_widget, "\\.\\.\\.$", "var_types")
                tktag_add_first(text_widget, "\\<.*?\\>",  "var_types")
                tktag_add_first(text_widget, pattern_chr,  "chr")
                tktag_add_first(text_widget, pattern_num,  "num")

                # Observations
                tktag_add_row(text_widget, "^Observations: \\d+ or more", "grey")
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        )
    }
}
