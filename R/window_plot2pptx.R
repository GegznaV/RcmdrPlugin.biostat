# TODO:
# Make Tcl/Tk interface for this function
#
window_plot2pptx <- function() {



    # Examples ---------------------------------------------------------------
    library(tidyverse)

    gg <-
        ggplot(data = CO2,
               aes(x = conc, y = uptake, color = treatment)) +
        geom_point()

    fig_code <- 'with(CO2, Hist(uptake, scale="frequency", breaks="Sturges", col="darkgray"))'
    gg_obj_name <- "gg"

    # Defaults ---------------------------------------------------------------
    pptx_open  <- 'NULL'
    pptx_save  <- str_glue('"{Sys.Date()} fig.pptx"')
    file_open  <- TRUE
    input_type <- "code"

    pos_top    <- 0
    pos_left   <- 0
    pos_width  <- 7
    pos_height <- 5

    # Computations based on selected values ----------------------------------

    if (isTRUE(file_open)) {
        txt_file_open <- "%>% \n browseURL()"
    }

    switch(input_type,
           "code" = {
               fig_input <- str_glue(
                   "code = {{      \n",
                   "    {fig_code} \n",
                   "}}") %>%
                   style_cmd()
           },

           "ggobj" = {
               fig_input <- str_glue('ggobj = {gg_obj_name}')
           }
    )

    # Main code to evaluate --------------------------------------------------
    Library(tidyverse)
    Library(officer)
    Library(rvg)

    str_glue(
        '{pptx_open} %>% \n',
        'officer::read_pptx() %>%  \n',
        'officer::add_slide(layout = "Blank", master = "Office Theme") %>% \n',
        'rvg::ph_with_vg_at(       \n',
        '    {fig_input},          \n',
        '    top    = {pos_top},   \n',
        '    left   = {pos_left},  \n',
        '    width  = {pos_width}, \n',
        '    height = {pos_height} \n',
        ') %>%  \n',
        'print(target = pptx_save)',
        '{txt_file_open}') %>%
        style_cmd()


    tkfocus(CommanderWindow())

}