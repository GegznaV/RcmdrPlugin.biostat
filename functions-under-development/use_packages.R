# Library

use_packages <- function(package, pos = length(search()), rmd = TRUE) {


    # pattern <- "^package:"
    #
    # loaded <- search() %>%
    #     stringr::str_subset(pattern) %>%
    #     stringr::str_remove(pattern)


    s <- paste(glue("library({package})"), collapse = "  \n")


    # loaded <- search()
    # loaded <- loaded[grep("^package:", loaded)]
    # loaded <- sub("^package:", "", loaded)
    # if (!getRcmdr("suppress.X11.warnings")) {
    #     messages.connection <- file(open = "w+")
    #     sink(messages.connection, type = "message")
    #     on.exit({
    #         sink(type = "message")
    #         close(messages.connection)
    #     })
    # }
    # if (!(package %in% loaded)) {
    #     for (pkg in dependencies[[package]]) {
    #         Library(pkg, pos = pos, rmd = rmd)
    #     }
    #     command <- glue("library({package}, pos={pos})")
    #     logger(command, rmd = rmd)
    #     result <- try(eval(parse(text = command), envir = .GlobalEnv),
    #                   silent = TRUE)
    #     if (class(result)[1] == "try-error") {
    #         Message(message = paste(strsplit(result, ":")[[1]][2]),
    #                 type = "error")
    #         tkfocus(CommanderWindow())
    #         return("error")
    #     }
    #     return(package)
    # }
    # else return(invisible(NULL))
}