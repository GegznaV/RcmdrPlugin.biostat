# #'  Define a Macro Command
# #'
# #'   Function was copied from \pkg{Rcmdr} with this comment in \pkg{Rcmdr}:
# #'      'the following function is slightly modified, with permission,
# #'       from Thomas Lumley,"Programmer's Niche: Macros in R," R-News,
# #'       Sept. 2001, Vol. 1, No. 3, pp.11-13.'
# #'
# #' @param ... Named arguments for the extression.
# #' @param expr The expression.

# #  Functions to build dialog boxes, etc.
#
# defmacro <- function(..., expr) {
#     expr <- substitute(expr)
#     len <- length(expr)
#     expr[3:(len + 1)] <- expr[2:len]
#     ## delete "macro" variables starting in ..
#     expr[[2]] <- quote(on.exit(remove(list = objects(pattern = "^\\.\\.",
#                                                      all.names = TRUE))))
#     a <- substitute(list(...))[-1]
#     ## process the argument list
#     nn <- names(a)
#     if (is.null(nn)) nn <- rep("", length(a))
#     for (i in seq(length.out = length(a))) {
#         if (nn[i] == "") {
#             nn[i]  <- paste(a[[i]])
#             msg    <- paste(a[[i]], gettext("not supplied", domain = "R-Rcmdr"))
#             a[[i]] <- substitute(stop(foo), list(foo = msg))
#         }
#     }
#     names(a) <- nn
#     a <- as.list(a)
#     ff <- eval(substitute(
#         function() {
#             tmp <- substitute(body)
#             eval(tmp, parent.frame())
#         },
#         list(body = expr)
#     ))
#     ## add the argument list
#     formals(ff) <- a
#     ## create a fake source attribute
#     mm <- match.call()
#     mm$expr <- NULL
#     mm[[1]] <- as.name("macro")
#     expr[[2]] <- NULL # get "local" variable removal out of source
#     attr(ff, "source") <- c(deparse(mm), deparse(expr))
#     ## return the macro
#     ff
# }