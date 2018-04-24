# "Confidence interval" menu functions ============================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_mean <- function() {

    # Example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logger(paste0(
        "# # An example of code for confidence intervals of mean:\n",
        "# \n",
        '# x <- 1:10\n',
        '# DescTools::MeanCI(x, method = "boot", type = "bca", na.rm = TRUE)\n',
        "\n"
    ))

    return()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    function_not_implemented("Function for confidence intervals of mean")
    return()

    Library("tidyverse")
    Library("biostat")

    conf_level = 0.95
    R = 2000

    set.seed(123456)

    iris %>%
        group_by(Species)  %>%
        do(ci_mean_boot(.$Sepal.Width,
                        conf_level = conf_level,
                        repetitions = R))  %>%
        ungroup()  %>%
        format_numbers(digits = c(NA, 2, 2, 2, 2, NA))  %>%
        pander::pander()


    iris %>%
        group_by(Species)  %>%
        do(ci_mean_t(.$Sepal.Width, conf_level = conf_level))  %>%
        ungroup()  %>%
        format_numbers(digits = c(NA, 2, 2, 2, 2))  %>%
        pander::pander()



    doItAndPrint(glue::glue("biostat::ci_mean_boot({ActiveDataSet()}${Numeric()[1]}, conf_level = {conf_level}, na.rm = TRUE)"))

    doItAndPrint(glue::glue("biostat::ci_mean_t({ActiveDataSet()}${Numeric()[1]}, conf_level = {conf_level}, na.rm = TRUE)"))


    function_not_implemented()

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_mean_diff <- function() {

    # Example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logger(paste0(
        "# # An example of code for confidence intervals of\n",
        "# # difference in means of two groups:\n",
        "# \n",
        "# set.seed(123456)\n",
        '# x <- 1:10\n',
        "# y <- rnorm(10)\n",
        '# DescTools::MeanDiffCI(x, y, method = "bca", na.rm = TRUE)\n',
        "\n"
    ))

    return()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_median <- function() {

    # Example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logger(paste0(
        "# # An example of code for confidence intervals of median:\n",
        "# \n",
        '# x <- 1:10\n',
        '# DescTools::MedianCI(x, method = "boot", type = "bca",  na.rm = TRUE)\n',
        "\n"
    ))

    return()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_prop_multi <- function() {
    # Example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logger(paste0(
        "# # An example of code for confidence intervals\n",
        "# # of multinomial proportion:\n",
        "# \n",
        '# x <- c(35, 74, 22, 69)\n',
        '# DescTools::MultinomCI(x, method = "goodman")\n',
        "# \n",
        '# x <- c(35, 39, 32, 34, 40, 38)\n',
        '# DescTools::MultinomCI(x, method = "sisonglaz")\n',
        "\n"
    ))

    return()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_prop_binom <- function() {

    # Example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logger(paste0(
        "# # An example of code for confidence intervals\n",
        "# # of binomial proportion:\n",
        "# \n",
        '# DescTools::BinomCI(x = 16, n = 200, method = "modified wilson")\n',
        "\n"


    ))

    return()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    function_not_implemented("Function for proportion confidence intervals")


    Library("tidyverse")
    Library("binom")
    # doItAndPrint('library("tidyverse")')

    conf_level = 0.95

    available_methods <- c("all", "wilson", "exact", "asymptotic", "ac", "prop.test", "bayes", "logit", "cloglog", "probit")
    names_of_methods  <- c("All", "Wilson", "Pearson-Klopper (exact)", "asymptotic", "Agresti-Coull", "prop.test", "bayes", "logit", "cloglog", "probit")


    binom::binom.confint(x = c(2, 4),
                         n = 100,
                         conf.level = conf_level,
                         methods = "wilson") %>%
        dplyr::rename(proportion = mean)  %>%
        dplyr::select(proportion, lower, upper)

    function_not_implemented()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_boot <- function() {

    # Example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logger(paste0(
        "# # An example of code for bootstrap confidence intervals\n",
        "# # (in the example, CI of Pearson correlation coefficient):\n",
        "# \n",
        "# set.seed(123456)\n",
        "# x <- 1:10\n",
        "# y <- rnorm(10)\n",
        '# DescTools::BootCI(x, y, FUN = cor, bci.method = "bca")\n',
        "\n"

    ))

    return()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    function_not_implemented("Function for bootstrap confidence intervals")


    Library("tidyverse")
    Library("boot")
    # doItAndPrint('library("tidyverse")')

    conf_level = 0.95

    # strata <- eval_glue("{activeDataSet()}[['{Factors()[1]}']]")
         x <- eval_glue("{activeDataSet()}[['{Numeric()[1]}']]")

    statistic <- function(x, i) {
        mean(x[i])
    }

    simulation <- c("ordinary",
                    "balanced",
                    "permutation",
                    "parametric",
                    "antithetic")[1]


    set.seed(169854)

    boot_obj <- boot(data = x,
                     statistic = statistic,
                     R = 5000,
                     # strata = strata,
                     sim = simulation
                     )

    plot(boot_obj)

    boot_ci_type <- c("bca", "basic", "perc", "norm")[1]

    boot.ci(boot_obj, conf = conf_level, type = boot_ci_type)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


