# "Confidence interval" menu functions ============================================

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_ci_mean <- function() {
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



    doItAndPrint(glue::glue("biostat::ci_mean_boot({ActiveDataSet()}${Numeric()[1]}, conf_level = {conf_level})"))

    doItAndPrint(glue::glue("biostat::ci_mean_t({ActiveDataSet()}${Numeric()[1]}, conf_level = {conf_level})"))


    function_not_implemented()

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_ci_prop <- function() {
    function_not_implemented("Function for proportion confidence intervals")
    return()


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
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_ci_boot <- function() {
    function_not_implemented("Function for bootstrap confidence intervals")
    return()


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


