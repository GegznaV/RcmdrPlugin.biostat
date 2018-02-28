library(DescTools)

# DescTools::BinomCI(x = 16, n = 200, method = "modified wilson")

# x <- c(35, 74, 22, 69)
# DescTools::MultinomCI(x, method = "goodman")
#
# x <- c(35, 39, 32, 34, 40, 38)
# DescTools::MultinomCI(x, method = "sisonglaz")


# x <- 1:10
# DescTools::MeanCI(x, method = "boot", type = "bca")
#
# x <- 1:10
# DescTools::MedianCI(x, method = "boot", type = "bca")
#
#
# set.seed(123456)
# x <- 1:10
# y <- rnorm(10)
# DescTools::MeanDiffCI(x, y, method = "bca")

# set.seed(123456)
# x <- 1:10
# y <- rnorm(10)
# DescTools::BootCI(x, y, FUN = cor, bci.method = "bca")

SignTest()
