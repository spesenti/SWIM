## ---- setup, echo=FALSE, cache=FALSE------------------------------------------
options(digits = 2) # auto round to 2 decimals when printed

## ----wrap-hook, echo = FALSE--------------------------------------------------
library(knitr)
hook_error = knit_hooks$get('error')
knit_hooks$set(error = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_error(x, options)
})

## ---- example1_sim_data, include = TRUE---------------------------------------
set.seed(0)
# number of simulated scenarios
n.sim <- 10 ^ 5
# correlation between Z1 and Z2
r <- 0.5
# simulation of Z1  and Z2
# constructed as a combination of independent standard normals U1, U2
U1 <- rnorm(n.sim)
U2 <- rnorm(n.sim)
Z1 <- 100 + 40 * U1
Z2 <- 100 + 20 * (r * U1 + sqrt(1 - r ^ 2) * U2)
# simulation of Z3
Z3 <- rnorm(n.sim, 100, 20)
# portfolio loss Y
Y <- Z1 + Z2 + Z3
# data of baseline model
dat <- data.frame(Z1, Z2, Z3, Y)

## ---- example1_first_stress, echo = -3, warning = FALSE, message = FALSE------
library(SWIM)
str.mean <- stress(type = "mean", x = dat, k = 1, new_means = 110)
options(digits = 2)
summary(str.mean, base = TRUE)

## ---- example1-cdfs-mean, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%', fig.cap = "Baseline and stressed empirical distribution functions of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the mean of $Z_1$."----
# refer to variable of interest by name...
plot_cdf(str.mean, xCol = "Z1", base = TRUE)
# ... or column number
plot_cdf(str.mean, xCol = 4, base = TRUE)

## ---- example1-weights-mean, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%',fig.cap = "Scenario weights against observations of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the mean of $Z_1$."----
# parameter n specifies the number of scenario weights plotted
plot_weights(str.mean, xCol = "Z1", n = 1000)
# specifying the limits of the x-axis
plot_weights(str.mean, xCol = "Y", x_limits = c(90, 550), n = 1000)

## ---- example1_second_stress, cache = TRUE, echo = -2, warning = FALSE, message = FALSE----
str.sd <- stress(type = "mean sd", x = dat, k = 1, new_means = 100, new_sd = 50)
options(digits = 2)
summary(str.sd, base = FALSE)

## ---- example1-cdfs-sd, cache = FALSE, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%', fig.cap = "Baseline and stressed empirical distribution functions of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the standard deviation of $Z_1$."----
plot_cdf(str.sd, xCol = "Z1", base = TRUE)
plot_cdf(str.sd, xCol = 4, base = TRUE)

## ---- example1-weights-sd, cache = TRUE, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%',fig.cap = "Scenario weights against observations of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the standard deviation of $Z_1$."----
plot_weights(str.sd, xCol = "Z1", n = 2000)
plot_weights(str.sd, xCol = "Y", n = 2000)

## ---- example1_third_stress, cache = FALSE, error=TRUE, linewidth = 80--------
stress(type = "mean", x = dat, k = 1, new_means = 300)
max(Z1)

## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------
require(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=70),tidy=TRUE)

## ---- loading-packages, cache = FALSE, include = FALSE------------------------
  library(SWIM)
  library(ggplot2)
  library(ggpubr)

## ---- CM-data-head, echo = -2, cache = TRUE-----------------------------------
data("credit_data")
options(digits = 3)
head(credit_data)

## ---- CM-stress-VaR, cache = FALSE, echo = TRUE, linewidth = 55---------------
stress.credit <- stress(type = "VaR", x = credit_data, k = "L", alpha = 0.9, q_ratio = 1.2)

## ---- CM-stress-VaR-check-ES, cache = FALSE, linewidth = 70-------------------
VaR_stressed(object = stress.credit, alpha =  c(0.75, 0.9, 0.95, 0.99), xCol = "L", wCol = 1, base = TRUE)
ES_stressed(object = stress.credit, alpha = 0.9, xCol = "L", wCol = 1, base = TRUE)

## ---- CM-stress-VaR-ES, cache = FALSE, linewidth = 60-------------------------
stress.credit <- stress(type = "VaR ES", x = stress.credit, k = "L", alpha = 0.9, q_ratio = 1.2, s = 3500)

## ---- CM-summary, echo = -1, cache = FALSE------------------------------------
options(digits = 3)
summary(stress.credit, base = TRUE)

## ---- CM-specs, echo = -1, cache = FALSE--------------------------------------
options(digits = 3)
get_specs(stress.credit)

