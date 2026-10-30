## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.width=5--------------------------------------------------------------
library(jumps)
set.seed(2025)
n <- 100

# simulated smooth trend
mu <- 100*cos(3*pi/n*(1:n)) - ((1:n) > 50)*n - c(rep(0, 50), 1:50)*10
# simulated time series
y <- mu + rnorm(n, sd = 20)

# HP filter with jumps with estimated lambda and fixed penalty (maxsum = 50)
hpj_sim <- hpj(y, maxsum = 50)

print(hpj_sim)

plot(hpj_sim)
plot(hpj_sim, prob = 0.95)
plot(hpj_sim, use_ggplot = FALSE)

## ----fig.width=5--------------------------------------------------------------
# HP filter with jumps with estimated lambda and automatically selected penalty
hpj_nile <- hpj(Nile)

print(hpj_nile)

plot(hpj_nile, main = "Nile river flow")
plot(hpj_nile, prob = 0.95, main = "Nile river flow")
plot(hpj_nile, use_ggplot = FALSE, main = "Nile river flow")

## ----warning=FALSE, message=FALSE, fig.width=5--------------------------------
data("employed_IT")
y <- window(employed_IT[, "Y25.29"], start = c(2009, 1))
hpj_emp <- hpj(y, scl = "original")

print(hpj_emp)
plot(hpj_emp, main = "Millions of employed in Italy: age 25-29")

## -----------------------------------------------------------------------------
g1 <- function(x) {
  besselJ(20 * x, nu = 1) + x * (x >= 0.3 & x <= 0.4) - x * (x >= 0.6)
}
true_disc <- c(0.3, 0.4, 0.6)

set.seed(2026)
n <- 100
x <- sort(runif(n))
y <- g1(x) + rnorm(n, sd = 0.05)

## -----------------------------------------------------------------------------
fit_mle <- auto_ssj_mle(y = y, x = x, ic = "ebic")
fit_mle

## -----------------------------------------------------------------------------
fit_atw <- auto_ssj_atw_cv(y = y, x = x, ic = "ebic")
fit_atw

## -----------------------------------------------------------------------------
c(lambda_mle = fit_mle$pars[["lambda"]],
  lambda_cv  = fit_atw$pars[["lambda"]])

## ----fig.width=6, fig.height=6------------------------------------------------
plot(fit_mle, xlab = "x", ylab = "y")

## ----fig.width=6, fig.height=6------------------------------------------------
plot(fit_atw, xlab = "x", ylab = "y")

## ----fig.width=6, fig.height=4------------------------------------------------
plot(x, y, pch = 16, cex = 0.6, col = "grey55", xlab = "x", ylab = "y")
lines(x, g1(x), col = "grey20", lty = 2)
lines(fit_atw$x, fit_atw$smoothed_level, col = "firebrick", lwd = 2)
abline(v = ssj_breaks(fit_atw)$x, lty = 3, col = "firebrick")
legend("bottomleft", bty = "n", cex = 0.85,
       lty = c(2, 1), lwd = c(1, 2), col = c("grey20", "firebrick"),
       legend = c("true signal", "smoothing spline with jumps"))

## -----------------------------------------------------------------------------
data(demand_temp)
str(demand_temp)

## -----------------------------------------------------------------------------
fit <- auto_ssj_atw(y = demand_temp$Load, x = demand_temp$Temperature,
                    lambda = 254.3, ic = "ebic")
fit

## -----------------------------------------------------------------------------
ssj_breaks(fit)

## ----fig.width=6, fig.height=6------------------------------------------------
plot(fit, xlab = "Temperature (Celsius)", ylab = "Demand (MWh)")

## -----------------------------------------------------------------------------
# robust noise scale, immune to the jumps themselves
sig <- mad(diff(demand_temp$Load[order(demand_temp$Temperature)])) / sqrt(2)

fit_cssd <- ssj_cssd_dp(demand_temp$Load, demand_temp$Temperature,
                           p = 0.9, gamma = 10, delta = sig)
fit_cssd

## ----fig.width=6, fig.height=4------------------------------------------------
plot(fit_cssd, xlab = "Temperature (Celsius)", ylab = "Demand (MWh)")

## -----------------------------------------------------------------------------
predict_cssd_dp(fit_cssd, xnew = c(20, 26.9, 27.1, 30))

