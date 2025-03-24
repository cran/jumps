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

