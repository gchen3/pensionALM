library(dplyr)
library(ggplot2)
library(tidyverse)

# Build a simple Asset Liability Model that simulates the evoluation of assets and liabilities over 30 years
# Inputs are investment returns generated from another ESG model for 30 years
# Assumptions include discount rate, salary growth rate, inflation rate, initial benefit payments, assets, and liabilities
# Funding policies include the following: 20 year open level dollar amortization

# Set assumptions
salary_growth <- 0.04
benefit_factor <- 0.02
discount_rate <- 0.07
COLA <- 0.02
inflation_rate <- 0.02
amort_period <- 30

# set the number of simulation years
n <- 30

# Calculate annuity factor for amortization over 30 years
growth_annuity <- (1 - ((1 + salary_growth) / (1 + discount_rate))^amort_period) / (discount_rate - salary_growth)
print(growth_annuity)

# Create the initial empty dataframe
alm <- data.frame(
  year = 1:n,
  asset = rep(0, n),
  liability = rep(0, n),
  normal_cost = rep(0, n),
  payroll = rep(0, n),
  benefit_payment = rep(0, n),
  contribution = rep(0, n),
  investment_returns = rep(0, n),
  investment_return_rate = rep(0.07, n),
  discount_rate = rep(discount_rate, n)
  )

# Input the investment return rates
alm$investment_return_rates <-

# Set the initial scenario (year 0) for assets, liabilities, and benefit payments
alm$liability[1] <- 100
alm$asset[1] <- 79
alm$payroll[1] <- 19             #19% of actuarial liabilities
alm$benefit_payment[1] <- 5.13   #27% of payroll
alm$normal_cost[1] <- 2.28       #12% of payroll
alm$contribution[1] <- alm$normal_cost[1] + (alm$liability[1]-alm$asset[1])/growth_annuity
alm$investment_returns[1] <- alm$asset[1]*0.07

# set return scenario assumptions met
# alm$investment_return <- rnorm(nrow(alm), mean = 0.07, sd = 0.14)
alm$investment_return <- 0.07

# Calculate assets, liabilities, contributions in 30 years
for (i in 2:nrow(alm)) {
  alm$payroll[i] <-  alm$payroll[i-1] * (1+salary_growth)
  alm$benefit_payment[i] <-alm$benefit_payment[i-1] * (1+salary_growth)
  alm$normal_cost[i] <-  alm$normal_cost[i-1] * (1+salary_growth)
  alm$liability[i] <-  alm$liability[i-1] * (1 + discount_rate) + alm$normal_cost[i] - alm$benefit_payment[i]
  alm$investment_returns[i] <- alm$asset[i-1] * alm$investment_return_rate[i]
  alm$contribution[i] <- alm$normal_cost[i] + (alm$liability[i-1]  - alm$asset[i-1])/growth_annuity
  alm$asset[i] <- alm$asset[i-1] + alm$contribution[i] + alm$investment_returns[i] - alm$benefit_payment[i]
  }

alm <- alm %>%
  mutate(
  normal_cost_rate = normal_cost/payroll,
  contribution_rate = contribution/payroll,
  funded_ratio = asset/liability)

ggplot(alm, aes(x=year, y=funded_ratio)) + 
  geom_line() +
  ylim(0, 1)

# Risk measures #
# Adapting from penmod #
source("penmod.R")

res <- penmod(nsims=10000, nyears = 50)

# Using simulation method to calculate VaR_loss, VaR_erc, VaR_uaal at 95% and 99% over t time period
# Note: Monte Carlo simulation method for VaR: Under the Monte Carlo method, Value at Risk is calculated by randomly 
# creating a number of scenarios for future rates to estimate the change in value for each scenario, 
# and then calculating the VaR according to the worst losses.

# Worst (alpha percentile) asset loss at the end of year 5
VaR_loss <- function(alpha, t, res=res) {
  res_cut <- res |> filter(year==t)
  assets_tail = quantile(res_cut$assets, (1-alpha))
  VaR_loss = assets_tail - res$assets[1]
  VaR_loss_per = VaR_loss/res$assets[1]
  return(VaR_loss_per=VaR_loss_per)
  }

VaR_loss(0.95, 2, res)

VaR_erc <- function(alpha, t) {
  res_cut <- res |> filter(year==t)
  erc_tail = quantile(res_cut$contrib, alpha)
  VaR_erc_jump = erc_tail - res$contrib[1]
  VaR_erc_jump_per =  VaR_erc_jump/res$contrib[1]
  return(list(erc_tail, VaR_erc_jump, VaR_erc_jump_per=VaR_erc_jump_per))
}

VaR_erc(0.95, 10)

VaR_uaal <- function(alpha, t) {
  res_cut <- res |> filter(year==t)
  VaR_fr = quantile(res_cut$fr, (1-alpha))
  VaR_uaal = 1 - VaR_fr
  return(list(VaR_fr=VaR_fr, VaR_uaal=VaR_uaal))
}

VaR_uaal(0.95, 10)

# Other risk metrics
# Probability of funded ratio falling below x% at the end of t years
prob_fr <- function(x, t) {
  res_cut <- res |> filter(year==t)
  n_below = sum(res_cut$fr < x)
  n_percent = n_below/nrow(res_cut)
  return(n_percent=n_percent)
 }

prob_fr(0.8, 5)

# Probability of contribution rate higher than x% of payroll at the end of t years
# See CalPERS "Probability of Employer Contribution Rates Exceeding Given Level (at any point in next 30 years)"

prob_erc <- function(x, t) {
  res_cut <- res |> filter(year==t) |> mutate(erc_rate=contrib/payroll)
  n_above = sum(res_cut$erc_rate > x)
  n_percent = n_above/nrow(res_cut)
  return(n_percent=n_percent)
}

prob_erc(0.30, 4)

