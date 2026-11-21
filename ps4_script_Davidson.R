library(tidyverse)
library(fpp3)
library(BVAR)


# 2.1a

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(
    Month = yearmonth(observation_date),
    sales = MRTSSM4453USN
  ) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)



# 2.1b

# Graph 1: Monthly sales over time
liquor |>
  autoplot(sales) +
  labs(
    title = "U.S. Beer, Wine, and Liquor Store Sales",
    x = "Year",
    y = "Sales (Millions of Dollars)"
  )

# Graph 2: Seasonal pattern of log sales
liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Pattern of Liquor Store Sales",
    x = "Month",
    y = "Log of Sales"
  )


# 2.2a

liquor_fit <- liquor |>
  model(
    arima = ARIMA(log(sales)),
    ets   = ETS(log(sales))
  )

# ARIMA model
liquor_fit |>
  select(arima) |>
  report()

# ETS model
liquor_fit |>
  select(ets) |>
  report()


# 2.2b

# Plot residuals
liquor_fit |>
  select(arima) |>
  gg_tsresiduals()

# Ljung Box test at lag 24
liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)


# 2.3a

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(
    snaive = SNAIVE(sales),
    ets    = ETS(log(sales)),
    arima  = ARIMA(log(sales))
  )

# Save fitted models so we don't need to rerun them
saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
