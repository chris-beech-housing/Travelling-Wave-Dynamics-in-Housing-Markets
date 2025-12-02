# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(fpp3)
library(seasonal)
library(fable.prophet)
library(plotly)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # pp-stampduty-psqm doesn't work even when adding the noise below

## Calculate median

medians <-
  price_paid_data |>
  mutate(year_quarter = yearquarter(`Date of Transfer`)) |> # or yearmonth()
  group_by(year_quarter, `Local Authority`) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) |>
  ungroup() |>
  as_tsibble(key = `Local Authority`, index = year_quarter)

# SEATS doesn't seem to like some repeated (median) values in a handful of Local Authorities, giving the error:
# "The covariance matrix of the ARMA parameters is singular; cannot compute t-statistics for the ARMA parameters."
# Hence, add some noise to the values:

medians$median_price <-
  medians$median_price + rnorm(nrow(medians), mean = 0, sd = 1)

## Try various decomposition options to determine which approach is the best

Sample_LA <-
  medians |>
  filter(`Local Authority` == "Camden")

Sample_LA |>
  model(classical_decomposition(median_price, type = "additive")) |>
  components() |>
  autoplot()

Sample_LA |>
  model(classical_decomposition(median_price, type = "multiplicative")) |>
  components() |>
  autoplot()

Sample_LA |>
  model(stl = STL(median_price)) |>
  components() |>
  autoplot()

Sample_LA |>
  model(STL(
    median_price ~
      trend(window = 21) +
        season(window = "periodic"),
    robust = TRUE
  )) |>
  components() |>
  autoplot()

Sample_LA |>
  model(x11 = X_13ARIMA_SEATS(median_price ~ x11())) |> # Use yearquarter() for the function below
  components() |>
  autoplot()

Sample_LA |>
  model(seats = X_13ARIMA_SEATS(median_price ~ seats())) |> # Use yearquarter() for the function below
  components() |>
  autoplot()

Sample_LA |>
  model(
    prophet = prophet(
      median_price ~
        growth("linear") + season("year", 4, type = "multiplicative")
    )
  ) |> # Use yearmonth() for the function below
  components() |>
  autoplot()

## Function to loop through all Local Authorities

time_series_decomposition <- function(local_authority) {
  print(local_authority)

  local_authority_dcmp <-
    medians |>
    filter(`Local Authority` == local_authority) |>
    model(seats = X_13ARIMA_SEATS(median_price ~ seats())) |>
    components()

  return(local_authority_dcmp)
}

local_authorities_dcmp <-
  map(
    as.list(unique(as.character(medians$`Local Authority`))),
    time_series_decomposition
  ) |>
  list_rbind()

local_authorities_dcmp <-
  local_authorities_dcmp |>
  as_tibble() |>
  group_by(`Local Authority`) |>
  mutate(log_trend_return = log(trend / lag(trend, n = 4))) |>
  ungroup() |>
  group_by(year_quarter) |>
  mutate(Rank = row_number(desc(log_trend_return))) |>
  ungroup()

## Re-order the Local Authorities, use levels(local_authorities_dcmp$`Local Authority`) to see the effect

order_by_trend <-
  local_authorities_dcmp |>
  group_by(`Local Authority`) |>
  summarise(mean_trend = mean(trend))

ordered_local_authorities <-
  with(order_by_trend, reorder(`Local Authority`, mean_trend))

local_authorities_dcmp$`Local Authority` <-
  factor(
    local_authorities_dcmp$`Local Authority`,
    levels = levels(ordered_local_authorities)
  )

## Heatmaps

# Reduce the scale slightly, thereby increasingly the contrast
local_authorities_dcmp <-
  local_authorities_dcmp |>
  mutate(
    display_trend_return = case_when(
      log_trend_return > 0.2623643 ~ 0.2623643,
      log_trend_return < -0.2623643 ~ -0.2623643,
      TRUE ~ log_trend_return
    )
  )

local_authorities_dcmp$year_quarter <- as_date(
  local_authorities_dcmp$year_quarter
)

# Plot trend return
local_authorities_dcmp |>
  plot_ly(
    x = ~year_quarter,
    y = ~`Local Authority`,
    z = ~display_trend_return,
    type = "heatmap",
    colorscale = "RdBu"
  ) |>
  colorbar(
    title = "Log return",
    tickvals = c(-0.2623643, -0.1397619, 0, 0.1397619, 0.2623643),
    ticktext = c("-30%", "-15%", "0%", "15%", "30%"),
    orientation = "h",
    thickness = 10,
    x = 0.44675,
    y = -0.05
  ) |>
  layout(
    # title = list(
    #   text = "Annual log return of the SEATS trend decomposition of the quarterly median, by Local Authority",
    #   font = list(family = "Helvetica", size = 20),
    #   x = 0.048
    # ),
    # margin = list(t = 33),
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "Local Authority", ticks = "", showticklabels = FALSE)
  )

# Plot rank
legend_numbers <- seq(50, 300, by = 50) # for Local Authority

local_authorities_dcmp |>
  plot_ly(
    x = ~year_quarter,
    y = ~`Local Authority`,
    z = ~ -Rank,
    type = "heatmap",
    colorscale = "RdBu"
  ) |>
  colorbar(
    title = "Return rank",
    tickvals = -unique(legend_numbers),
    ticktext = unique(legend_numbers),
    orientation = "h",
    thickness = 10,
    x = 0.44675,
    y = -0.05
  ) |>
  layout(
    # title = list(
    #   text = "Rank of the annual return of the SEATS trend decomposition of the quarterly median, by Local Authority",
    #   font = list(family = "Helvetica", size = 20),
    #   x = 0.048
    # ),
    # margin = list(t = 33),
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "Local Authority", ticks = "", showticklabels = FALSE)
  )
