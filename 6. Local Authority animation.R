# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(plotly)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # or pp-stampduty-psqm

# Add a little bit of noise to the price_paid_data to ensure repeated values don't stop the median being calculated (not necessary for pp-stampduty-psqm)
price_paid_data$Price_inc_Stamp_Duty <-
  price_paid_data$Price_inc_Stamp_Duty +
  rnorm(nrow(price_paid_data), mean = 0, sd = 1)

## Calculate median, log return

yearly_median <-
  price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "year")) |>
  group_by(`Local Authority`, Date) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) |> # or Price_per_inc_Stamp_Duty
  mutate(log_return = log(median_price / lag(median_price, n = 1))) |>
  ungroup()

## Re-order the Local Authorities using the 2025 median; use levels(monthly_median$`Local Authority`) to see the effect

median_prices <-
  price_paid_data |>
  filter(`Date of Transfer` >= "2025-01-01") |>
  group_by(`Local Authority`) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) # or Price_per_inc_Stamp_Duty

ordered_local_authorities <-
  with(median_prices, reorder(`Local Authority`, median_price))

yearly_median$`Local Authority` <-
  factor(
    yearly_median$`Local Authority`,
    levels = levels(ordered_local_authorities)
  )

## Animation of log_return, by Local Authority over time

yearly_median |>
  filter(!is.na(log_return)) |>
  plot_ly(
    x = ~`log_return`,
    y = ~`Local Authority`,
    frame = ~`Date`,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'text',
    text = ~`Local Authority`,
    marker = list(color = "#A3A533")
  ) |>
  layout(
    title = list(
      text = "Yearly log return, by Local Authority",
      font = list(family = "Helvetica", size = 20),
      x = 0.0475
    ),
    margin = list(t = 33),
    xaxis = list(title = "Log return"),
    yaxis = list(title = "Local Authority", ticks = "", showticklabels = FALSE),
    showlegend = FALSE
  ) |>
  animation_opts(
    1000,
    easing = 'linear',
    redraw = FALSE
  ) |>
  animation_slider(
    currentvalue = list(prefix = "Date: ")
  ) |>
  animation_button(
    label = "Play",
    x = 1,
    xanchor = "right",
    y = 0,
    yanchor = "bottom"
  )
