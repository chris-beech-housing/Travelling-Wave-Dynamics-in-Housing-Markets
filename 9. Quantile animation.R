# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(plotly)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # or pp-stampduty-psqm

## Calculate median, log return

yearly_median <-
  price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "year")) |>
  group_by(Date) |>
  mutate(quantile = ntile(Price_inc_Stamp_Duty, 317)) |> # or Price_per_inc_Stamp_Duty
  ungroup() |>
  group_by(quantile, Date) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) |> # or Price_per_inc_Stamp_Duty
  mutate(log_return = log(median_price / lag(median_price, n = 1))) |>
  ungroup()

## Animation of log_return, by quantile over time

yearly_median |>
  filter(!is.na(log_return)) |>
  plot_ly(
    x = ~`log_return`,
    y = ~quantile,
    frame = ~`Date`,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'text',
    text = ~quantile,
    marker = list(color = "#A3A533")
  ) |>
  layout(
    title = list(
      text = "Yearly log return, by quantile",
      font = list(family = "Helvetica", size = 20),
      x = 0.0475
    ),
    margin = list(t = 33),
    xaxis = list(title = "Log return"),
    yaxis = list(title = "Quantile", ticks = "", showticklabels = FALSE),
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
