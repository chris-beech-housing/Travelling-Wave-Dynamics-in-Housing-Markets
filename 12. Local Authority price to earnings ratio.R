# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(readxl)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv")

# 'House price to workplace-based earnings ratio' data from:
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian

price_earnings_ratio <-
  read_excel(
    "Data/aff1ratioofhousepricetoworkplacebasedearnings2024.xlsx",
    sheet = "5c",
    skip = 1
  ) |>
  select(
    `Local Authority` = `Local authority name`,
    `Price-to-earnings ratio (2024)` = `2024`
  ) |>
  filter(`Local Authority` != "Isles of Scilly") |>
  mutate(
    `Price-to-earnings ratio (2024)` = as.numeric(
      `Price-to-earnings ratio (2024)`
    )
  )

## Re-order the Local Authorities using the 2024 median; use levels(price_earnings_ratio$`Local Authority`) to see the effect

median_prices <-
  price_paid_data |>
  filter(`Date of Transfer` >= "2024-01-01" & `Date of Transfer` < "2025-01-01") |>
  group_by(`Local Authority`) |>
  summarise(median_price = median(Price_inc_Stamp_Duty))

ordered_local_authorities <-
  with(median_prices, reorder(`Local Authority`, median_price))

price_earnings_ratio$`Local Authority` <-
  factor(
    price_earnings_ratio$`Local Authority`,
    levels = levels(ordered_local_authorities)
  )

## Plot the price-to-earnings ratio per Local Authority

price_earnings_ratio$`Local Authority` <-
  as.numeric(price_earnings_ratio$`Local Authority`) # Convert to a number, else theme_minimal() doesn't work!

price_earnings_ratio |>
  ggplot(aes(
    x = `Price-to-earnings ratio (2024)`,
    y = `Local Authority`,
  )) +
  geom_point(colour = "#A3A533") +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(breaks = seq(0, 36, by = 2)) +
  # labs(title = "Price-to-earnings ratio (2024), by Local Authority") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )
