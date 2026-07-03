# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(ggrepel)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Import data

# Repeat sales file; the sales that we are interested in are below the lowest Stamp Duty threshold so we can export from 1. Match to geographies
# price_paid_data |>
#   mutate(Date = floor_date(`Date of Transfer`, "month")) |>
#   group_by(Date) |>
#   mutate(quantile = ntile(Price, 317)) |>
#   ungroup() |>
#   filter(quantile <= 10) |> # filter for sales at the margin
#   select(
#     `Date of Transfer`,
#     Date,
#     Postcode,
#     PAON,
#     SAON,
#     Street,
#     `Local Authority`,
#     Price,
#     quantile
#   ) |>
#   group_by(Postcode, PAON, SAON, Street) |>
#   filter(n() > 1) |>
#   ungroup() |>
#   write_csv("Data/pp-repeat.csv")

end_year <- 2009

repeat_sales <-
  read_csv("Data/pp-repeat.csv") |>
  mutate(year = year(`Date of Transfer`)) |>
  filter(year < end_year + 1)

# CPI data: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7bt/mm23
cpi <-
  read_csv("Data/series-300426.csv") |>
  rename(Date = Title, Index = `CPI INDEX 00: ALL ITEMS 2015=100`) |>
  filter(str_detect(Date, "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) |>
  mutate(
    Date = floor_date(as_date(ym(Date)), "month"),
    Index = as.numeric(Index)
  )

# Helper: interpolate CPI index at a given date vector
cpi_at <- function(dates) {
  approx(cpi$Date, cpi$Index, xout = dates, rule = 2)$y
}

## Anchor pairs and intermediate sales

sales_prior_to <- 2006

# Anchor pairs: every pre-sales_prior_to sale paired with the end_year sale, per property
anchors <-
  repeat_sales |>
  group_by(`Local Authority`, Postcode, PAON, SAON, Street) |>
  filter(any(year == end_year) & any(year < sales_prior_to)) |>
  reframe(
    year_first = year[year < sales_prior_to],
    date_first = `Date of Transfer`[year < sales_prior_to],
    price_first = Price[year < sales_prior_to],
    quantile_first = quantile[year < sales_prior_to],
    date_2009 = first(`Date of Transfer`[year == end_year]),
    price_2009 = first(Price[year == end_year]),
    quantile_2009 = first(quantile[year == end_year])
  ) |>
  mutate(
    cpi_first = cpi_at(date_first),
    cpi_2009 = cpi_at(date_2009),
    cpi_multiplier = cpi_2009 / cpi_first,
    cpi_adjusted_price = price_first * cpi_multiplier,
    percent_difference = (price_2009 / cpi_adjusted_price - 1) * 100
  ) |>
  filter(
    abs(quantile_first - quantile_2009) <= 2,
    abs(percent_difference) <= 5,
    price_2009 <= 45000
  )

# Intermediate sales
intermediates <-
  repeat_sales |>
  semi_join(
    anchors,
    by = c("Local Authority", "Postcode", "PAON", "SAON", "Street")
  ) |>
  inner_join(
    anchors |>
      select(
        `Local Authority`,
        Postcode,
        PAON,
        SAON,
        Street,
        year_first,
        price_first,
        price_2009,
        cpi_adjusted_price
      ),
    by = c("Local Authority", "Postcode", "PAON", "SAON", "Street"),
    relationship = "many-to-many"
  ) |>
  filter(year > year_first, year < end_year) |>
  select(
    `Local Authority`,
    Postcode,
    PAON,
    SAON,
    Street,
    cpi_adjusted_price,
    year_second = year,
    price_second = Price
  )

## Plot data

# Labels on anchors: suppress duplicates at the same plotted position
repeat_sales_plot <-
  anchors |>
  group_by(`Local Authority`, Postcode, PAON, SAON, Street) |>
  mutate(
    label_first = if_else(
      !duplicated(paste(
        year_first,
        round(cpi_adjusted_price),
        round(price_2009)
      )),
      as.character(year_first),
      NA_character_
    )
  ) |>
  ungroup()

# Labels on intermediates: suppress duplicate year labels at the same position
intermediates_plot <-
  intermediates |>
  left_join(
    anchors |>
      select(`Local Authority`, Postcode, PAON, SAON, Street, price_2009),
    by = c("Local Authority", "Postcode", "PAON", "SAON", "Street"),
    relationship = "many-to-many"
  ) |>
  group_by(`Local Authority`, Postcode, PAON, SAON, Street) |>
  mutate(
    label_second = if_else(
      !duplicated(paste(
        year_second,
        round(cpi_adjusted_price),
        round(price_second)
      )),
      as.character(year_second),
      NA_character_
    )
  ) |>
  ungroup()

# Showing the full intermediate sale history as context
repeat_sales_plot |>
  ggplot(aes(x = cpi_adjusted_price, y = price_2009)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "#AFAFAF"
  ) +
  geom_segment(
    data = intermediates_plot,
    aes(xend = cpi_adjusted_price, yend = price_second),
    linewidth = 0.5,
    alpha = 0.4,
    linetype = "solid"
  ) +
  geom_point(
    data = intermediates_plot,
    aes(y = price_second),
    shape = 1,
    size = 2.5,
    colour = "#888888",
    alpha = 0.6
  ) +
  geom_point(aes(colour = percent_difference), size = 3) +
  geom_text_repel(
    aes(label = label_first),
    size = 3,
    nudge_x = 200,
    nudge_y = 150,
    na.rm = TRUE
  ) +
  geom_label_repel(
    data = intermediates_plot,
    aes(y = price_second, label = label_second),
    size = 3,
    nudge_x = 600,
    na.rm = TRUE
  ) +
  scale_colour_gradient(
    low = "#00B0F6",
    high = "#F8766D",
    breaks = c(-4, -2, 0, 2, 4),
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = paste0("Repeat sales: multiple sales with first year CPI-adjusted to ", end_year, " versus ", end_year, " sales prices"),
    x = paste0("First sales price adjusted by CPI to ", end_year, " (£)"),
    y = paste0("Sale price (£)  ●  ", end_year, " sale  ○  Intermediate sale"),
    colour = "Percentage difference from CPI-adjusted sales price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))
