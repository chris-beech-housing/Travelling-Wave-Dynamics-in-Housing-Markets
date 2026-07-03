# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(ggrepel)
library(patchwork)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Import data

end_year <- 2009

repeat_sales <-
  read_csv("Data/pp-repeat.csv") |>
  mutate(year = year(`Date of Transfer`)) |>
  filter(year < end_year + 1)

## Anchor pairs and intermediate sales

sales_from <- 2003
sales_to <- 2008

# Anchor pairs: every pre-sales_to sale paired with the end_year sale, per property
anchors <-
  repeat_sales |>
  filter(`Date of Transfer` >= sales_from) |>
  group_by(`Local Authority`, Postcode, PAON, SAON, Street) |>
  filter(any(year == end_year) & any(year < sales_to)) |>
  reframe(
    year_first = year[year < sales_to],
    date_first = `Date of Transfer`[year < sales_to],
    price_first = Price[year < sales_to],
    quantile_first = quantile[year < sales_to],
    date_2009 = first(`Date of Transfer`[year == end_year]),
    price_2009 = first(Price[year == end_year]),
    quantile_2009 = first(quantile[year == end_year])
  ) |>
  mutate(
    percent_difference = (price_2009 / price_first - 1) * 100
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
        price_2009
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
    price_first,
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
        round(price_first),
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
        round(price_first),
        round(price_second)
      )),
      as.character(year_second),
      NA_character_
    )
  ) |>
  ungroup()

# Showing the full intermediate sale history as context
repeat_sales_plot |>
  ggplot(aes(x = price_first, y = price_2009)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "#AFAFAF"
  ) +
  geom_segment(
    data = intermediates_plot,
    aes(xend = price_first, yend = price_second),
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
    # title = paste0("Repeat sales: multiple sales versus ", end_year, " sales prices"),
    x = "First sales price (£)",
    y = paste0("Sale price (£)  ●  ", end_year, " sale  ○  Intermediate sale"),
    colour = "Percentage difference from first sales price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))

## Per-quantile analysis: peak earlier price vs anchor price

## Per-quantile analysis: peak earlier price vs anchor price

# Anchor pairs (one row per property × pre-sales_to sale)
anchor_pairs <-
  repeat_sales |>
  filter(`Date of Transfer` >= sales_from) |>
  group_by(`Local Authority`, Postcode, PAON, SAON, Street) |>
  filter(any(year == end_year), any(year < sales_to)) |>
  reframe(
    year_first = year[year < sales_to],
    quantile_first = quantile[year < sales_to],
    price_2009 = first(Price[year == end_year]),
    quantile_2009 = first(quantile[year == end_year])
  ) |>
  filter(abs(quantile_first - quantile_2009) <= 1)

# Max intermediate sale price per anchor pair, as % above price_2009
max_above_anchor <-
  anchor_pairs |>
  inner_join(
    repeat_sales |>
      select(`Local Authority`, Postcode, PAON, SAON, Street, year, Price),
    by = c("Local Authority", "Postcode", "PAON", "SAON", "Street"),
    relationship = "many-to-many"
  ) |>
  filter(year > year_first, year < end_year) |>
  group_by(
    `Local Authority`,
    Postcode,
    PAON,
    SAON,
    Street,
    year_first,
    price_2009,
    quantile_2009
  ) |>
  summarise(max_intermediate_price = max(Price), .groups = "drop") |>
  mutate(pct_above_anchor = (max_intermediate_price / price_2009 - 1) * 100)

# Plot helper
plot_pct_above <- function(data, x, x_breaks, y_cap = 125) {
  # Flag and clamp overflow points
  plot_data <- data |>
    mutate(
      capped = pct_above_anchor > y_cap,
      y_plotted = pmin(pct_above_anchor, y_cap)
    )

  # Compute the 90th-percentile line from the original (unclamped) values,
  # so outliers don't silently distort it
  pct90_data <- plot_data |>
    summarise(
      p90 = quantile(pct_above_anchor, 0.90),
      .by = {{ x }}
    )

  ggplot(plot_data, aes(x = {{ x }}, y = y_plotted)) +

    # --- Background scatter ---
    geom_point(
      data = ~ filter(.x, !capped),
      alpha = 0.15,
      size = 1.2,
      colour = "grey50",
      shape = 16
    ) +

    # --- Triangle ---
    geom_point(
      data = ~ filter(.x, capped),
      alpha = 0.6,
      size = 3.5,
      colour = "grey50",
      shape = 17
    ) +

    # --- 90th-percentile line ---
    geom_line(
      data = pct90_data,
      aes(y = p90),
      colour = "#E77D72",
      linewidth = 1
    ) +
    geom_point(
      data = pct90_data,
      aes(y = p90),
      colour = "#E77D72",
      size = 1,
      shape = 19
    ) +

    # Inline label on the right-hand endpoint
    geom_text(
      data = slice_max(pct90_data, {{ x }}, n = 1),
      aes(y = p90, label = "P90"),
      hjust = -0.3,
      colour = "red",
      fontface = "bold",
      size = 3.2
    ) +

    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(NA, y_cap),
      oob = scales::oob_keep
    ) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none")
}

decile_plot <-
  max_above_anchor |>
  mutate(q_bin = ntile(quantile_2009, 10)) |>
  plot_pct_above(q_bin, 1:10)

quantile_plot <-
  max_above_anchor |>
  plot_pct_above(quantile_2009, seq(0, 317, by = 10))

decile_plot / quantile_plot
