# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(fitdistrplus)
library(scales)
library(ggrepel)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # or pp-stampduty-psqm

## Create year, log_price, and empirical CDF

price_paid_data <-
  price_paid_data |>
  mutate(
    Year = year(`Date of Transfer`),
    log_price = log(Price_inc_Stamp_Duty) # or Price_per_inc_Stamp_Duty
  ) |>
  arrange(Year, log_price) |> 
  group_by(Year) |>
  mutate(ecdf = row_number(log_price) / n()) |>
  ungroup()

## ecdf

price_paid_data |>
  ggplot(aes(x = log_price, y = ecdf, colour = factor(Year))) +
  geom_line() +
#  facet_wrap(~Year, ncol = 4, scales = "free") +
  labs(
    # title = "Empirical cumulative distribution function (cumulative log_price), by year",
    x = "Log price",
    y = "Cumulative proportion",
    colour = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

## pdf

price_paid_data |>
  filter(Price_inc_Stamp_Duty < exp(14.5)) |> # or Price_per_inc_Stamp_Duty
  ggplot(aes(x = Price_inc_Stamp_Duty)) + # or Price_per_inc_Stamp_Duty
  geom_histogram(bins = 1000) +
  facet_wrap(~Year, ncol = 4, scales = "free") +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e3) +
  labs(
    # title = "Price pdf (histogram), by year",
    x = "Price (millions)",
    y = "Count (thousands)"
  ) +
  theme_minimal()

price_paid_data |>
  filter(log_price > 8 & log_price < 18) |>
  ggplot(aes(x = log_price)) +
  geom_histogram(bins = 1000) +
  facet_wrap(~Year, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Log price pdf (histogram), by year",
    x = "Log price",
    y = "Count"
  ) +
  theme_minimal()

price_paid_data |>
  filter(Price_inc_Stamp_Duty < exp(14.5)) |> # or Price_per_inc_Stamp_Duty
  ggplot(aes(x = Price_inc_Stamp_Duty)) + # or Price_per_inc_Stamp_Duty
  geom_density() +
  facet_wrap(~Year, ncol = 4, scales = "free") +
  scale_x_continuous(labels = \(x) x / 1e6) +
  labs(
    # title = "Price pdf (density), by year",
    x = "Price (millions)",
    y = "Count"
  ) +
  theme_minimal()

price_paid_data |>
  filter(log_price > 8 & log_price < 18) |>
  ggplot(aes(x = log_price)) +
  geom_density() +
  facet_wrap(~Year, ncol = 4, scales = "free") +
  labs(
    # title = "Log price pdf (density), by year",
    x = "Log price",
    y = "Count"
  ) +
  theme_minimal()

## Calculate log price pdf by year

cullen_frey <- function(price_paid_data) {
  # Helper function to calculate moments
  calculate_moments <- function(data, column) {
    moments <-
      data |>
      group_by(Year) |>
      group_split() |>
      map(\(x) descdist(x[[column]], graph = FALSE))

    skewness <- map_dbl(moments, "skewness")
    skewness_squared <- skewness^2
    kurtosis <- map_dbl(moments, "kurtosis")

    data.frame(
      skewness_squared = skewness_squared,
      kurtosis = kurtosis,
      Year = unique(data$Year)
    )
  }

  # Calculate moments for log price
  price_data <- calculate_moments(price_paid_data, "log_price")

  # Distribution data frames, algorithms taken from: https://github.com/cran/fitdistrplus/blob/master/R/descdist.R
  sequence <- seq(-8, 8, 0.01)

  gamma_df <- data.frame(
    skewness_squared = 4 / exp(sequence),
    kurtosis = 3 + 6 / exp(sequence)
  )

  lognormal_df <- data.frame(
    skewness_squared = (exp(exp(sequence)^2) + 2)^2 *
      (exp(exp(sequence)^2) - 1),
    kurtosis = exp(exp(sequence)^2)^4 +
      2 * exp(exp(sequence)^2)^3 +
      3 * exp(exp(sequence)^2)^2 -
      3
  )

  normal_df <- data.frame(
    skewness_squared = 0,
    kurtosis = 3
  )

  logistic_df <- data.frame(
    skewness_squared = 0,
    kurtosis = 4.2
  )

  # Define category labels to match the alphabetical assignment of colours in the other Cullen and Frey plots
  categories <- c("Gamma", "Local Authority", "Logistic", "Lognormal", "Normal")

  # Generate colours for all 5 categories
  default_hue <- hue_pal()(length(categories))

  # Assign colours
  custom_colours <- setNames(default_hue, categories)

  # Save "Local Authority" colour and remove it from the list
  year_colour <- custom_colours["Local Authority"]
  custom_colours <- custom_colours[names(custom_colours) != "Local Authority"]

  # Assign "Year" to "Local Authority"'s colour
  custom_colours["Year"] <- year_colour

  # Base Cullen and Frey plot
  cullen_frey_ggplot <-
    ggplot() +
    geom_line(
      data = gamma_df,
      aes(x = skewness_squared, y = kurtosis, colour = "Gamma")
    ) +
    geom_line(
      data = lognormal_df,
      aes(x = skewness_squared, y = kurtosis, colour = "Lognormal")
    ) +
    geom_point(
      data = normal_df,
      aes(x = skewness_squared, y = kurtosis, colour = "Normal", size = 1)
    ) +
    geom_point(
      data = logistic_df,
      aes(x = skewness_squared, y = kurtosis, colour = "Logistic", size = 1)
    ) +
    scale_y_reverse() +
    labs(x = "Square of skewness", y = "Kurtosis", colour = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(size = "none")

  # This shows that the log price distributions are closer to logistic than normal
  log_price_plot <-
    cullen_frey_ggplot +
    geom_point(
      data = price_data,
      aes(x = skewness_squared, y = kurtosis, colour = "Year")
    ) +
    geom_text_repel(
      data = price_data,
      aes(x = skewness_squared, y = kurtosis, label = Year),
      size = 3
    ) +
    coord_cartesian(xlim = c(0, 0.25), ylim = c(7.5, 0)) +
    scale_colour_manual(
      values = custom_colours,
      breaks = c("Normal", "Logistic", "Gamma", "Lognormal", "Year")
    )
  # +
  #   labs(title = "Log price pdf kurtosis and square of skewness, by year")

  return(log_price_plot)
}

cullen_frey(price_paid_data)
