# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(fitdistrplus)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # or pp-stampduty-psqm

# Add a little bit of noise to the price_paid_data to ensure repeated values don't stop the median being calculated (not necessary for pp-stampduty-psqm)
price_paid_data$Price_inc_Stamp_Duty <-
  price_paid_data$Price_inc_Stamp_Duty +
  rnorm(nrow(price_paid_data), mean = 0, sd = 1)

## Sales frequency by month, all Local Authorities

price_paid_data |>
  group_by(
    `Start date` = floor_date(`Date of Transfer`, "month"),
    `Local Authority`
  ) |>
  arrange(`Start date`, `Local Authority`) |>
  summarise(Total = n()) |>
  ggplot(aes(x = Total)) +
  geom_freqpoly(binwidth = 5) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Sales frequency by month, all Local Authorities",
    x = "Sales",
    y = "Count"
  ) +
  theme_minimal()

## Calculate median, log return

monthly_median <-
  price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "month")) |>
  group_by(`Local Authority`, Date) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) |> # or Price_per_inc_Stamp_Duty
  mutate(log_return = log(median_price / lag(median_price, n = 12))) |> # n = 12 for yearly, n = 1 for monthly
  ungroup()

# Using median matches the ONS and is the only sensible approach - see below (the narrative is slightly out of order to allow a single graphing function)
# Other approaches are possible but not recommended (requires grouping by quarter):
#  summarise(mean = exp(map_dbl(list(fitdistr(Price_inc_Stamp_Duty, densfun = "lognormal")), ~.x$estimate["meanlog"]))) # using library(MASS)
#  summarise(mean = exp(map_dbl(list(rightparetolognormal.mle(Price_inc_Stamp_Duty)), ~.x[["coefficients"]][["meanlog"]]))) # using library(distributionsrd)

## Calculate price pdf and log return pdf by Local Authority

cullen_frey <- function(price_paid_data, monthly_median) {
  # Helper function to calculate moments
  calculate_moments <- function(data, column) {
    moments <-
      data |>
      group_by(`Local Authority`) |>
      group_split() |>
      map(\(x) descdist(x[[column]], graph = FALSE))

    skewness <- map_dbl(moments, "skewness")
    skewness_squared <- skewness^2
    kurtosis <- map_dbl(moments, "kurtosis")

    data.frame(skewness_squared = skewness_squared, kurtosis = kurtosis)
  }

  # Calculate moments for price and log return
  price_data <- calculate_moments(price_paid_data, "Price_inc_Stamp_Duty") # or Price_per_inc_Stamp_Duty
  log_return_data <- calculate_moments(monthly_median, "log_return")

  # Distribution data frames, algorithms taken from: https://github.com/cran/fitdistrplus/blob/master/R/descdist.R
  sequence <- seq(-5, 5, 0.01)

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
    scale_colour_discrete(
      breaks = c("Normal", "Logistic", "Gamma", "Lognormal", "Local Authority")
    ) +
    labs(x = "Square of skewness", y = "Kurtosis", colour = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(alpha = "none", size = "none")

  # This shows why the median is the best approach for measuring central tendency as the pdfs have heavy kurtosis and skew
  median_price_plot <-
    cullen_frey_ggplot +
    geom_point(
      data = price_data,
      aes(
        x = skewness_squared,
        y = kurtosis,
        colour = "Local Authority",
        alpha = 0.5
      )
    ) +
    coord_cartesian(xlim = c(0, 40), ylim = c(80, 0)) 
  # +
  #   labs(
  #     title = "Price pdf kurtosis and square of skewness, by Local Authority (2025)" # or (per square metre)
  #   )

  # This shows that mean and standard deviation is the simplest approach to understanding the pdfs of log_return over time
  log_return_plot <-
    cullen_frey_ggplot +
    geom_point(
      data = log_return_data,
      aes(
        x = skewness_squared,
        y = kurtosis,
        colour = "Local Authority",
        alpha = 0.5
      )
    ) +
    coord_cartesian(xlim = c(0, 2), ylim = c(8, 2))
  # +
  #   labs(
  #     title = "Median price yearly log return pdf kurtosis and square of skewness, by Local Authority"
  #   )

  return(list(
    median_price_plot = median_price_plot,
    log_return_plot = log_return_plot
  ))
}

cullen_frey(
  price_paid_data |> filter(`Date of Transfer` >= "2025-01-01"),
  monthly_median |> filter(!is.na(log_return))
)

## Yearly log return of median price, all Local Authorities

monthly_median |>
  ggplot(aes(x = log_return)) +
  geom_histogram(bins = 500) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal() +
  labs(
    # title = "Yearly log return of the median price, all Local Authorities",
    x = "Log return",
    y = "Count"
  )

monthly_median |>
  filter(!is.na(log_return)) |>
  pull(log_return) |>
  descdist()
