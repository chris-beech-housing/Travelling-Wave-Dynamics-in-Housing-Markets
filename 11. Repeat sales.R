# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(janitor)
library(ggrepel)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Import data

# CPI data: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7bt/mm23
cpi <-
  read_csv("Data/series-201125.csv") |>
  rename(Date = Title, Index = `CPI INDEX 00: ALL ITEMS 2015=100`) |>
  filter(str_detect(
    Date,
    c("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")
  )) |>
  mutate(
    Date = floor_date(as_date(ym(Date)), "month"),
    Index = as.numeric(Index)
  )

# Price paid data pre-filtered for at least one repeat sale
price_paid_data <-
  read_csv("Data/pp-repeat.csv")

## Pairs of repeat sales

# Process repeat sales for a specific pair of years
analyse_pair_repeat_sales <- function(data, year1, year2, cpi = NULL) {
  # Select only properties with sales in both specified years
  result <- data |>
    filter(year(`Date of Transfer`) %in% c(year1, year2)) |>
    group_by(Postcode, PAON, SAON, Street) |>
    filter(n_distinct(year(`Date of Transfer`)) == 2) |> # Must have sales in both years
    summarise(
      # First sale (year1)
      date_first = first(`Date of Transfer`[year(`Date of Transfer`) == year1]), # this approach only takes the first sale in a given year
      price_first = first(Price[year(`Date of Transfer`) == year1]),
      quantile_first = first(quantile[year(`Date of Transfer`) == year1]),

      # Second sale (year2)
      date_second = first(`Date of Transfer`[
        year(`Date of Transfer`) == year2
      ]), # this approach only takes the first sale in a given year
      price_second = first(Price[year(`Date of Transfer`) == year2]),
      quantile_second = first(quantile[year(`Date of Transfer`) == year2]),

      # Keep Local Authority
      `Local Authority` = first(`Local Authority`),

      .groups = "drop"
    ) |>
    mutate(
      # Add year information
      year_first = year1,
      year_second = year2,
      year_pair = paste0(year1, "-", year2),

      # Calculate price ratio
      price_ratio = price_second / price_first
    )

  # Add CPI calculations if CPI data is provided
  result <- result |>
    mutate(
      # Get CPI values at the dates of sales
      cpi_first = approx(
        cpi$Date,
        cpi$Index,
        xout = date_first,
        rule = 2
      )$y,
      cpi_second = approx(
        cpi$Date,
        cpi$Index,
        xout = date_second,
        rule = 2
      )$y,

      # Calculate CPI multiplier
      cpi_multiplier = cpi_second / cpi_first,

      # Calculate CPI-adjusted price and differences
      cpi_adjusted_price = price_first * cpi_multiplier,
      difference_from_cpi = price_second - cpi_adjusted_price,
      percent_difference = (price_second / cpi_adjusted_price - 1) * 100
    )

  return(result)
}

# Function to generate and process all year combinations
generate_pair_combinations <- function(
  data,
  start_year = 1995,
  end_year = 2025,
  cpi = cpi
) {
  # Generate all possible year combinations
  years <- start_year:end_year
  combinations <- expand.grid(year1 = years, year2 = years)

  # Keep only combinations where year2 > year1
  combinations <- combinations[combinations$year2 > combinations$year1, ]

  # Initialise empty list to store results
  results_list <- list()

  # Process each combination
  for (i in 1:nrow(combinations)) {
    year1 <- combinations$year1[i]
    year2 <- combinations$year2[i]

    # Calculate and store result
    cat("Processing years:", year1, "to", year2, "\n")
    result <- analyse_pair_repeat_sales(data, year1, year2, cpi)

    if (!is.null(result) && nrow(result) > 0) {
      results_list[[paste0(year1, "_", year2)]] <- result
    }
  }

  # Combine all results
  if (length(results_list) > 0) {
    combined_results <- bind_rows(results_list)
    return(combined_results)
  } else {
    cat("No valid results found for any year combination.\n")
    return(NULL)
  }
}

# Generate and process all year combinations
all_repeat_sales <-
  generate_pair_combinations(
    price_paid_data |> filter(quantile <= 10), # Filter based on quantile for quicker analysis
    start_year = 1995,
    end_year = 2025,
    cpi = cpi
  )

# Repeat sales sold in 2009
pair_sales_in_2009 <-
  all_repeat_sales |>
  filter(quantile_first <= 5) |>
  filter(abs(quantile_first - quantile_second) <= 4) |>
  filter(abs(percent_difference) <= 5) |>
  filter(year_first < 2006 & year_second == 2009) |>
  arrange(price_second) |>
  select(-c(Postcode, PAON, SAON, Street)) |>
  select(`Local Authority`, everything())

# Plot data
pair_sales_in_2009 |>
  ggplot(aes(x = cpi_adjusted_price, y = price_second)) +
  geom_abline(
    slope = 1, # Reference line where CPI-adjusted price equals 2009 price
    intercept = 0,
    linetype = "dashed",
    colour = "#AFAFAF"
  ) +
  geom_point(aes(colour = percent_difference)) +
  geom_text_repel(
    data = pair_sales_in_2009,
    aes(x = cpi_adjusted_price, y = price_second, label = year_first),
    size = 3
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
    # title = "Repeat sales: sales prior to 2006 CPI-adjusted to 2009 versus 2009 sales prices",
    x = "Sales price adjusted by CPI to 2009 (£)",
    y = "2009 sales price (£)",
    colour = "Percentage difference from CPI-adjusted sales price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))

## Triplets of repeat sales

# Process repeat sales for a specific trio of years
analyse_triple_repeat_sales <- function(data, year1, year2, year3, cpi = NULL) {
  # Select only properties with sales in all three specified years
  result <- data |>
    filter(year(`Date of Transfer`) %in% c(year1, year2, year3)) |>
    group_by(Postcode, PAON, SAON, Street) |>
    filter(n_distinct(year(`Date of Transfer`)) == 3) |> # Must have sales in all three years
    summarise(
      # First sale (year1)
      date_first = first(`Date of Transfer`[year(`Date of Transfer`) == year1]), # this approach only takes the first sale in a given year
      price_first = first(Price[year(`Date of Transfer`) == year1]),
      quantile_first = first(quantile[year(`Date of Transfer`) == year1]),

      # Second sale (year2)
      date_second = first(`Date of Transfer`[
        year(`Date of Transfer`) == year2
      ]), # this approach only takes the first sale in a given year
      price_second = first(Price[year(`Date of Transfer`) == year2]),
      quantile_second = first(quantile[year(`Date of Transfer`) == year2]),

      # Third sale (year3)
      date_third = first(`Date of Transfer`[year(`Date of Transfer`) == year3]), # this approach only takes the first sale in a given year
      price_third = first(Price[year(`Date of Transfer`) == year3]),
      quantile_third = first(quantile[year(`Date of Transfer`) == year3]),

      # Keep Local Authority
      `Local Authority` = first(`Local Authority`),

      .groups = "drop"
    ) |>
    mutate(
      # Add year information
      year_first = year1,
      year_second = year2,
      year_third = year3,
      year_trio = paste0(year1, "-", year2, "-", year3),
      year_label = paste0(year1, "-", year2),

      # Calculate price ratios
      price_ratio_1_2 = price_second / price_first,
      price_ratio_2_3 = price_third / price_second,
      price_ratio_1_3 = price_third / price_first,
    )

  # Add CPI calculations if CPI data is provided
  if (!is.null(cpi)) {
    result <- result |>
      mutate(
        # Get CPI values at the dates of sales
        cpi_first = approx(
          cpi$Date,
          cpi$Index,
          xout = date_first,
          rule = 2
        )$y,
        cpi_second = approx(
          cpi$Date,
          cpi$Index,
          xout = date_second,
          rule = 2
        )$y,
        cpi_third = approx(
          cpi$Date,
          cpi$Index,
          xout = date_third,
          rule = 2
        )$y,

        # Calculate CPI multipliers
        cpi_multiplier_1_2 = cpi_second / cpi_first,
        cpi_multiplier_2_3 = cpi_third / cpi_second,
        cpi_multiplier_1_3 = cpi_third / cpi_first,

        # Calculate CPI-adjusted prices and differences
        # First to second period
        cpi_adjusted_price_1_2 = price_first * cpi_multiplier_1_2,
        difference_from_cpi_1_2 = price_second - cpi_adjusted_price_1_2,
        percent_difference_1_2 = (price_second / cpi_adjusted_price_1_2 - 1) *
          100,

        # Second to third period
        cpi_adjusted_price_2_3 = price_second * cpi_multiplier_2_3,
        difference_from_cpi_2_3 = price_third - cpi_adjusted_price_2_3,
        percent_difference_2_3 = (price_third / cpi_adjusted_price_2_3 - 1) *
          100,

        # First to third period (overall)
        cpi_adjusted_price_1_3 = price_first * cpi_multiplier_1_3,
        difference_from_cpi_1_3 = price_third - cpi_adjusted_price_1_3,
        percent_difference_1_3 = (price_third / cpi_adjusted_price_1_3 - 1) *
          100
      )
  }

  return(result)
}

# Function to generate all possible year triplets
generate_triplet_combinations <- function(
  data,
  start_year = 1995,
  end_year = 2009,
  cpi = NULL
) {
  # Generate all possible year triplets
  years <- start_year:end_year
  year_triplets <- expand.grid(year1 = years, year2 = years, year3 = years)

  # Keep only combinations where year1 < year2 < year3 and with minimum gaps
  year_triplets <- year_triplets[
    year_triplets$year1 < year_triplets$year2 &
      year_triplets$year2 < year_triplets$year3,
  ]

  # Initialise empty list to store results
  results_list <- list()

  # Process each combination
  for (i in 1:nrow(year_triplets)) {
    year1 <- year_triplets$year1[i]
    year2 <- year_triplets$year2[i]
    year3 <- year_triplets$year3[i]

    # Calculate and store result
    cat("Processing years:", year1, "to", year2, "to", year3, "\n")
    result <- analyse_triple_repeat_sales(data, year1, year2, year3, cpi)

    if (!is.null(result) && nrow(result) > 0) {
      results_list[[paste0(year1, "_", year2, "_", year3)]] <- result
    }
  }

  # Combine all results
  if (length(results_list) > 0) {
    combined_results <- bind_rows(results_list)
    return(combined_results)
  } else {
    cat("No valid results found for any year combination.\n")
    return(NULL)
  }
}

# Generate all possible triplets
all_triple_repeat_sales <-
  generate_triplet_combinations(
    price_paid_data |> filter(quantile <= 10), # Filter based on quantile for quicker analysis
    start_year = 1995,
    end_year = 2009,
    cpi = cpi
  )

# Compare CPI-adjusted first sale to third sale where the second sale is higher than either
triplet_sales_in_2009 <-
  all_triple_repeat_sales |>
  filter(year_third == 2009) |>
  filter(price_second > price_first & price_third < price_second) |>
  filter(price_third <= 40000) |>
  filter(quantile_first <= 5) |>
  filter(abs(quantile_first - quantile_third) <= 4) |>
  filter(abs(percent_difference_1_3) <= 5) |>
  arrange(price_third) |>
  select(-c(Postcode, PAON, SAON, Street)) |>
  select(`Local Authority`, everything())

# Plot data
triplet_sales_in_2009 |>
  ggplot(aes(x = cpi_adjusted_price_1_3, y = price_third)) +
  geom_abline(
    slope = 1, # Reference line where CPI-adjusted price equals actual price
    intercept = 0,
    linetype = "dashed",
    colour = "#AFAFAF"
  ) +
  geom_point(aes(colour = percent_difference_1_3)) +
  geom_text_repel(
    data = triplet_sales_in_2009,
    aes(
      x = cpi_adjusted_price_1_3,
      y = price_third,
      label = year_label
    ),
    size = 3
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
    # title = "Repeat sales: multiple sales with first year CPI-adjusted to 2009 versus 2009 sales prices",
    x = "First year sales price adjusted by CPI to 2009 (£)",
    y = "2009 sales price (£)",
    colour = "Percentage difference from CPI-adjusted sales price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))
