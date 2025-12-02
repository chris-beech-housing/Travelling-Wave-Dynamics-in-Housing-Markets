# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(jsonlite)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Import and convert data
price_paid_data <-
  read_csv("Data/pp-subset.csv") # or pp-psqm

# Convert JSON arrays to lists
convert_json_columns <- function(df) {
  df |>
    mutate(
      thresholds = map(thresholds, fromJSON),
      tax_rates = map(tax_rates, fromJSON)
    )
}

England_Wales_Stamp_Duty <-
  read_csv("Data/England and Wales Stamp Duty.csv") |>
  convert_json_columns()

Wales_only_Stamp_Duty <-
  read_csv("Data/Wales Stamp Duty.csv") |>
  convert_json_columns()

## Create country subsets

# Filter for England and Wales data
eng_wales_data <-
  price_paid_data |>
  filter(
    (Country %in% c("E", "W") & `Date of Transfer` < "2018-04-01") |
      (Country == "E" & `Date of Transfer` >= "2018-04-01")
  )

# Filter for Wales data
wales_data <-
  price_paid_data |>
  filter(Country == "W" & `Date of Transfer` >= "2018-04-01")

## Add the Stamp Duty thresholds and rates

# Join helper
join_stamp_duty <- function(data, stamp_duty_df) {
  left_join(
    data,
    stamp_duty_df,
    by = join_by(between(`Date of Transfer`, start_date, end_date))
  ) |>
    select(-start_date, -end_date)
}

# Apply joins
eng_wales_data <-
  eng_wales_data |>
  join_stamp_duty(England_Wales_Stamp_Duty)

wales_data <-
  wales_data |>
  join_stamp_duty(Wales_only_Stamp_Duty)

# Combine the data
price_paid_data <-
  bind_rows(
    eng_wales_data,
    wales_data
  )

## Tidy up the Environment
rm(eng_wales_data)
rm(England_Wales_Stamp_Duty)

rm(wales_data)
rm(Wales_only_Stamp_Duty)

## Functions

# Calculate the Stamp Duty based on the highest band the price falls into
calculate_stamp_duty_overall <- function(
  property_price,
  thresholds,
  tax_rates
) {
  # Find the appropriate tax rate based on the property price
  tax_rate_index <- max(which(property_price > thresholds))
  tax_rate <- tax_rates[tax_rate_index]

  # Calculate the stamp duty, round down as per HMRC
  stamp_duty <- floor(property_price * tax_rate / 100)

  return(stamp_duty)
}

# Calculate the Stamp Duty based on the portions of the price within different bands
# Ideally we should store the Stamp Duty for each band based on combinations of thresholds and tax_rates, only calculating residuals
calculate_stamp_duty_brackets <- function(
  property_price,
  thresholds,
  tax_rates
) {
  # Calculate the size of each band
  bands <- diff(thresholds)

  # Calculate the stamp duty for each band
  excess <- pmax(property_price - thresholds[-length(thresholds)], 0)
  stamp_duties <- pmin(bands, excess) * tax_rates[-length(tax_rates)] / 100

  # If the property price is greater than the highest threshold, add the excess to the stamp duties
  excess_over_top_threshold <- ifelse(
    property_price > max(thresholds),
    property_price - max(thresholds),
    0
  )
  stamp_duties <- c(
    stamp_duties,
    excess_over_top_threshold * tax_rates[length(tax_rates)] / 100
  )

  # Sum up the stamp duty for all bands, round down as per HMRC
  stamp_duty <- floor(sum(stamp_duties))

  return(stamp_duty)
}

## Main workflow
price_paid_data <-
  price_paid_data |>
  mutate(
    `Stamp Duty` = if_else(
      `Date of Transfer` < "2014-12-03",
      pmap_dbl(
        list(Price, thresholds, tax_rates),
        calculate_stamp_duty_overall
      ),
      pmap_dbl(
        list(Price, thresholds, tax_rates),
        calculate_stamp_duty_brackets
      )
    )
  ) |>
  select(-c(thresholds, tax_rates)) |> 
  mutate(Price_inc_Stamp_Duty = Price + `Stamp Duty`) |> 
  filter(!(is.na(Price_inc_Stamp_Duty)))

## Export
price_paid_data |> 
  select(`Date of Transfer`, `Local Authority`, Price_inc_Stamp_Duty) |>
  write_csv("Data/pp-stampduty.csv")

# Or for the psqm data
price_paid_data |>
  mutate(Price_per_inc_Stamp_Duty = Price_inc_Stamp_Duty / tfarea) |>
  select(`Date of Transfer`, `Local Authority`, Price_per_inc_Stamp_Duty) |>
  write_csv("Data/pp-stampduty-psqm.csv")
