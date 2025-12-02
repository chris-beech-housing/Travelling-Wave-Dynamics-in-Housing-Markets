# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Import data

# England and Wales price paid data: https://www.gov.uk/guidance/about-the-price-paid-data

column_names <- c(
  "Transaction unique identifier",
  "Price",
  "Date of Transfer",
  "Postcode",
  "Property Type",
  "Old/New",
  "Duration",
  "PAON",
  "SAON",
  "Street",
  "Locality",
  "Town/City",
  "District",
  "County",
  "PPD Category Type",
  "Record Status"
)

column_types <- cols(
  "Transaction unique identifier" = col_character(),
  "Price" = col_integer(),
  "Date of Transfer" = col_datetime(),
  "Postcode" = col_character(),
  "Property Type" = col_factor(),
  "Old/New" = col_factor(),
  "Duration" = col_factor(),
  "PAON" = col_character(),
  "SAON" = col_character(),
  "Street" = col_character(),
  "Locality" = col_character(),
  "Town/City" = col_character(),
  "District" = col_character(),
  "County" = col_character(),
  "PPD Category Type" = col_factor(),
  "Record Status" = col_factor()
)

price_paid_data <-
  read_csv(
    "Data/pp-complete.csv",
    col_names = column_names,
    col_types = column_types
  ) |>
  select(-`Transaction unique identifier`) |> # there are duplicate rows with identical columns except for the second part of this id
  distinct() |> # remove the duplicates
  filter(`Property Type` != "O") |> # remove 'Other' type
  filter(`PPD Category Type` != "B") |> # remove Additional Price Paid transactions
  #  mutate(`Postcode area` = str_extract(Postcode, '^[a-zA-Z][a-zA-Z]?')) |>
  #  mutate(`Postcode district` = str_sub(Postcode, end = -5)) |>
  #  mutate(`Postcode sector` = str_sub(Postcode, end = -3)) |>
  mutate(`Date of Transfer` = as_date(`Date of Transfer`)) |>
  arrange(Postcode) |> 
  filter(`Date of Transfer` < "2025-10-01")

# Or for the psqm data
price_paid_data <- 
  read_csv("Data/pp-psqm.csv")

# ONS data for Postcode to Local Authority mappings
# https://geoportal.statistics.gov.uk/datasets/3be72478d8454b59bb86ba97b4ee325b/about

ONS <-
  read_csv("Data/ONSPD_AUG_2025_UK.csv") |>
  select(pcds, lad25cd) |>
  arrange(pcds)

# Local Authority names
# https://www.data.gov.uk/dataset/b2c91962-58e7-40f1-ad56-7aa2473a93fd/local-authority-districts-april-2025-names-and-codes-in-the-uk-v21

LA_names <-
  read_csv(
    "Data/Local_Authority_Districts_(April_2025)_Names_and_Codes_in_the_UK_v2.csv"
  ) |>
  select(LAD25CD, LAD25NM)

## Match Postcode to Local Authority code, then name

price_paid_data <-
  price_paid_data |>
  left_join(ONS, by = c("Postcode" = "pcds")) |>
  mutate(Country = str_sub(`lad25cd`, start = 1, end = 1)) |>
  left_join(LA_names, by = c("lad25cd" = "LAD25CD")) |>
  rename(`Local Authority` = LAD25NM) |>
  arrange(`Local Authority`)

## Match County or District to a manually created mapping of Local Authority
missing_mappings <-
  read_csv("Data/Mappings for missing Local Authority.csv")

price_paid_data <-
  price_paid_data |>
  left_join(missing_mappings, by = c("County" = "County or District")) |>
  mutate(
    `Local Authority` = if_else(
      is.na(`Local Authority`),
      LA,
      `Local Authority`
    ),
    Country = if_else(
      is.na(Country),
      `Country code`,
      Country
    )
  ) |>
  select(-LA, -`Country code`) |>
  left_join(missing_mappings, by = c("District" = "County or District")) |>
  mutate(
    `Local Authority` = if_else(
      is.na(`Local Authority`),
      LA,
      `Local Authority`
    ),
    Country = if_else(
      is.na(Country),
      `Country code`,
      Country
    )
  ) |>
  select(-LA, -`Country code`) |>
  arrange(`Date of Transfer`, `Local Authority`) |>
  # Remove the Isles of Scilly as there are too few sales and remove the Scottish Borders as it is Scotland!
  filter(
    !is.na(`Local Authority`) &
      !(`Local Authority` %in% c("Isles of Scilly", "Scottish Borders"))
  )

## Write data for future reference

price_paid_data |>
  select(
    `Date of Transfer`,
    `Local Authority`,
    Country,
    `PPD Category Type`,
    Price
  ) |>
  write_csv("Data/pp-subset.csv")

# Or for the psqm data

price_paid_data |>
  select(
    `Date of Transfer`,
    `Local Authority`,
    Country,
    `PPD Category Type`,
    Price,
    tfarea,
    priceper
  ) |>
  write_csv("Data/pp-psqm.csv") # overwrite itself

# Repeat sales file; the sales that we are interested in are below the lowest Stamp Duty threshold so we can export from here
price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "month")) |>
  group_by(Date) |>
  mutate(quantile = ntile(Price, 317)) |>
  ungroup() |>
  filter(quantile <= 10) |> # filter for sales at the margin
  select(
    `Date of Transfer`,
    Date,
    Postcode,
    PAON,
    SAON,
    Street,
    `Local Authority`,
    Price,
    quantile
  ) |>
  group_by(Postcode, PAON, SAON, Street) |>
  filter(n() > 1) |>
  ungroup() |>
  write_csv("Data/pp-repeat.csv")
