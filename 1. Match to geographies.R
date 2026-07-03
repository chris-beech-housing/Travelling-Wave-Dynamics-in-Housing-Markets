# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Import data

# Option 1) Price paid data

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
  filter(`Date of Transfer` < "2026-04-01")

# Option 2) Price per square metre data

price_paid_data <-
  read_csv("Data/pp-psqm.csv")

## Matching process

# ONS data for Postcode to Local Authority mappings
# https://geoportal.statistics.gov.uk/datasets/6fff67d204fd4f339591ed667a6e3642/about

ONS <-
  read_csv("Data/ONSPD_MAY_2026_UK.csv") |>
  select(pcds, lad25cd) |>
  arrange(pcds)

# Local Authority names
# https://www.data.gov.uk/dataset/b2c91962-58e7-40f1-ad56-7aa2473a93fd/local-authority-districts-april-2025-names-and-codes-in-the-uk-v21

LA_names <-
  read_csv(
    "Data/Local_Authority_Districts_(April_2025)_Names_and_Codes_in_the_UK_v2.csv"
  ) |>
  select(LAD25CD, LAD25NM)

# Region names - need a UK version, not just England!
# https://www.data.gov.uk/dataset/c1d7290e-c00c-411d-a499-32e222b7bb59/local-authority-district-to-region-april-2025-lookup-in-en-v21

Region_names <-
  read_csv(
    "Data/Local_Authority_District_to_Region_(April_2025)_Lookup_in_EN_v2.csv"
  ) |>
  select(LAD25CD, RGN25NM)

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
  read_csv("Data/Mappings for missing Local Authority.csv") |>
  rename(Region_fill = Region)

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
    ),
    Region_manual = Region_fill
  ) |>
  select(-LA, -`Country code`, -Region_fill) |>
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
    ),
    Region_manual = if_else(
      is.na(Region_manual),
      Region_fill,
      Region_manual
    )
  ) |>
  select(-LA, -`Country code`, -Region_fill) |>
  arrange(`Date of Transfer`, `Local Authority`) |>
  # Remove the Isles of Scilly as there are too few sales and remove the Scottish Borders as it is Scotland!
  filter(
    !is.na(`Local Authority`) &
      !(`Local Authority` %in% c("Isles of Scilly", "Scottish Borders"))
  )

## Local Authority code to Region, ensure there are no missing Local Authority values before this step

price_paid_data |> filter(is.na(`Local Authority`))

price_paid_data <-
  price_paid_data |>
  left_join(Region_names, by = c("lad25cd" = "LAD25CD")) |>
  rename(Region = RGN25NM) |>
  mutate(
    Region = case_when(
      Country == "W" ~ "Wales", # since the Region_names doesn't have mappings for Wales!
      !is.na(Region) ~ Region,
      TRUE ~ Region_manual
    )
  ) |>
  select(-Region_manual)

## Write data for future reference

# Option 1) Price paid data

price_paid_data |>
  select(
    `Date of Transfer`,
    `Local Authority`,
    Country,
    Region,
    `Property Type`,
    `PPD Category Type`,
    Price
  ) |>
  write_csv("Data/pp-subset.csv")

# Option 2) Price per square metre data

price_paid_data |>
  select(
    `Date of Transfer`,
    `Local Authority`,
    Country,
    Region,
    `Property Type`,
    `PPD Category Type`,
    Price,
    tfarea,
    priceper
  ) |>
  write_csv("Data/pp-psqm.csv") # overwrite itself

# Repeat sales file
price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "month")) |>
  group_by(Date) |>
  mutate(quantile = ntile(Price, 317)) |>
  ungroup() |>
  select(
    `Date of Transfer`,
    Date,
    Postcode,
    PAON,
    SAON,
    Street,
    `Local Authority`,
    Country,
    Price,
    quantile
  ) |>
  group_by(Postcode, PAON, SAON, Street) |>
  filter(n() > 1) |>
  ungroup() |>
  write_csv("Data/pp-repeat.csv")
