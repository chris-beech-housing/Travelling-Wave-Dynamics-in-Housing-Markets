# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(slider)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # but not pp-stampduty-psqm!

# Add a little bit of noise to the price paid data to ensure repeated values don't stop the median being calculated
price_paid_data$Price_inc_Stamp_Duty <-
  price_paid_data$Price_inc_Stamp_Duty +
  rnorm(nrow(price_paid_data), mean = 0, sd = 1)

## Calculate the median, number of Land Registry transactions, and moving average of the median

monthly_median <-
  price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "month")) |>
  group_by(Date) |>
  summarise(
    median_price = median(Price_inc_Stamp_Duty),
    `Land Registry transactions` = n()
  ) |>
  mutate(
    `12-MA median price` = slide_dbl(
      median_price,
      mean,
      .before = 5,
      .after = 6,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `2x12-MA median price` = slide_dbl(
      `12-MA median price`,
      mean,
      .before = 1,
      .after = 0,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    )
  ) |>
  ungroup()

## Credit data

# Bank of England data from: https://www.bankofengland.co.uk/boeapps/database/
# Column names have been tidied to the name of the data set outside of this code

# LPMB3C2 Monthly values of total sterling approvals for house purchase to individuals (in sterling millions) not seasonally adjusted
LPMB3C2 <-
  read_csv("Data/LPMB3C2.csv") |>
  mutate(Date = floor_date(as_date(dmy(Date)), "month"))

# LPMVTVU Monthly number of total sterling approvals for house purchase to individuals not seasonally adjusted
LPMVTVU <-
  read_csv("Data/LPMVTVU.csv") |>
  mutate(Date = floor_date(as_date(dmy(Date)), "month"))

# LPMB3XF Monthly value of monetary financial institutions' sterling gross approvals for house purchase to individuals (in sterling millions) not seasonally adjusted
LPMB3XF <-
  read_csv("Data/LPMB3XF.csv") |>
  mutate(Date = floor_date(as_date(dmy(Date)), "month"))

# LPMB3ZF Monthly number of monetary financial institutions' sterling gross approvals for house purchase to individuals not seasonally adjusted
LPMB3ZF <-
  read_csv("Data/LPMB3ZF.csv") |>
  mutate(Date = floor_date(as_date(dmy(Date)), "month"))

# LPMBV86 Monthly value of total sterling gross approvals for house purchase to individuals (in sterling millions) not seasonally adjusted
LPMBV86 <-
  read_csv("Data/LPMBV86.csv") |>
  mutate(Date = floor_date(as_date(dmy(Date)), "month"))

# LPMBV87 Monthly number of total sterling gross approvals for house purchase to individuals not seasonally adjusted
LPMBV87 <-
  read_csv("Data/LPMBV87.csv") |>
  mutate(Date = floor_date(as_date(dmy(Date)), "month"))

# Help to Buy data from: https://www.gov.uk/government/statistics/help-to-buy-equity-loan-scheme-data-to-31-may-2023
# Data has been compiled into a csv file for simplicity
help_to_buy <-
  read_csv("Data/Help to Buy.csv") |>
  mutate(Date = floor_date(as_date(my(Date)), "month")) |>
  rename(htb = Value) |>
  arrange(Date)

# Combine the above
credit <-
  list(LPMB3C2, LPMVTVU, LPMB3XF, LPMB3ZF, LPMBV86, LPMBV87, help_to_buy) |>
  reduce(full_join) |>
  arrange(Date)

rm(LPMB3C2, LPMVTVU, LPMB3XF, LPMB3ZF, LPMBV86, LPMBV87, help_to_buy)

## Calculate variables of interest

credit <-
  credit |>
  mutate(across(everything(), \(x) replace_na(x, 0))) |>
  mutate(across(c(LPMB3C2, LPMB3XF, LPMBV86), \(x) x * 1000000)) |>
  mutate(
    LPMB3C2_cumulative = cumsum(LPMB3C2),
    LPMB3XF_cumulative = cumsum(LPMB3XF),
    LPMBV86_cumulative = cumsum(LPMBV86)
  ) |>
  mutate(
    LPMB3C2_LPMVTVU = (LPMB3C2 + htb) / LPMVTVU,
    LPMB3XF_LPMB3ZF = (LPMB3XF + htb) / LPMB3ZF,
    LPMBV86_LPMBV87 = (LPMBV86 + htb) / LPMBV87
  ) |>
  mutate(across(where(is.numeric), \(x) replace(x, x == 0, NA_real_))) |>
  mutate(across(everything(), \(x) replace(x, is.nan(x), NA_real_))) |>
  mutate(
    `12-MA LPMB3C2 / LPMVTVU` = slide_dbl(
      LPMB3C2_LPMVTVU,
      mean,
      .before = 5,
      .after = 6,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `12-MA LPMB3XF / LPMB3ZF` = slide_dbl(
      LPMB3XF_LPMB3ZF,
      mean,
      .before = 5,
      .after = 6,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `12-MA LPMBV86 / LPMBV87` = slide_dbl(
      LPMBV86_LPMBV87,
      mean,
      .before = 5,
      .after = 6,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `2x12-MA LPMB3C2 / LPMVTVU` = slide_dbl(
      `12-MA LPMB3C2 / LPMVTVU`,
      mean,
      .before = 1,
      .after = 0,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `2x12-MA LPMB3XF / LPMB3ZF` = slide_dbl(
      `12-MA LPMB3XF / LPMB3ZF`,
      mean,
      .before = 1,
      .after = 0,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `2x12-MA LPMBV86 / LPMBV87` = slide_dbl(
      `12-MA LPMBV86 / LPMBV87`,
      mean,
      .before = 1,
      .after = 0,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    )
  )

## Join the credit and house price data, and calculate the median and credit growth

credit_house_prices <-
  left_join(credit, monthly_median, by = c("Date" = "Date")) |>
  mutate(
    `2x12-MA median price delta from previous month` = `2x12-MA median price` -
      lag(`2x12-MA median price`),
    `2x12-MA LPMB3C2 / LPMVTVU delta from previous month` = `2x12-MA LPMB3C2 / LPMVTVU` -
      lag(`2x12-MA LPMB3C2 / LPMVTVU`),
    `2x12-MA median price delta from previous year` = `2x12-MA median price` -
      lag(`2x12-MA median price`, n = 12),
    `2x12-MA LPMB3C2 / LPMVTVU delta from previous year` = `2x12-MA LPMB3C2 / LPMVTVU` -
      lag(`2x12-MA LPMB3C2 / LPMVTVU`, n = 12)
  )

## Graphs

years_sequence <- seq(
  as_date("1995-01-01"),
  as_date("2025-01-01"),
  by = "5 years"
)

# Cumulative credit
credit_house_prices |>
  ggplot(aes(x = Date, y = LPMB3C2_cumulative)) +
  geom_path() +
  scale_x_date(breaks = years_sequence, date_labels = "%Y") +
  scale_y_continuous(labels = \(x) x / 1e12) +
  labs(
    # title = "LPMB3C2 cumulative credit",
    x = "",
    y = "LPMB3C2 (£ trillions)"
  ) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

# Moving average median price and moving average total credit per approval
credit_house_prices |>
  pivot_longer(
    cols = c(
      `2x12-MA median price`,
      `2x12-MA LPMB3C2 / LPMVTVU`,
      `2x12-MA LPMB3XF / LPMB3ZF`,
      `2x12-MA LPMBV86 / LPMBV87`
    ),
    names_to = "Variable",
    values_to = "Pounds"
  ) |>
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "2x12-MA median price",
        "2x12-MA LPMB3C2 / LPMVTVU",
        "2x12-MA LPMBV86 / LPMBV87",
        "2x12-MA LPMB3XF / LPMB3ZF"
      )
    )
  ) |>
  ggplot(aes(x = Date, y = Pounds, colour = Variable)) +
  geom_path() +
  scale_x_date(breaks = years_sequence, date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Moving average median price and moving average total credit per approval",
    x = "",
    y = "Median price / total credit per approval (£)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

# Median price versus total credit per approval
model <- lm(median_price ~ LPMB3C2_LPMVTVU, data = credit_house_prices)
r_squared <- summary(model)$r.squared

credit_house_prices |>
  ggplot(aes(x = LPMB3C2_LPMVTVU, y = median_price)) +
  geom_point(alpha = 0.5) +
  # geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Median price versus total credit per approval",
    # subtitle = paste("R-squared:", round(r_squared, 3)),
    x = "LPMB3C2 / LPMVTVU",
    y = "Median Price (£)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(alpha = "none", size = "none")

credit_house_prices |>
  ggplot(aes(x = `2x12-MA LPMB3C2 / LPMVTVU`, y = `2x12-MA median price`)) +
  geom_path() +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Moving average median price versus moving average total credit per approval",
    x = "LPMB3C2 / LPMVTVU",
    y = "Median Price (£)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

# Median price monthly delta versus credit per transaction monthly delta
credit_house_prices |>
  pivot_longer(
    cols = c(
      "2x12-MA median price delta from previous month",
      "2x12-MA LPMB3C2 / LPMVTVU delta from previous month"
    ),
    names_to = "Variable",
    values_to = "Counts"
  ) |>
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "2x12-MA median price delta from previous month",
        "2x12-MA LPMB3C2 / LPMVTVU delta from previous month"
      )
    )
  ) |>
  ggplot(aes(x = Date, y = Counts, colour = Variable)) +
  geom_path() +
  scale_x_date(breaks = years_sequence, date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Median price monthly delta versus credit per transaction monthly delta",
    x = "",
    y = "Difference (£)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))

# Median price yearly delta versus credit per transaction yearly delta
credit_house_prices |>
  pivot_longer(
    cols = c(
      "2x12-MA median price delta from previous year",
      "2x12-MA LPMB3C2 / LPMVTVU delta from previous year"
    ),
    names_to = "Variable",
    values_to = "Counts"
  ) |>
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "2x12-MA median price delta from previous year",
        "2x12-MA LPMB3C2 / LPMVTVU delta from previous year"
      )
    )
  ) |>
  ggplot(aes(x = Date, y = Counts, colour = Variable)) +
  geom_path() +
  scale_x_date(breaks = years_sequence, date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Median price yearly delta versus credit per transaction yearly delta",
    x = "",
    y = "Difference (£)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))

# Land Registry transactions versus number of credit approvals
credit_house_prices |>
  dplyr::select(Date, LPMVTVU, `Land Registry transactions`) |>
  mutate(Difference = `Land Registry transactions` - LPMVTVU) |>
  pivot_longer(
    cols = c(LPMVTVU, `Land Registry transactions`, Difference),
    names_to = "Variable",
    values_to = "Counts"
  ) |>
  mutate(
    Variable = factor(
      Variable,
      levels = c("Land Registry transactions", "LPMVTVU", "Difference")
    )
  ) |>
  ggplot(aes(x = Date, y = Counts, colour = Variable)) +
  geom_path() +
  scale_x_date(breaks = years_sequence, date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    # title = "Land Registry transactions versus number of credit approvals",
    x = "",
    y = "Count",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 1))
