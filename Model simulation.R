## ------------------------------------------------------------------
## Travelling Wave Simulation
##
## Simulates the travelling wave in z-space (standardised log-price),
## constrained to a logistic distribution, and converts to quantile
## space.
##
## The model implements:
##   - Uniform advection of a localised growth RIDGE at speed κ in
##     z-space (a Gaussian bump, used here for definiteness)
##   - Log-logistic structure maintained by PARTIAL projection onto
##     the logistic manifold each step; this projection is what splits
##     the realised dynamics into aggregate drift (μ̇), reshaping (ṡ·z),
##     and the residual ridge — see §3.2. With g_bar = 0 the only
##     applied growth is the ridge; drift and reshaping are emergent.
##   - Replicator dynamics (§3.5): capital density, driven by relative
##     growth (g − ḡ), reallocates in synchrony with the ridge and
##     numerically exhibits the same advection speed κ
##   - Crash trigger (§4): activates when the ridge peak reaches the
##     resolvable margin — Δz consumed, no adjacent lower segment left
##     to target — independent of growth amplitude
##
## The crash field itself is illustrative: it models the qualitative
## prediction of §4 (monotonic correction propagating upward from
## the margin) without claiming a derived functional form for the
## correction dynamics. The recovery between cycles is similarly
## illustrative; the paper has one completed crash observation and
## does not propose a recovery mechanism.
##
## Cycle 2 uses different growth amplitude, background growth, and
## noise to demonstrate that the travelling wave's ordering is
## independent of aggregate growth rates (§2.1).
##
## All traversal timing is endogenous from κ and the distributional
## geometry. The initial μ and s are fitted from the empirical data.
##
## Prices are initialised at the empirical cross-sectional median
## prices from the first month of the price-per-square-metre dataset,
## and log-logistic shape preservation uses the same projection as
## the null model simulation.
## ------------------------------------------------------------------

library(tidyverse)
library(slider)
library(plotly)

set.seed(42)

## ── Empirical initial prices ───────────────────────────────────────
## Same initialisation as the null model simulation.

setwd("/Users/chrisbeech/Documents/UK housing analysis")

psqm <- read_csv("Data/pp-stampduty-psqm.csv")

n_quantiles <- 317

initial_prices <-
  psqm |>
  mutate(Date = floor_date(`Date of Transfer`, "month")) |>
  filter(Date == min(Date)) |>
  mutate(quantile = ntile(Price_inc_Stamp_Duty, n_quantiles)) |>
  group_by(quantile) |>
  summarise(median_price = median(Price_inc_Stamp_Duty), .groups = "drop") |>
  arrange(quantile) |>
  pull(median_price)

## ── Parameters ──────────────────────────────────────────────────────

kappa <- 1.1 # advection speed in z-space (per year)
sigma_width <- 2.0 # width of the Gaussian growth profile in z-space

## Cycle 1 growth parameters
g_bar_1 <- 0 # background annual growth rate
g_amplitude_1 <- 0.10 # amplitude of the growth peak
noise_sd_1 <- 0.001 # monthly noise
crash_amplitude_1 <- 0.05 # peak monthly negative growth at the margin
crash_speed_1 <- 12 # months for crash front to propagate

## Cycle 2 growth parameters — deliberately different to demonstrate
## that the travelling wave ordering is independent of growth rates
g_bar_2 <- 0 # lower background growth
g_amplitude_2 <- 0.04 # smaller amplitude
noise_sd_2 <- 0.0005 # monthly noise
crash_amplitude_2 <- 0.03 # peak monthly negative growth at the margin
crash_speed_2 <- 12 # months for crash front to propagate

## Recovery duration (illustrative)
recovery_m <- 12

## ── Logistic coordinates ───────────────────────────────────────────
## Same as null model.

q_vals <- ((1:n_quantiles) - 0.5) / n_quantiles
logit_q <- log(q_vals / (1 - q_vals))

## Log-logistic shape preservation (same as null model)
shape_reversion <- 0.05

## ── Distribution boundaries and endogenous timing ──────────────────

## Distribution boundaries from §3.6 (truncation parameters,
## not grid resolution). The wave peak in z-space is continuous;
## the 317-quantile grid is for representation only.

q_min <- 1e-4
q_max <- 1 - q_min

z_full_lower <- log(q_min / (1 - q_min))
z_full_upper <- log(q_max / (1 - q_max))

delta_z_active <- z_full_upper - z_full_lower

## Equilibrium capital density per quantile
f_eq <- 1 / n_quantiles

## Margin band: bottom 10% of quantiles, used for density monitoring
margin_band <- 1:max(1, floor(n_quantiles * 0.10))

## Simulation sizing: generous allocation for two full cycles
T_traversal_m <- round((delta_z_active / kappa) * 12)
n_months <- as.integer(
  2 * (T_traversal_m + max(crash_speed_1, crash_speed_2) + recovery_m) + 36
)

## ── Growth field in z-space ────────────────────────────────────────

growth_bump <- function(z, z_peak, amplitude) {
  amplitude * exp(-(z - z_peak)^2 / (2 * sigma_width^2))
}

## ── Crash field (illustrative, §4) ───────────────────────────
## Models the qualitative prediction of §4: a correction front
## propagates upward from the margin. Below the front, growth is
## negative, strongest at the margin. Above the front, background
## growth diminishes. This reproduces the empirically observed
## monotonic crash ordering (§4.1).

crash_field <- function(
  progress,
  logit_q,
  q_vals,
  z_min,
  delta_z,
  amplitude,
  g_bar
) {
  z_front <- z_min + progress * delta_z * 1
  g <- numeric(length(logit_q))

  below <- logit_q < z_front
  if (any(below)) {
    depth <- 1 -
      (logit_q[below] - min(logit_q)) /
        (z_front - min(logit_q) + 0.01)
    g[below] <- -amplitude * depth
  }
  g[!below] <- (g_bar / 12) * (1 - progress * 1)

  g
}

## ── Simulation ──────────────────────────────────────────────────────

log_price <- matrix(0, nrow = n_months, ncol = n_quantiles)
growth_rate <- matrix(0, nrow = n_months, ncol = n_quantiles)
capital_f <- matrix(f_eq, nrow = n_months, ncol = n_quantiles)
s_t <- numeric(n_months)
mu_t <- numeric(n_months)
z_peak_t <- numeric(n_months)
phase <- character(n_months)
margin_share <- numeric(n_months)

# Initial log-prices from empirical data
log_price[1, ] <- log(initial_prices)

# Fit initial μ and s from the empirical prices (s is implicit)
fit0 <- .lm.fit(cbind(1, logit_q), log_price[1, ])
mu_t[1] <- fit0$coefficients[1]
s_t[1] <- fit0$coefficients[2]

## State variables
z_peak_current <- z_full_upper
in_crash <- FALSE
in_recovery <- FALSE
crash_start <- NA
recovery_start <- NA
cycle_number <- 1L

z_peak_t[1] <- z_peak_current
phase[1] <- "Cycle 1"
margin_share[1] <- sum(capital_f[1, margin_band])

for (t in 2:n_months) {
  ## ── Monitor capital density at the margin ────────────────────────
  ## This is the structural indicator: density excess at the boundary
  ## signals that conservative transport is about to fail (§4.2)

  ms <- sum(capital_f[t - 1, margin_band])
  margin_share[t] <- ms

  ## ── Crash trigger (§4) ─────────────────────────────────────────────
  ## The crash activates when the wave reaches the distributional
  ## boundary: the informational distance Δz has been fully consumed
  ## and no adjacent lower segment remains for reallocation to target.
  ## This is independent of growth amplitude (§2.1).

  wave_at_boundary <- z_peak_current <= z_full_lower + 0.5

  if (!in_crash && !in_recovery && wave_at_boundary) {
    in_crash <- TRUE
    crash_start <- t
  }

  ## ── Crash → Recovery transition (illustrative) ──────────────────

  crash_spd_now <- if (cycle_number >= 2L) crash_speed_2 else crash_speed_1
  if (in_crash && (t - crash_start) >= crash_spd_now) {
    in_crash <- FALSE
    in_recovery <- TRUE
    recovery_start <- t
  }

  ## ── Recovery → New cycle transition (illustrative) ───────────────

  if (in_recovery && (t - recovery_start) >= recovery_m) {
    in_recovery <- FALSE
    z_peak_current <- z_full_upper
    cycle_number <- cycle_number + 1L
  }

  ## ── Select cycle parameters ─────────────────────────────────────

  c2 <- cycle_number >= 2L
  g_bar_now <- if (c2) g_bar_2 else g_bar_1
  g_amp_now <- if (c2) g_amplitude_2 else g_amplitude_1
  noise_now <- if (c2) noise_sd_2 else noise_sd_1
  crash_amp_now <- if (c2) crash_amplitude_2 else crash_amplitude_1

  ## ── Growth field ────────────────────────────────────────────────

  if (in_crash) {
    ## Crash (illustrative, §4)
    crash_progress <- min(1, (t - crash_start) / crash_spd_now)
    growth_rate[t, ] <- crash_field(
      crash_progress,
      logit_q,
      q_vals,
      z_full_lower,
      delta_z_active,
      crash_amp_now,
      g_bar_now
    )
  } else if (in_recovery) {
    ## Recovery (illustrative)
    rec_progress <- (t - recovery_start) / recovery_m
    growth_rate[t, ] <- g_bar_now / 12 * rec_progress * 0.5

    # Capital density re-equilibrates during recovery
    capital_f[t - 1, ] <- capital_f[t - 1, ] * (1 - 0.15) + f_eq * 0.15
  } else {
    ## Normal: advect wave peak and apply growth bump
    z_peak_current <- z_peak_current - kappa / 12
    z_peak_current <- max(z_peak_current, z_full_lower)

    ## When the wave peak is in the tails (outside grid representable
    ## range), the visible bump is held at the boundary decile. The wave
    ## continues to advect in z-space for cycle timing and the crash
    ## trigger; the bump applied to the grid is clamped.
    z_peak_effective <- max(min(z_peak_current, max(logit_q)), min(logit_q))
    bump <- growth_bump(logit_q, z_peak_effective, g_amp_now)

    growth_rate[t, ] <- g_bar_now / 12 + bump / 12
  }

  # Add noise (cycle-specific)
  growth_rate[t, ] <- growth_rate[t, ] + rnorm(n_quantiles, 0, noise_now)

  ## ── Update log-prices ───────────────────────────────────────────

  log_price[t, ] <- log_price[t - 1, ] + growth_rate[t, ]

  ## ── Project onto log-logistic manifold ──────────────────────────
  ## Same as null model.

  fit <- .lm.fit(cbind(1, logit_q), log_price[t, ])
  mu_t[t] <- fit$coefficients[1]
  s_t[t] <- fit$coefficients[2]

  fitted_logistic <- mu_t[t] + s_t[t] * logit_q
  log_price[t, ] <- log_price[t, ] +
    shape_reversion * (fitted_logistic - log_price[t, ])

  ## ── Update capital density (replicator dynamics, §3.5) ──────────
  ## Euler

  # g_vec <- growth_rate[t, ]
  # g_mean <- sum(capital_f[t - 1, ] * g_vec)
  # capital_f[t, ] <- capital_f[t - 1, ] * (1 + g_vec - g_mean)
  # capital_f[t, ] <- pmax(capital_f[t, ], 1e-10)
  # capital_f[t, ] <- capital_f[t, ] / sum(capital_f[t, ])
  
  ## ── Update capital density (replicator dynamics, §3.5) ──────────
  ## Exact one-step solution of the replicator equation for monthly-
  ## constant g: f ∝ f·exp(g), normalised. The ḡ subtraction is
  ## absorbed by the normalisation, Z(t), so the
  ## discrete panel equals the closed form sampled monthly, exactly.
  
  capital_f[t, ] <- capital_f[t - 1, ] * exp(growth_rate[t, ])
  capital_f[t, ] <- capital_f[t, ] / sum(capital_f[t, ])

  ## ── Store state ─────────────────────────────────────────────────

  z_peak_t[t] <- z_peak_current
  if (in_crash) {
    phase[t] <- paste("Crash", cycle_number)
  } else if (in_recovery) {
    phase[t] <- paste("Recovery", cycle_number)
  } else {
    phase[t] <- paste("Cycle", cycle_number)
  }
}

## ── Trim to meaningful data ────────────────────────────────────────

last_month <- max(which(s_t > 0))
if (last_month < n_months) {
  n_months <- last_month
}

## ── Closed-form verification of the replicator panel  ─────────────
## Within each stretch where only the replicator acts (cycles and
## crashes), the capital density must satisfy
##   f(t) ∝ f(anchor) · exp( Σ g ),   renormalised each month.
## Recovery months are excluded: the re-equilibration step there is
## deliberately outside the replicator. With the Euler update,
## agreement is to integrator accuracy (~1e-5); with the exponential
## update it is exact to machine precision.

is_rec  <- startsWith(phase[1:n_months], "Recovery")
run_id  <- cumsum(c(TRUE, diff(is_rec) != 0))

closed_form_check <- map_dfr(unique(run_id[!is_rec]), function(rid) {
  months <- which(run_id == rid)
  t0 <- min(months); t1 <- max(months)
  anchor <- max(t0 - 1L, 1L)
  ## the first recovery step mutates the preceding stored row in
  ## place, so drop the final month of any run followed by recovery
  if (t1 < n_months && is_rec[t1 + 1L]) t1 <- t1 - 1L
  if (t1 <= anchor) return(NULL)
  mths <- (anchor + 1L):t1
  G <- apply(growth_rate[mths, , drop = FALSE], 2, cumsum)
  if (is.null(dim(G))) G <- matrix(G, nrow = 1L)
  w <- sweep(exp(G), 2, capital_f[anchor, ], `*`)
  f_ref <- w / rowSums(w)
  tibble(
    segment     = rid,
    months      = paste0(min(mths), "-", t1),
    phase_end   = phase[t1],
    max_abs_err = max(abs(f_ref - capital_f[mths, , drop = FALSE]))
  )
})

print(closed_form_check)
stopifnot(all(closed_form_check$max_abs_err < 1e-3))  # tighten to 1e-12 with the exponential update

## ── Assemble results ───────────────────────────────────────────────

sim_data <- tibble(
  month = rep(1:n_months, each = n_quantiles),
  quantile = rep(1:n_quantiles, times = n_months),
  q = rep(q_vals, times = n_months),
  z = rep(logit_q, times = n_months),
  log_price = as.numeric(t(log_price[1:n_months, ])),
  growth = as.numeric(t(growth_rate[1:n_months, ])),
  capital_f = as.numeric(t(capital_f[1:n_months, ])),
  year = rep(1:n_months, each = n_quantiles) / 12
)

## ── Compute smoothed growth ranks (matching empirical methodology) ─

sim_ranked <- sim_data |>
  group_by(quantile) |>
  arrange(month) |>
  mutate(
    log_return = log_price - lag(log_price, 12),
    ma12 = slide_dbl(
      log_return,
      mean,
      .before = 5,
      .after = 6,
      .complete = FALSE
    ),
    ma2x12 = slide_dbl(ma12, mean, .before = 1, .after = 0, .complete = FALSE)
  ) |>
  ungroup() |>
  drop_na(ma2x12) |>
  group_by(month) |>
  mutate(growth_rank = row_number(desc(ma2x12))) |>
  ungroup()

## ── Decile aggregation ─────────────────────────────────────────────

sim_decile <- sim_ranked |>
  mutate(decile = ceiling(quantile / (n_quantiles / 10))) |>
  group_by(decile, month, year) |>
  summarise(
    ma2x12 = mean(ma2x12, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(month) |>
  mutate(growth_rank = row_number(desc(ma2x12))) |>
  ungroup()

## ── Wave peak trajectory ───────────────────────────────────────────

wave_peak_q <- sim_ranked |>
  group_by(month) |>
  slice_max(ma2x12, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    z_peak = logit_q[quantile],
    q_peak = q_vals[quantile]
  )

wave_peak_decile <- sim_decile |>
  group_by(month) |>
  slice_max(ma2x12, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(z_peak = log((decile - 0.5) / 10 / (1 - (decile - 0.5) / 10)))

## ── Plots ───────────────────────────────────────────────────────────

cat("Creating travelling wave simulation plots...\n")

p_qspace <- sim_decile |>
  plot_ly(
    x = ~year,
    y = ~decile,
    z = ~ -growth_rank,
    type = "heatmap",
    colorscale = "RdBu",
    showscale = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(
      title = "Decile",
      autorange = TRUE,
      showticklabels = FALSE,
      ticks = ""
    )
  )

p_qspace_full <- sim_ranked |>
  plot_ly(
    x = ~year,
    y = ~quantile,
    z = ~ -growth_rank,
    type = "heatmap",
    colorscale = "RdBu",
    showscale = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Quantile", autorange = TRUE)
  )

p_zpeak <- wave_peak_q |>
  plot_ly(
    x = ~year,
    y = ~z_peak,
    type = "scatter",
    mode = "markers",
    marker = list(size = 3, opacity = 0.4, color = "steelblue"),
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "z* = log(q*/(1-q*))")
  )

p_qpeak <- wave_peak_q |>
  plot_ly(
    x = ~year,
    y = ~q_peak,
    type = "scatter",
    mode = "markers",
    marker = list(size = 3, opacity = 0.4, color = "steelblue"),
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "q* (wave peak quantile)")
  )

p_returns <- sim_decile |>
  plot_ly(
    x = ~year,
    y = ~ma2x12,
    color = ~ factor(decile),
    type = "scatter",
    mode = "lines",
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Annual log return")
  )

capital_data <- sim_data |>
  mutate(decile = ceiling(quantile / (n_quantiles / 10))) |>
  group_by(decile, month, year) |>
  summarise(capital_f = sum(capital_f), .groups = "drop")

p_capital <- capital_data |>
  plot_ly(
    x = ~year,
    y = ~capital_f,
    color = ~ factor(decile),
    type = "scatter",
    mode = "lines",
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Fractional capital share")
  )

p_scale <- tibble(
  month = 1:n_months,
  s = s_t[1:n_months],
  year = (1:n_months) / 12
) |>
  filter(s > 0) |>
  plot_ly(
    x = ~year,
    y = ~s,
    type = "scatter",
    mode = "lines",
    line = list(color = "darkgreen"),
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "s")
  )

decile_prices <- sim_data |>
  mutate(decile = ceiling(quantile / (n_quantiles / 10))) |>
  group_by(decile, month, year) |>
  summarise(log_price = mean(log_price), .groups = "drop")

p_prices <- decile_prices |>
  plot_ly(
    x = ~year,
    y = ~log_price,
    color = ~ factor(decile),
    type = "scatter",
    mode = "lines",
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Log price")
  )

# Combine all plots into a subplot

proportional_share <- length(margin_band) / n_quantiles

p_margin <- tibble(
  month = 1:n_months,
  margin_share_pct = margin_share[1:n_months] * 100,
  year = (1:n_months) / 12
) |>
  plot_ly(
    x = ~year,
    y = ~margin_share_pct,
    type = "scatter",
    mode = "lines",
    line = list(color = "firebrick"),
    name = "Margin band share",
    showlegend = FALSE
  ) |>
  add_lines(
    x = c(1, n_months) / 12,
    y = rep(proportional_share * 100, 2),
    line = list(color = "gray", dash = "dash"),
    name = "Proportional share",
    inherit = FALSE,
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Bottom 10% share of capital (%)")
  )

subplot_combined <-
  subplot(
    p_returns,
    p_qspace,
    p_capital,
    p_margin,
    nrows = 4,
    shareX = TRUE,
    shareY = FALSE
  )

subplot_combined
