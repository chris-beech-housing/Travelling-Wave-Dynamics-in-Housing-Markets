# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(sf)
library(slider)
library(plotly)
library(scales)
library(ggrepel)
library(patchwork)
library(igraph)
library(fitdistrplus)
library(broom)
library(av)
library(sandwich)
library(Kendall)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

ppd <- read_csv("Data/pp-stampduty.csv")
psqm <- read_csv("Data/pp-stampduty-psqm.csv")

## Local Authority shapefile
## Source: https://geoportal.statistics.gov.uk/datasets/f3528c2d6d454edab74f2648cc6a45f6_0/
## st_simplify reduces geometry complexity for faster rendering (dTolerance in metres)
uk_shapefile <-
  st_read(
    "Data/Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-5850961694214429102/LAD_MAY_2024_UK_BGC.shp",
    quiet = TRUE
  ) |>
  filter(str_starts(LAD24CD, "E") | str_starts(LAD24CD, "W")) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 500)

## Display constants for log-return heatmaps and colour bars
log_return_bound <- log(1.3) # ±30% — clamp for heatmap display and colorbar extremes
log_return_tick <- log(1.15) # ±15% — inner colorbar tick marker
colourbar_x <- 0.44675 # plotly colorbar horizontal position (centred below 2x2 grid)
kappa_r2 <- 0.8 # filter kappa plots based on this R^2 value

## Function to calculate median, log return, and growth rank
## Parameters
##   filter_col / filter_val : optional single-category subset
##   group_by_var            : "quantile" | "Local Authority"
##   n_quantiles             : price ntiles when group_by_var == "quantile"
##   ma_type                 : "symmetric" (centred 2x12-MA) | "predictive" (trailing)
create_monthly_median <- function(
  data,
  filter_col = NULL,
  filter_val = NULL,
  group_by_var = NULL,
  n_quantiles = NULL,
  ma_type = "symmetric"
) {
  # Apply filter if specified
  if (!is.null(filter_col) && !is.null(filter_val)) {
    data <- data |> filter(.data[[filter_col]] == filter_val)
  }

  # Order and optionally bin Local Authorities
  if (group_by_var == "Local Authority") {
    # Order Local Authorities based on most recent year
    max_date <- max(data$`Date of Transfer`)
    one_year_ago <- max_date - years(1)

    ordered_levels <- data |>
      filter(`Date of Transfer` >= one_year_ago) |>
      group_by(.data[[group_by_var]]) |>
      summarise(
        median_price = median(Price_inc_Stamp_Duty),
        .groups = "drop"
      ) |>
      arrange(median_price) |>
      pull(.data[[group_by_var]])

    data <- data |>
      mutate(
        !!group_by_var := factor(.data[[group_by_var]], levels = ordered_levels)
      )

    # Only bin into ntiles if n_quantiles differs from the number of distinct Local Authorities
    n_la <- n_distinct(data[[group_by_var]])
    if (!is.null(n_quantiles) && n_quantiles != n_la) {
      data <- data |>
        mutate(
          !!group_by_var := ntile(
            as.numeric(.data[[group_by_var]]),
            n_quantiles
          )
        )
    }
  }

  # Calculate base metrics
  if (group_by_var == "quantile") {
    # Create quantiles based on price
    result <- data |>
      mutate(Date = floor_date(`Date of Transfer`, "month")) |>
      arrange(Date) |>
      group_by(Date) |>
      mutate(quantile = ntile(Price_inc_Stamp_Duty, n_quantiles)) |>
      ungroup() |>
      group_by(quantile, Date) |>
      summarise(
        median_price = median(Price_inc_Stamp_Duty),
        .groups = "drop"
      ) |>
      group_by(quantile) |>
      mutate(log_return = log(median_price / lag(median_price, n = 12))) |>
      ungroup()
  } else {
    # Group by existing variable (i.e. Local Authority)
    result <- data |>
      mutate(Date = floor_date(`Date of Transfer`, "month")) |>
      arrange(Date) |>
      group_by(.data[[group_by_var]], Date) |>
      summarise(
        median_price = median(Price_inc_Stamp_Duty),
        .groups = "drop"
      ) |>
      group_by(.data[[group_by_var]]) |>
      mutate(log_return = log(median_price / lag(median_price, n = 12))) |>
      ungroup()
  }

  # Apply moving average based on type
  if (ma_type == "symmetric") {
    # 2x12-MA (symmetric)
    result <- result |>
      group_by(.data[[group_by_var]]) |>
      mutate(
        `12-MA_log_return` = slide_dbl(
          log_return,
          mean,
          .before = 5,
          .after = 6,
          .complete = FALSE
        ),
        `2x12-MA_log_return` = slide_dbl(
          `12-MA_log_return`,
          mean,
          .before = 1,
          .after = 0,
          .complete = FALSE
        )
      ) |>
      ungroup()
  } else if (ma_type == "predictive") {
    # Predictive: rolling average of the last 11 months (plus current)
    result <- result |>
      group_by(.data[[group_by_var]]) |>
      mutate(
        `12-MA_log_return` = slide_dbl(
          log_return,
          mean,
          .before = 11,
          .after = 0,
          .complete = FALSE
        ),
        `2x12-MA_log_return` = slide_dbl(
          `12-MA_log_return`,
          mean,
          .before = 0,
          .after = 0,
          .complete = FALSE
        )
      ) |>
      ungroup()
  }

  # Calculate growth rank based on the smoothed return
  result <- result |>
    group_by(Date) |>
    # Un-comment this line to re-order the Local Authorities each month
    # and ensure static = FALSE in the function call
    # mutate(`Local Authority` = row_number(median_price)) |>
    # Change this line to just log_return for unsmoothed plots
    mutate(growth_rank = row_number(desc(`2x12-MA_log_return`))) |>
    ungroup()

  return(result)
}

## Cullen and Frey plot: kurtosis vs square of skewness, with reference distributions
cullen_frey <- function(
  data,
  column,
  group_by,
  add_labels = FALSE,
  xlim = NULL,
  ylim = NULL,
  plot_title = NULL,
  point_alpha = 1,
  use_custom_colors = (group_by == "Year")
) {
  # Calculate moments for the specified column grouped by the grouping variable
  moments <-
    data |>
    filter(is.finite(.data[[column]])) |>
    group_by(.data[[group_by]]) |>
    group_split() |>
    map(\(x) descdist(x[[column]], graph = FALSE))

  skewness <- map_dbl(moments, "skewness")
  skewness_squared <- skewness^2
  kurtosis <- map_dbl(moments, "kurtosis")

  results_df <- tibble(
    skewness_squared = skewness_squared,
    kurtosis = kurtosis,
    group = unique(data[[group_by]])
  )

  # Reference distribution curves (algorithms from fitdistrplus::descdist)
  sequence <- seq(-8, 8, 0.01)

  gamma_df <- tibble(
    skewness_squared = 4 / exp(sequence),
    kurtosis = 3 + 6 / exp(sequence)
  )

  lognormal_df <- tibble(
    skewness_squared = (exp(exp(sequence)^2) + 2)^2 *
      (exp(exp(sequence)^2) - 1),
    kurtosis = exp(exp(sequence)^2)^4 +
      2 * exp(exp(sequence)^2)^3 +
      3 * exp(exp(sequence)^2)^2 -
      3
  )

  normal_df <- tibble(skewness_squared = 0, kurtosis = 3)
  logistic_df <- tibble(skewness_squared = 0, kurtosis = 4.2)

  # Setup custom colors if needed (for Year plots)
  if (use_custom_colors) {
    categories <- c(
      "Gamma",
      "Local Authority",
      "Logistic",
      "Lognormal",
      "Normal"
    )
    default_hue <- hue_pal()(length(categories))
    custom_colours <- set_names(default_hue, categories)
    group_colour <- custom_colours["Local Authority"]
    custom_colours <- custom_colours[names(custom_colours) != "Local Authority"]
    custom_colours[group_by] <- group_colour
  }

  # Base Cullen and Frey plot
  p <-
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
    geom_point(
      data = results_df,
      aes(
        x = skewness_squared,
        y = kurtosis,
        colour = group_by,
        alpha = point_alpha
      )
    ) +
    scale_y_reverse() +
    labs(x = "Square of skewness", y = "Kurtosis", colour = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(size = "none", alpha = "none")

  if (add_labels) {
    p <- p +
      geom_text_repel(
        data = results_df,
        aes(x = skewness_squared, y = kurtosis, label = group),
        size = 3
      )
  }

  if (use_custom_colors) {
    p <- p +
      scale_colour_manual(
        values = custom_colours,
        breaks = c("Normal", "Logistic", "Gamma", "Lognormal", group_by)
      )
  } else {
    p <- p +
      scale_colour_discrete(
        breaks = c("Normal", "Logistic", "Gamma", "Lognormal", group_by)
      )
  }

  if (!is.null(xlim) && !is.null(ylim)) {
    p <- p + coord_cartesian(xlim = xlim, ylim = ylim, clip = "on")
  }
  if (!is.null(plot_title)) {
    p <- p + labs(title = plot_title)
  }

  return(p)
}

## Kappa helpers: fit linear model to log-odds wave peak z*(t) per cycle
fit_kappa <- function(data) {
  df <- data |>
    filter(!is.na(z_star), is.finite(z_star)) |>
    mutate(t_years = as.numeric(Date - min(Date)) / 365.25)

  fit <- lm(z_star ~ t_years, data = df)

  kappa <- coef(fit)[2]
  # x-intercept: extrapolated date at which z* = 0 (wave crosses the distribution centre)
  t0_years <- -coef(fit)[1] / kappa
  t0_date <- min(df$Date) + t0_years * 365.25

  ols_se <- summary(fit)$coefficients[2, 2]
  hac_vcov <- sandwich::kernHAC(fit, prewhite = 1, adjust = TRUE)
  hac_se <- sqrt(hac_vcov[2, 2])

  tibble(
    kappa = kappa,
    intercept = coef(fit)[1],
    t0 = t0_date,
    r_squared = summary(fit)$r.squared,
    se_kappa_ols = ols_se,
    se_kappa_hac = hac_se,
    n_obs = nrow(df)
  )
}

## Two-sample z-test on κ₁ − κ₂ using HAC standard errors.
## NOTE: this is computed on a single illustrative window, not the grid —
## report it as illustrative, not as ensemble evidence for κ-invariance.
test_kappa_invariance <- function(fits) {
  k1 <- fits$kappa[fits$cycle == 1]
  k2 <- fits$kappa[fits$cycle == 2]
  se1 <- fits$se_kappa_hac[fits$cycle == 1]
  se2 <- fits$se_kappa_hac[fits$cycle == 2]
  z <- (k1 - k2) / sqrt(se1^2 + se2^2)
  tibble(
    diff = k1 - k2,
    z = z,
    p = 2 * (1 - pnorm(abs(z)))
  )
}

## Per-cycle velocity decomposition: V = dℓ*/dt and drift dμ/dt via OLS on t_years
## (HAC SEs), plus mean/harmonic-mean of the cross-sectional scale s.
## Used to check V against dμ/dt and the s·κ term.
fit_V <- function(wave_peak, parameters) {
  df <- wave_peak |>
    inner_join(parameters |> dplyr::select(Date, ln_mu, slope), by = "Date") |>
    mutate(l_star = ln_mu + slope * z_star) |>
    filter(!is.na(l_star), !is.na(cycle))

  V_estimates <- df |>
    group_by(cycle) |>
    group_modify(
      ~ {
        d <- .x |> mutate(t_years = as.numeric(Date - min(Date)) / 365.25)
        fit_l <- lm(l_star ~ t_years, data = d)
        fit_mu <- lm(ln_mu ~ t_years, data = d)
        hac_l <- sandwich::kernHAC(fit_l, prewhite = 1, adjust = TRUE)
        hac_mu <- sandwich::kernHAC(fit_mu, prewhite = 1, adjust = TRUE)
        tibble(
          V = coef(fit_l)[2],
          se_V = sqrt(hac_l[2, 2]),
          mu_dot = coef(fit_mu)[2],
          se_mu = sqrt(hac_mu[2, 2]),
          n_obs = nrow(d)
        )
      }
    ) |>
    ungroup()

  s_estimates <- df |>
    group_by(cycle) |>
    summarise(
      s_mean = mean(slope, na.rm = TRUE),
      s_harmonic = 1 / mean(1 / slope, na.rm = TRUE),
      s_sd = sd(slope, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  list(
    V_estimates = V_estimates,
    s_estimates = s_estimates
  )
}

## Pointwise term series for the dℓ*/dt decomposition, window-independent.
## Computes the per-row series in both s-variants (raw, 2x12-smoothed) so the
## per-window aggregation can be evaluated many times without recomputing the
## differencing. Returns list(d_raw, d_smooth) of identical schema.
velocity_decomposition_pointwise <- function(params_df, wave_peak) {
  base <- inner_join(
    params_df |> dplyr::select(Date, ln_mu, slope),
    wave_peak |> dplyr::select(Date, z_star),
    by = "Date"
  ) |> arrange(Date)

  base <- base |>
    mutate(`12-MA_slope` =
             slider::slide_dbl(slope,
                               mean,
                               .before = 5,
                               .after = 6,
                               .complete = FALSE),
           `2x12-MA_slope` =
             slider::slide_dbl(`12-MA_slope`,
                               mean,
                               .before = 1,
                               .after = 0,
                               .complete = FALSE))

  decompose <- function(df, s_col) {
    df |>
      mutate(
        s_used = .data[[s_col]],
        dt_yr  = as.numeric(Date - lag(Date)) / 365.25,
        mu_dot = (ln_mu  - lag(ln_mu))  / dt_yr,        # dμ/dt
        s_dot  = (s_used - lag(s_used)) / dt_yr,        # ṡ
        z_dot  = (z_star - lag(z_star)) / dt_yr,        # ż*
        l_star = ln_mu + s_used * z_star,
        l_star_dot_meas = (l_star - lag(l_star)) / dt_yr,  # measured dℓ*/dt (numerical)
        term_drift     = mu_dot,
        term_reshaping     = s_dot * z_star,                # the dropped term
        term_intrinsic = s_used * z_dot                 # κ-free: derived from data, not from κ
      ) |>
      filter(is.finite(term_reshaping), is.finite(l_star_dot_meas))
  }

  list(
    d_raw    = decompose(base, "slope"),
    d_smooth = decompose(base, "2x12-MA_slope")
  )
}

## Window-aggregation step: cycle-means of the pointwise term series, one row
## per (cycle × s_variant). κ enters only `approx_mu_sbar_kappa`; the other
## terms are κ-free.
velocity_decomposition_window <- function(d_raw, d_smooth, cycle_windows, kappa) {
  summarise_cycle <- function(df, win, cyc, s_label, s_bar, kap) {
    w <- df |> filter(Date >= win[1], Date <= win[2])
    exact_sum <- mean(w$term_drift) + mean(w$term_reshaping) + mean(w$term_intrinsic)
    tibble(
      cycle          = as.integer(cyc),
      s_variant      = s_label,
      drift          = mean(w$term_drift),         # dμ/dt
      reshaping      = mean(w$term_reshaping),     # ṡ·z*   (the dropped term)
      reshaping_abs  = mean(abs(w$term_reshaping)),    # its pointwise magnitude
      intrinsic      = mean(w$term_intrinsic),     # s·ż*  (κ-free)
      exact_sum      = exact_sum,                  # dμ/dt + ṡz* + sż*
      measured       = mean(w$l_star_dot_meas),    # dℓ*/dt, measured directly
      approx_mu_sbar_kappa = mean(w$term_drift) - s_bar * kap,  # V ≈ dμ/dt - s_bar·κ : the approximation that drops the reshaping (ṡ·z*) term
      resid_exact_vs_meas  = exact_sum - mean(w$l_star_dot_meas),
      resid_approx_vs_meas = (mean(w$term_drift) - s_bar * kap) - mean(w$l_star_dot_meas)
    )
  }

  imap_dfr(cycle_windows, function(win, cyc) {
    kap <- unname(kappa[cyc])
    sbar_raw    <- mean(filter(d_raw,    Date >= win[1], Date <= win[2])$slope,  na.rm = TRUE)
    sbar_smooth <- mean(filter(d_smooth, Date >= win[1], Date <= win[2])$s_used, na.rm = TRUE)
    bind_rows(
      summarise_cycle(d_raw,    win, cyc, "raw",      sbar_raw,    kap),
      summarise_cycle(d_smooth, win, cyc, "smoothed", sbar_smooth, kap)
    )
  })
}

## Three-term decomposition of dℓ*/dt, raw vs 2x12-smoothed s(t).
## params_df: Date, ln_mu, slope          (run_log_logistic()$parameters_df)
## wave_peak: Date, z_star (and q_star)    (run_kappa()$wave_peak)
## kappa: named vector of the rank-based κ per cycle
## Returns one row per cycle per s-variant (raw / smoothed), with every term and both targets.
velocity_decomposition <- function(
    params_df, wave_peak,
    kappa = c(`1` = 1.20, `2` = 1.04), # decile fallback; live run passes resolution-matched κ from kappa_estimates
    cycle_windows = list(
      `1` = as_date(c("1999-08-01", "2005-08-01")),
      `2` = as_date(c("2013-12-01", "2019-12-01")))
) {
  d <- velocity_decomposition_pointwise(params_df, wave_peak)
  velocity_decomposition_window(d$d_raw, d$d_smooth, cycle_windows, kappa)
}

## Grid-aggregated three-term decomposition over the surviving κ-fit windows
## from run_kappa_detailed (filter: r_sq_1 > r2_threshold & r_sq_2 > r2_threshold).
## Pointwise term series are window-independent so computed once; the per-row
## window mean is looped over surviving rows. κ enters only `approx_mu_sbar_kappa`,
## using each row's own fitted abs(kappa_1) / abs(kappa_2). The three κ-free
## terms (drift, reshaping, intrinsic) are identical to the single-window function.
## Returns a long-format tibble of median + Q25 + Q75 per (cycle, s_variant,
## variable) for variable ∈ {drift, reshaping, intrinsic, approx_mu_sbar_kappa},
## with identity-check scalars attached as attributes:
##   attr(.,"identity_check_median_abs_resid")          : pooled, reporting figure
##   attr(.,"identity_check_max_abs_resid_by_variant")  : per s_variant, correctness guard
##   attr(.,"n_windows")                                : surviving window count
velocity_decomposition_grid <- function(
    params_df, wave_peak, kappa_segments,
    r2_threshold = kappa_r2
) {
  surviving <- kappa_segments |>
    filter(
      !is.na(kappa_1), !is.na(kappa_2),
      r_sq_1 > r2_threshold, r_sq_2 > r2_threshold
    )

  empty_summary <- tibble(
    variable  = character(),
    cycle     = integer(),
    s_variant = character(),
    median    = numeric(),
    q25       = numeric(),
    q75       = numeric()
  )

  if (nrow(surviving) == 0) {
    warning(
      "velocity_decomposition_grid: 0 windows pass r_sq > ", r2_threshold,
      "; returning empty summary. (e.g. n_quantiles = 317 produces no viable",
      " kappa_segments because q_star is the mean of 32 quantile positions.)",
      call. = FALSE
    )
    attr(empty_summary, "identity_check_median_abs_resid")         <- NA_real_
    attr(empty_summary, "identity_check_max_abs_resid_by_variant") <- c(raw = NA_real_, smoothed = NA_real_)
    attr(empty_summary, "n_windows") <- 0L
    return(empty_summary)
  }

  d <- velocity_decomposition_pointwise(params_df, wave_peak)

  grid_rows <- surviving |>
    mutate(row_id = row_number()) |>
    dplyr::select(row_id, c1_start, c1_end, c2_start, c2_end, kappa_1, kappa_2)

  per_window <- pmap_dfr(
    grid_rows,
    function(row_id, c1_start, c1_end, c2_start, c2_end, kappa_1, kappa_2) {
      cycle_windows <- list(
        `1` = c(c1_start, c1_end),
        `2` = c(c2_start, c2_end)
      )
      kappa <- c(`1` = abs(kappa_1), `2` = abs(kappa_2))
      velocity_decomposition_window(d$d_raw, d$d_smooth, cycle_windows, kappa) |>
        mutate(row_id = row_id, .before = 1)
    }
  )

  # Identity-check scalars before any aggregation drops them.
  resid <- abs(per_window$exact_sum - per_window$measured)
  identity_check_median_abs_resid <- median(resid, na.rm = TRUE)
  identity_check_max_abs_resid_by_variant <- c(
    raw      = max(abs(per_window$exact_sum[per_window$s_variant == "raw"]      -
                       per_window$measured[per_window$s_variant == "raw"]),      na.rm = TRUE),
    smoothed = max(abs(per_window$exact_sum[per_window$s_variant == "smoothed"] -
                       per_window$measured[per_window$s_variant == "smoothed"]), na.rm = TRUE)
  )

  # Long-format summary. `variable` values are deliberately distinct from
  # `decomposition_median_IQR` ({kappa, s_mean, mu_dot, V, V_calc}) so the two
  # cannot be confused at the consumer end.
  summary <- per_window |>
    pivot_longer(
      cols = c(drift, reshaping, intrinsic, approx_mu_sbar_kappa),
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(variable, cycle, s_variant) |>
    summarise(
      median = median(value, na.rm = TRUE),
      q25    = quantile(value, 0.25, na.rm = TRUE),
      q75    = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  attr(summary, "identity_check_median_abs_resid")         <- identity_check_median_abs_resid
  attr(summary, "identity_check_max_abs_resid_by_variant") <- identity_check_max_abs_resid_by_variant
  attr(summary, "n_windows") <- nrow(surviving)
  summary
}

## Exact three-term decomposition of the wave-peak velocity dℓ*/dt, on the
## regression-slope basis (slopes on t_years over the cycle window), so it is
## consistent with the fitted κ.
## dℓ*/dt = dμ/dt + ṡ·z* + s·ż*,   ℓ* = μ + s·z*
## The three term-slopes sum to the measured slope of ℓ* by linearity of OLS.
## params_df: Date, ln_mu, slope   (run_log_logistic()$parameters_df)
## wave_peak: Date, z_star, cycle  (run_kappa()$wave_peak, already cycle-labelled)
## smooth_s: 2x12-smooth s(t) to match the basis z* is defined on (recommended).
fit_decomp_terms <- function(params_df, wave_peak, smooth_s = TRUE) {
  df <- wave_peak |>
    inner_join(params_df |> dplyr::select(Date, ln_mu, slope), by = "Date") |>
    filter(!is.na(cycle)) |>
    arrange(cycle, Date)
  
  if (smooth_s) {
    df <- df |>
      group_by(cycle) |>
      mutate(`12-MA_slope` = 
               slider::slide_dbl(slope,
                                 mean,
                                 .before = 5,
                                 .after = 6,
                                 .complete = FALSE),
             `2x12-MA_slope` = 
               slider::slide_dbl(`12-MA_slope`,
                                 mean,
                                 .before = 1,
                                 .after = 0,
                                 .complete = FALSE)) |>
      ungroup()
  }
  
  df |>
    filter(!is.na(`2x12-MA_slope` ), !is.na(ln_mu), !is.na(z_star)) |>
    group_by(cycle) |>
    group_modify(~ {
      d <- .x |> mutate(t_years = as.numeric(Date - min(Date)) / 365.25)
      n <- nrow(d)
      # Pointwise term series, then OLS slope of each on time.
      d <- d |> mutate(
        s_dot   = c(NA, diff(`2x12-MA_slope` )  / diff(t_years)),
        z_dot   = c(NA, diff(z_star) / diff(t_years)),
        l_star  = ln_mu + `2x12-MA_slope`  * z_star,
        reshaping_t = s_dot * z_star,             # ṡ·z*  (pointwise)
        intr_t  = `2x12-MA_slope`  * z_dot        # s·ż*  (pointwise)
      )
      slope_on_t <- function(y) unname(coef(lm(y ~ d$t_years))[2])
      drift     <- slope_on_t(d$ln_mu)            # dμ/dt
      measured  <- slope_on_t(d$l_star)           # dℓ*/dt, the target
      
      # Regress the term series directly (NA-robust).
      reshaping <- unname(coef(lm(reshaping_t ~ t_years, data = d))[2])
      intrinsic <- unname(coef(lm(intr_t  ~ t_years, data = d))[2])
      tibble(
        drift     = drift,
        reshaping = mean(d$reshaping_t, na.rm = TRUE),   # see note: report the MEAN term
        intrinsic = mean(d$intr_t,  na.rm = TRUE),
        exact_sum = drift + mean(d$reshaping_t, na.rm = TRUE) + mean(d$intr_t, na.rm = TRUE),
        measured  = measured,
        n_obs     = n
      )
    }) |>
    ungroup() |>
    mutate(resid = exact_sum - measured)
}

## Label wave_peak rows with cycle membership, then fit kappa per cycle.
## Returns a tibble with one row per cycle.
label_and_fit <- function(wp, c1_start, c1_end, c2_start, c2_end) {
  wp |>
    mutate(
      cycle = case_when(
        Date >= c1_start & Date <= c1_end ~ 1L,
        Date >= c2_start & Date <= c2_end ~ 2L,
        TRUE ~ NA_integer_
      )
    ) |>
    filter(!is.na(cycle)) |>
    group_by(cycle) |>
    group_modify(~ fit_kappa(.x)) |>
    ungroup()
}

## Per-window decomposition helper: arithmetic mean of slope, OLS slopes of
## ln_mu and l_star = ln_mu + slope*z_star on t_years. No HAC — grid-search
## summary captures window-selection variance, not statistical SE.
fit_decomp_one <- function(wp_window) {
  d <- wp_window |>
    filter(!is.na(slope), !is.na(ln_mu), !is.na(z_star)) |>
    mutate(
      t_years = as.numeric(Date - min(Date)) / 365.25,
      l_star = ln_mu + slope * z_star
    )
  if (nrow(d) < 3) {
    return(c(s_mean = NA_real_, mu_dot = NA_real_, V = NA_real_))
  }
  c(
    s_mean = mean(d$slope, na.rm = TRUE),
    mu_dot = unname(coef(lm(ln_mu ~ t_years, data = d))[2]),
    V = unname(coef(lm(l_star ~ t_years, data = d))[2])
  )
}

## Grid search helper: attempt kappa fits for a given pair of cycle windows.
## Returns a single-row tibble with per-cycle kappa, r_squared, intercept.
try_kappa <- function(wp, c1_start, c2_start, len_months) {
  c1_end <- c1_start %m+% months(len_months)
  c2_end <- c2_start %m+% months(len_months)

  na_row <- tibble(
    c1_start,
    c1_end,
    c2_start,
    c2_end,
    len_months,
    kappa_1 = NA_real_,
    kappa_2 = NA_real_,
    kappa_diff = NA_real_,
    r_sq_1 = NA_real_,
    r_sq_2 = NA_real_,
    r_sq_min = NA_real_,
    intercept_1 = NA_real_,
    intercept_2 = NA_real_,
    s_mean_1 = NA_real_,
    s_mean_2 = NA_real_,
    mu_dot_1 = NA_real_,
    mu_dot_2 = NA_real_,
    V_1 = NA_real_,
    V_2 = NA_real_,
    V_calc_1 = NA_real_,
    V_calc_2 = NA_real_
  )

  n1 <- sum(wp$Date >= c1_start & wp$Date <= c1_end, na.rm = TRUE)
  n2 <- sum(wp$Date >= c2_start & wp$Date <= c2_end, na.rm = TRUE)
  if (n1 < 3 || n2 < 3) {
    return(na_row)
  }

  wp1 <- wp |> filter(Date >= c1_start & Date <= c1_end)
  wp2 <- wp |> filter(Date >= c2_start & Date <= c2_end)

  q1_range <- range(wp1$q_star, na.rm = TRUE)
  q2_range <- range(wp2$q_star, na.rm = TRUE)

  # For 317 quantiles, ceiling(317 * 0.1) = 32
  # so q_star is the mean of 32 quantile positions.
  # That mean can never reach the theoretical single-quantile extremes
  # so it returns the simple plot. It does work for 100 quantiles though.
  if (
    q1_range[1] > 0.5 / max(wp1$n_groups) ||
      q1_range[2] < (max(wp1$n_groups) - 0.5) / max(wp1$n_groups) ||
      q2_range[1] > 0.5 / max(wp2$n_groups) ||
      q2_range[2] < (max(wp2$n_groups) - 0.5) / max(wp2$n_groups)
  ) {
    return(na_row)
  }

  fits <- label_and_fit(wp, c1_start, c1_end, c2_start, c2_end)

  d1 <- fit_decomp_one(wp1)
  d2 <- fit_decomp_one(wp2)

  kappa_1_val <- fits$kappa[fits$cycle == 1]
  kappa_2_val <- fits$kappa[fits$cycle == 2]

  tibble(
    c1_start,
    c1_end,
    c2_start,
    c2_end,
    len_months,
    kappa_1 = kappa_1_val,
    kappa_2 = kappa_2_val,
    kappa_diff = abs(kappa_1_val - kappa_2_val),
    r_sq_1 = fits$r_squared[fits$cycle == 1],
    r_sq_2 = fits$r_squared[fits$cycle == 2],
    r_sq_min = pmin(r_sq_1, r_sq_2),
    intercept_1 = fits$intercept[fits$cycle == 1],
    intercept_2 = fits$intercept[fits$cycle == 2],
    s_mean_1 = unname(d1["s_mean"]),
    s_mean_2 = unname(d2["s_mean"]),
    mu_dot_1 = unname(d1["mu_dot"]),
    mu_dot_2 = unname(d2["mu_dot"]),
    V_1 = unname(d1["V"]),
    V_2 = unname(d2["V"]),
    V_calc_1 = unname(d1["s_mean"]) * kappa_1_val,
    V_calc_2 = unname(d2["s_mean"]) * kappa_2_val
  )
}

## Static analysis function: mean/sd and PCA — depends only on raw log_return,
## so results are identical regardless of ma_type; call once per dataset/grouping
## Returns list(mean_sd = <ggplot>, pca = <ggplot>) for assembly into 2x2 combined plots
analyse_static <- function(
  data,
  monthly_median,
  group_by_var,
  price_xlim = NULL
) {
  ## Price pdf histogram by year
  data_prepared <-
    data |>
    mutate(
      Year = year(`Date of Transfer`),
      log_price = log(Price_inc_Stamp_Duty)
    )

  price_pdf_plot <-
    data_prepared |>
    filter(
      Price_inc_Stamp_Duty < if (!is.null(price_xlim)) price_xlim else exp(14.5)
    ) |>
    ggplot(aes(x = Price_inc_Stamp_Duty)) +
    geom_histogram(bins = 1000) +
    facet_wrap(~Year, ncol = 4, scales = "free") +
    scale_x_continuous(labels = \(x) x / 1e6) +
    scale_y_continuous(labels = \(x) x / 1e3) +
    labs(x = "Price (millions)", y = "Count (thousands)") +
    theme_minimal()

  ## Log-price Cullen and Frey by year
  cullen_frey_year_plot <-
    cullen_frey(
      data = data_prepared,
      column = "log_price",
      group_by = "Year",
      add_labels = TRUE,
      xlim = c(0, 0.5),
      ylim = c(6.75, 2.75),
      use_custom_colors = TRUE
    )

  ## Mean and standard deviation of median price log_return

  mean_sd <-
    monthly_median |>
    filter(!is.na(log_return)) |>
    group_by(.data[[group_by_var]]) |>
    summarise(Mean = mean(log_return), `Standard Deviation` = sd(log_return))

  if (group_by_var == "Local Authority") {
    mean_sd$`Local Authority` <- as.numeric(mean_sd$`Local Authority`) # Convert to a number, else theme_minimal() doesn't work
  }

  mean_sd_plot <-
    mean_sd |>
    pivot_longer(
      cols = c(Mean, `Standard Deviation`),
      names_to = "variable",
      values_to = "Summary statistics"
    ) |>
    ggplot(aes(
      x = `Summary statistics`,
      y = .data[[group_by_var]],
      colour = variable
    )) +
    geom_point(size = 1) +
    coord_cartesian(clip = 'off') +
    labs(
      # title = "Mean and standard deviation of the log return of the median price, by Local Authority",
      # title = "Mean and standard deviation of the log return of the median price, by quantile",
      x = NULL,
      y = NULL,
      colour = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = if (group_by_var == "Local Authority") {
        element_blank()
      } else {
        element_text()
      },
      axis.ticks.y = if (group_by_var == "Local Authority") {
        element_blank()
      } else {
        element_line()
      },
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )

  ## Principal Component Analysis K-Means Clustering

  # Scale the data
  cluster_data_scaled <-
    monthly_median |>
    arrange(.data[[group_by_var]]) |>
    filter(!is.na(log_return)) |>
    dplyr::select(all_of(group_by_var), Date, log_return) |>
    pivot_wider(names_from = Date, values_from = log_return) |>
    column_to_rownames(group_by_var) |>
    as.matrix() |>
    scale()

  # K-Means Clustering
  # set.seed(123) # seems to work fine without this
  kmeans_result <- kmeans(
    cluster_data_scaled,
    centers = if (group_by_var == "Local Authority") 2 else 4
  ) # use the whole data set to preserve all original features

  # Principal Components Analysis
  pca_result <- prcomp(cluster_data_scaled)

  # Re-label clusters consistently by ranking by mean PC1 score,
  # so cluster colours are stable across datasets (ppd vs psqm).
  # PC1 sign can flip between datasets, so anchor it: ensure PC1 correlates
  # positively with row index (cheap → expensive order in cluster_data_scaled).
  pc1_scores <- pca_result$x[, 1]
  if (cor(pc1_scores, seq_along(pc1_scores)) < 0) {
    pc1_scores <- -pc1_scores
  }
  cluster_means <- tibble(cluster = kmeans_result$cluster, pc1 = pc1_scores) |>
    group_by(cluster) |>
    summarise(mean_pc1 = mean(pc1), .groups = "drop") |>
    mutate(rank_order = rank(-mean_pc1))
  rank_lookup <- set_names(cluster_means$rank_order, cluster_means$cluster)
  kmeans_result$cluster <- as.integer(rank_lookup[as.character(
    kmeans_result$cluster
  )])

  # Prepare and plot
  pca_scores <- as_tibble(pca_result$x, rownames = group_by_var)

  pca_plot <-
    pca_scores |>
    ggplot(aes(
      x = PC2,
      y = -PC1,
      label = .data[[group_by_var]],
      colour = factor(
        kmeans_result$cluster,
        levels = sort(unique(kmeans_result$cluster))
      )
    )) +
    geom_point() +
    geom_text_repel(
      data = pca_scores,
      aes(x = PC2, y = -PC1, label = .data[[group_by_var]]),
      size = 3
    ) +
    coord_cartesian(clip = 'off') +
    # labs(
    #   title = "Principal Component Analysis, K-Means Clustering, by Local Authority"
    #   title = "Principal Component Analysis K-Means Clustering, by quantile"
    # ) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "none")

  ## Local Authority maps (LA only, ppd only — slow, uses pre-simplified shapefile)
  map_median_price <- NULL
  map_kmeans <- NULL

  if (group_by_var == "Local Authority") {
    # Derive most recent year median price from monthly_median
    la_median_price <-
      monthly_median |>
      filter(Date == max(Date)) |>
      dplyr::select(`Local Authority`, median_price)

    # Median price map
    sf_median <-
      uk_shapefile |>
      left_join(la_median_price, by = c("LAD24NM" = "Local Authority")) |>
      filter(!is.na(median_price))

    map_median_price <-
      sf_median |>
      ggplot() +
      geom_sf(aes(fill = log(median_price)), colour = NA) +
      scale_fill_distiller(palette = "RdBu") +
      theme_minimal() +
      theme(legend.position = "none")

    # K-Means cluster map
    kmeans_la <-
      tibble(
        `Local Authority` = rownames(cluster_data_scaled),
        kmeans = factor(
          kmeans_result$cluster,
          levels = sort(unique(kmeans_result$cluster))
        )
      )

    sf_kmeans <-
      uk_shapefile |>
      left_join(kmeans_la, by = c("LAD24NM" = "Local Authority")) |>
      filter(!is.na(kmeans))

    map_kmeans <-
      sf_kmeans |>
      ggplot() +
      geom_sf(aes(fill = kmeans), colour = NA) +
      scale_fill_hue() +
      theme_minimal() +
      theme(legend.position = "none")
  }

  # Spearman rank correlation vs first month (LA only)
  if (group_by_var == "Local Authority") {
    spearman_plot <-
      monthly_median |>
      group_by(Date) |>
      mutate(monthly_rank = row_number(median_price)) |>
      ungroup() |>
      left_join(
        monthly_median |>
          filter(Date == min(Date)) |>
          group_by(Date) |>
          mutate(first_month_rank = row_number(median_price)) |>
          ungroup() |>
          dplyr::select(`Local Authority`, first_month_rank),
        by = "Local Authority"
      ) |>
      group_by(Date) |>
      summarise(
        spearman_rho = cor(
          monthly_rank,
          first_month_rank,
          method = "spearman",
          use = "complete.obs"
        )
      ) |>
      ungroup() |>
      ggplot(aes(x = Date, y = spearman_rho)) +
      geom_line() +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
      labs(x = NULL, y = "Spearman's rho") +
      theme_minimal()

    # First vs last month rank scatter
    rank_scatter_plot <-
      monthly_median |>
      group_by(Date) |>
      mutate(rank = row_number(median_price)) |>
      ungroup() |>
      filter(Date == min(Date) | Date == max(Date)) |>
      mutate(period = if_else(Date == min(Date), "first", "last")) |>
      dplyr::select(`Local Authority`, period, rank) |>
      pivot_wider(names_from = period, values_from = rank) |>
      mutate(abs_change = abs(last - first)) |>
      ggplot(aes(x = first, y = last, colour = abs_change)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        colour = "grey50"
      ) +
      scale_colour_viridis_c(name = "Rank change") +
      labs(
        x = paste("Rank in", format(min(monthly_median$Date), "%b %Y")),
        y = paste("Rank in", format(max(monthly_median$Date), "%b %Y"))
      ) +
      theme_minimal()
  } else {
    spearman_plot <- NULL
    rank_scatter_plot <- NULL
  }

  list(
    price_pdf = price_pdf_plot,
    cullen_frey_year = cullen_frey_year_plot,
    mean_sd = mean_sd_plot,
    pca = pca_plot,
    map_median_price = map_median_price,
    map_kmeans = map_kmeans,
    spearman = spearman_plot,
    rank_scatter = rank_scatter_plot
  )
}

## LA-only static analysis: two Cullen-Frey plots (price by LA, log return by LA)
## Returns list(cullen_frey_price, cullen_frey_log_return) for 2x2 assembly
analyse_static_la <- function(data, monthly_median) {
  # Price distribution by Local Authority (most recent year)
  cullen_frey_price_plot <-
    cullen_frey(
      data = data |> filter(`Date of Transfer` >= "2025-01-01"),
      column = "Price_inc_Stamp_Duty",
      group_by = "Local Authority",
      add_labels = FALSE,
      xlim = c(0, 40),
      ylim = c(80, 0),
      point_alpha = 0.5,
      use_custom_colors = FALSE
    )

  # Log return distribution by Local Authority
  cullen_frey_log_return_plot <-
    cullen_frey(
      data = monthly_median |> filter(!is.na(log_return)),
      column = "log_return",
      group_by = "Local Authority",
      add_labels = FALSE,
      xlim = c(0, 2),
      ylim = c(8, 2),
      point_alpha = 0.5,
      use_custom_colors = FALSE
    )

  list(
    cullen_frey_price = cullen_frey_price_plot,
    cullen_frey_log_return = cullen_frey_log_return_plot
  )
}

## Spearman 2x2: rho time series (top) + rank scatter (bottom), ppd left / psqm right
plot_combined_spearman <- function(plots_LA) {
  p_ppd <- plots_LA$ppd$spearman
  p_psqm <- plots_LA$psqm$spearman
  s_ppd <- plots_LA$ppd$rank_scatter
  s_psqm <- plots_LA$psqm$rank_scatter

  # Shared y-range for Spearman rho row
  ymin <- min(
    layer_data(p_ppd, 1)$y,
    layer_data(p_psqm, 1)$y,
    na.rm = TRUE
  )
  shared_ylim <- coord_cartesian(ylim = c(ymin, 1))

  right_panel_theme <- theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

  wrap_plots(
    p_ppd + shared_ylim,
    p_psqm + shared_ylim + right_panel_theme,
    s_ppd,
    s_psqm + theme(axis.title.y = element_blank()),
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "none")
}

## Combined 2x2 static plot functions (patchwork)
## Order matches plotly 2x2s: LA/ppd top-left, LA/psqm top-right, Q/ppd bottom-left, Q/psqm bottom-right

plot_combined_kappa <- function(plots) {
  all_plots <- list(
    plots$LA$ppd$kappa,
    plots$LA$psqm$kappa,
    plots$Q$ppd$kappa,
    plots$Q$psqm$kappa
  )
  all_x <- map(all_plots, \(p) layer_data(p, 1)$x) |> list_c()
  shared_xlim <- coord_cartesian(xlim = as_date(range(all_x, na.rm = TRUE)))

  right_panel_theme <- theme(axis.title.y = element_blank())
  wrap_plots(
    plots$LA$ppd$kappa + shared_xlim,
    plots$LA$psqm$kappa + shared_xlim + right_panel_theme,
    plots$Q$ppd$kappa + shared_xlim,
    plots$Q$psqm$kappa + shared_xlim + right_panel_theme,
    nrow = 2
  )
}

plot_combined_mean_sd <- function(plots) {
  # Suppress y-axis title on right-hand panels (already shown on left)
  right_panel_theme <- theme(axis.title.y = element_blank())

  inner <- wrap_plots(
    plots$LA$ppd$mean_sd,
    plots$LA$psqm$mean_sd + right_panel_theme,
    plots$Q$ppd$mean_sd,
    plots$Q$psqm$mean_sd + right_panel_theme,
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "bottom")

  inner
}

plot_combined_pca <- function(plots) {
  inner <- wrap_plots(
    plots$LA$ppd$pca,
    plots$LA$psqm$pca,
    plots$Q$ppd$pca,
    plots$Q$psqm$pca,
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "none")

  # Shared axis labels: "PC1" on the left y-axis, "PC2" on the bottom x-axis
  wrap_elements(inner) +
    labs(tag = "PC1", caption = "PC2") +
    theme(
      plot.tag = element_text(angle = 90, size = 11),
      plot.tag.position = "left",
      plot.caption = element_text(hjust = 0.5, size = 11)
    )
}


## 2x2 combined LA-only static plots: rows = price by LA | log return by LA
## columns = ppd (left) | psqm (right)
## Right panels have y-axis title suppressed (left panels retain their own per-row titles)
plot_combined_la_static_2x2 <- function(plots_la_static) {
  right_panel_theme <- theme(axis.title.y = element_blank())

  wrap_plots(
    plots_la_static$ppd$cullen_frey_price,
    plots_la_static$psqm$cullen_frey_price + right_panel_theme,
    plots_la_static$ppd$cullen_frey_log_return,
    plots_la_static$psqm$cullen_frey_log_return + right_panel_theme,
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "bottom")
}

# Log return histogram (2x2: LA/ppd, LA/psqm, Q/ppd, Q/psqm)
# Shared symmetric x-axis so zero aligns across all panels
plot_combined_log_return_hist <- function(plots) {
  all_medians <- list(
    plots$LA$ppd$monthly_median,
    plots$LA$psqm$monthly_median,
    plots$Q$ppd$monthly_median,
    plots$Q$psqm$monthly_median
  )
  max_abs <- max(map_dbl(all_medians, \(mm) {
    max(abs(mm$log_return), na.rm = TRUE)
  }))

  make_hist <- function(monthly_median, xlim) {
    lr <- monthly_median$log_return[!is.na(monthly_median$log_return)]
    desc <- descdist(lr, graph = FALSE)
    label <- paste0(
      "Skewness: ",
      round(desc$skewness, 2),
      "\nKurtosis: ",
      round(desc$kurtosis, 2)
    )

    ggplot(monthly_median, aes(x = log_return)) +
      geom_histogram(bins = 500) +
      annotate(
        "text",
        x = xlim,
        y = Inf,
        label = label,
        hjust = 1,
        vjust = 1.5,
        size = 3
      ) +
      scale_x_continuous(labels = label_comma()) +
      scale_y_continuous(labels = label_comma()) +
      coord_cartesian(xlim = c(-xlim, xlim)) +
      labs(x = "Log return", y = "Count") +
      theme_minimal()
  }

  right_panel_theme <- theme(axis.title.y = element_blank())

  wrap_plots(
    make_hist(plots$LA$ppd$monthly_median, max_abs),
    make_hist(plots$LA$psqm$monthly_median, max_abs) + right_panel_theme,
    make_hist(plots$Q$ppd$monthly_median, max_abs),
    make_hist(plots$Q$psqm$monthly_median, max_abs) + right_panel_theme,
    nrow = 2
  )
}

## Dynamic analysis function: network graph and adjacency matrix — depends on
## growth_rank which varies with ma_type; returns list(adj = <adjacency matrix plotly>)
analyse_monthly_median <- function(monthly_median, group_by_var) {
  ## Network graph

  # Convert to a number to match quantile - needed for the 2x2 plots
  if (group_by_var == "Local Authority") {
    monthly_median$`Local Authority` <- as.numeric(
      monthly_median$`Local Authority`
    )
  }

  # Spatial edges - Growth rank pairs (r → r + 1) and (r → r - 1) at time t
  spatial_edges <-
    monthly_median |>
    dplyr::select(all_of(group_by_var), Date, growth_rank) |>
    filter(!is.na(growth_rank)) |>
    arrange(Date, growth_rank) |>
    group_by(Date) |>
    mutate(
      to_forward = lead(.data[[group_by_var]]),
      to_backward = lag(.data[[group_by_var]])
    ) |>
    ungroup() |>
    pivot_longer(
      cols = c(to_forward, to_backward),
      names_to = "direction",
      values_to = "to"
    ) |>
    filter(!is.na(to)) |>
    mutate(type = "spatial") |>
    dplyr::select(from = all_of(group_by_var), to, type, Date)

  # Temporal edges - Growth rank at times t and t + 1
  temporal_edges <-
    monthly_median |>
    dplyr::select(all_of(group_by_var), Date, growth_rank) |>
    filter(!is.na(growth_rank)) |>
    arrange(growth_rank, Date) |>
    group_by(growth_rank) |>
    mutate(to = lead(.data[[group_by_var]])) |>
    ungroup() |>
    filter(!is.na(to)) |>
    mutate(type = "temporal") |>
    dplyr::select(from = all_of(group_by_var), to, type, Date)

  # Combine both edge types
  edges <- bind_rows(spatial_edges, temporal_edges)

  # Per-month diagonal concentration (±5% and ±10% bandwidths)
  n_ids <- n_distinct(monthly_median[[group_by_var]])
  bandwidth_5 <- max(1, round(n_ids * 0.05))
  bandwidth_10 <- max(1, round(n_ids * 0.10))

  diag_conc_ts_5 <- edges |>
    group_by(Date) |>
    summarise(diag_conc = mean(abs(from - to) <= bandwidth_5), .groups = "drop")

  diag_conc_ts_10 <- edges |>
    group_by(Date) |>
    summarise(
      diag_conc = mean(abs(from - to) <= bandwidth_10),
      .groups = "drop"
    )

  edges <- edges |> dplyr::select(from, to, type)

  # Count frequency of each connection
  edge_weights <-
    edges |>
    group_by(from, to) |>
    summarize(weight = n(), .groups = "drop") |>
    mutate(weight = weight / max(weight))

  # Create graph; the graphs are too busy to be plotted using library(ggraph)
  graph <-
    graph_from_data_frame(
      d = edge_weights,
      directed = TRUE,
      vertices = unique(union(edge_weights$from, edge_weights$to))
    )

  ## Adjacency matrix

  adj_matrix <-
    as_adjacency_matrix(
      graph,
      attr = "weight",
      sparse = FALSE
    )

  if (group_by_var == "Local Authority") {
    row_ids <- rownames(adj_matrix)

    adj_matrix_long <-
      as_tibble(adj_matrix, rownames = "y") |>
      mutate(y = factor(y, levels = row_ids)) |>
      pivot_longer(-y, names_to = "x", values_to = "value") |>
      mutate(x = factor(x, levels = row_ids))
  } else {
    adj_matrix_long <-
      as_tibble(adj_matrix, rownames = "y") |>
      mutate(y = as.numeric(y)) |>
      pivot_longer(-y, names_to = "x", values_to = "value") |>
      mutate(x = as.numeric(x))
  }

  # Plot adjacency matrix
  adj_plot <-
    adj_matrix_long |>
    plot_ly(
      x = ~x,
      y = ~y,
      z = ~ -value,
      type = "heatmap",
      colorscale = "Greys",
      showscale = FALSE
    ) |>
    layout(
      xaxis = list(
        title = "",
        type = "linear",
        autorange = "reversed"
      ),
      yaxis = list(
        title = "",
        type = "linear"
      )
    )

  return(list(
    adj = adj_plot,
    diag_conc_ts_5 = diag_conc_ts_5,
    diag_conc_ts_10 = diag_conc_ts_10
  ))
}

## Standard axis layout for monthly heatmaps: blank titles, year ticks on x, hidden y labels
heatmap_axis_layout <- function(p) {
  p |>
    layout(
      xaxis = list(
        title = "",
        tickformat = "%Y",
        range = c("1996-07-01", "2026-03-01")
      ),
      yaxis = list(title = "", ticks = "", showticklabels = FALSE)
    )
}

## Region and Property Type subplot functions

# Comment out lines here to create a subset, filter by Date below
regions <- c(
  "London",
  "South East",
  "East of England",
  "South West",
  "West Midlands",
  "East Midlands",
  "Yorkshire and The Humber",
  "North West",
  "Wales",
  "North East"
)

property_types <- c("D", "S", "T", "F")

plot_category_subplots <- function(
  data,
  categories,
  filter_col,
  group_by_var,
  n_quantiles = 10,
  ma_type = "symmetric"
) {
  plots <- map(categories, function(cat_val) {
    create_monthly_median(
      data,
      filter_col = filter_col,
      filter_val = cat_val,
      group_by_var = group_by_var,
      n_quantiles = n_quantiles,
      ma_type = ma_type
    ) |>
      plot_ly(
        x = ~Date,
        y = ~ .data[[group_by_var]],
        z = ~ -growth_rank,
        type = "heatmap",
        colorscale = "RdBu",
        showscale = FALSE
      ) |>
      heatmap_axis_layout()
  })

  n <- length(categories)
  m <- 0.005
  t <- (1 - (n - 1) * m) / n
  row_heights <- c(t + m / 2, rep(t + m, n - 2), t + m / 2)

  subplot(
    plots,
    nrows = n,
    shareX = TRUE,
    shareY = TRUE,
    margin = m,
    heights = row_heights
  )
}

## Combined 2x2 and 2x1 plot functions

# Shared rank colorbar helper used by plot_combined_rank and plot_combined_log_return_rank
make_rank_colorbar <- function(n_quantiles) {
  legend_numbers <- if (n_quantiles == 10) {
    seq(1, 10, by = 1)
  } else {
    seq(50, 300, by = 50)
  }
  plot_ly(
    z = matrix(c(-n_quantiles, -1), nrow = 2),
    type = "heatmap",
    colorscale = "RdBu",
    showscale = TRUE,
    opacity = 0,
    colorbar = list(
      title = "Growth rank",
      tickvals = -unique(legend_numbers),
      ticktext = unique(legend_numbers),
      orientation = "h",
      thickness = 10,
      x = colourbar_x,
      y = 0
    )
  ) |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# Growth plots
plot_combined_growth <- function(plots) {
  subplot_combined <-
    subplot(
      plots$LA$ppd$growth,
      plots$LA$psqm$growth,
      plots$Q$ppd$growth,
      plots$Q$psqm$growth,
      nrows = 2,
      shareX = TRUE,
      shareY = FALSE
    )

  # Dummy trace for shared colorbar only
  colorbar <-
    plot_ly(
      z = matrix(c(-log_return_bound, log_return_bound), nrow = 2),
      type = "heatmap",
      colorscale = "RdBu",
      showscale = TRUE,
      opacity = 0,
      colorbar = list(
        title = "Log return",
        tickvals = c(
          -log_return_bound,
          -log_return_tick,
          0,
          log_return_tick,
          log_return_bound
        ),
        ticktext = c("-30%", "-15%", "0%", "15%", "30%"),
        orientation = "h",
        thickness = 10,
        x = colourbar_x,
        y = 0
      )
    ) |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )

  # Use subplot and add the shared colorbar trace
  subplot(subplot_combined, colorbar, nrows = 2, heights = c(0.95, 0.025))
}

# Growth rank plots
plot_combined_rank <- function(plots, n_quantiles = 317) {
  subplot_combined <-
    subplot(
      plots$LA$ppd$rank,
      plots$LA$psqm$rank,
      plots$Q$ppd$rank,
      plots$Q$psqm$rank,
      nrows = 2,
      shareX = TRUE,
      shareY = FALSE
    )

  subplot(
    subplot_combined,
    make_rank_colorbar(n_quantiles),
    nrows = 2,
    heights = c(0.95, 0.025)
  )
}

# Log return and growth rank for a single dataset (ppd or psqm)
plot_combined_log_return_rank <- function(plots_single, n_quantiles = 10) {
  subplot_combined <-
    subplot(
      plots_single$log_return,
      plots_single$rank,
      nrows = 2,
      shareX = TRUE,
      shareY = FALSE
    )

  subplot(
    subplot_combined,
    make_rank_colorbar(n_quantiles),
    nrows = 2,
    heights = c(0.95, 0.025)
  )
}

# Adjacency matrices
plot_combined_adj <- function(plots) {
  subplot(
    plots$LA$ppd$adj,
    plots$LA$psqm$adj,
    plots$Q$ppd$adj,
    plots$Q$psqm$adj,
    nrows = 2,
    shareX = FALSE,
    shareY = FALSE
  )
}

# Diagonal concentration time series (2-panel: ±5% top, ±10% bottom)
plot_combined_diag_concentration <- function(plots) {
  make_panel <- function(field, label) {
    bind_rows(
      plots$LA$ppd[[field]] |> mutate(combination = "Local Authority by price"),
      plots$LA$psqm[[field]] |>
        mutate(combination = "Local Authority by price per square metre"),
      plots$Q$ppd[[field]] |> mutate(combination = "Quantile by price"),
      plots$Q$psqm[[field]] |>
        mutate(combination = "Quantile by price per square metre")
    ) |>
      ggplot(aes(x = Date, y = diag_conc, colour = combination)) +
      geom_line() +
      labs(x = NULL, y = label, colour = NULL) +
      theme_minimal()
  }

  wrap_plots(
    make_panel("diag_conc_ts_5", "Diagonal concentration (\u00B15% bandwidth)"),
    make_panel(
      "diag_conc_ts_10",
      "Diagonal concentration (\u00B110% bandwidth)"
    ),
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "bottom")
}

## Per-dataset analysis function
## static = TRUE produces mean/sd and PCA plots (only needed once, not repeated across ma_type runs)
## subplots = TRUE produces region and property type subplots (quantile only, slow)
run_analysis <- function(
  data,
  group_by_var,
  n_quantiles = NULL,
  ma_type = "symmetric",
  static = TRUE,
  subplots = TRUE,
  price_xlim = NULL
) {
  monthly_median <-
    create_monthly_median(
      data,
      group_by_var = group_by_var,
      n_quantiles = n_quantiles,
      ma_type = ma_type
    )

  ## Plot log return

  log_return_plot <-
    monthly_median |>
    plot_ly(
      x = ~Date,
      y = ~`2x12-MA_log_return`,
      color = ~ factor(.data[[group_by_var]]),
      type = 'scatter',
      mode = 'lines',
      showlegend = FALSE
    )

  ## Heatmaps

  # Reduce the scale slightly, thereby increasingly the contrast
  monthly_median <-
    monthly_median |>
    mutate(
      display_return = pmin(
        pmax(`2x12-MA_log_return`, -log_return_bound),
        log_return_bound
      )
    )

  # Plot growth and growth rank

  growth_plot <-
    monthly_median |>
    plot_ly(
      x = ~Date,
      y = ~ .data[[group_by_var]],
      z = ~display_return,
      zmid = 0,
      type = "heatmap",
      colorscale = "RdBu",
      showscale = FALSE
    ) |>
    heatmap_axis_layout()

  rank_plot <-
    monthly_median |>
    plot_ly(
      x = ~Date,
      y = ~ .data[[group_by_var]],
      z = ~ -growth_rank,
      type = "heatmap",
      colorscale = "RdBu",
      showscale = FALSE
    ) |>
    heatmap_axis_layout()

  if (group_by_var == "quantile" && subplots) {
    ## Region
    plot_category_subplots(
      data,
      regions,
      "Region",
      group_by_var,
      n_quantiles,
      ma_type
    ) |>
      print_plot(width = 14, height = 10)

    ## Property Type
    plot_category_subplots(
      data,
      property_types,
      "Property Type",
      group_by_var,
      n_quantiles,
      ma_type
    ) |>
      print_plot(width = 14, height = 10)
  }

  static_plots <- if (static) {
    analyse_static(data, monthly_median, group_by_var, price_xlim = price_xlim)
  } else {
    list(
      price_pdf = NULL,
      cullen_frey_year = NULL,
      mean_sd = NULL,
      pca = NULL,
      map_median_price = NULL,
      map_kmeans = NULL,
      spearman = NULL,
      rank_scatter = NULL
    )
  }

  analysis <- analyse_monthly_median(
    monthly_median,
    group_by_var = group_by_var
  )

  list(
    log_return = log_return_plot,
    growth = growth_plot,
    rank = rank_plot,
    adj = analysis$adj,
    diag_conc_ts_5 = analysis$diag_conc_ts_5,
    diag_conc_ts_10 = analysis$diag_conc_ts_10,
    price_pdf = static_plots$price_pdf,
    cullen_frey_year = static_plots$cullen_frey_year,
    mean_sd = static_plots$mean_sd,
    pca = static_plots$pca,
    map_median_price = static_plots$map_median_price,
    map_kmeans = static_plots$map_kmeans,
    spearman = static_plots$spearman,
    rank_scatter = static_plots$rank_scatter,
    monthly_median = monthly_median
  )
}

## Log-logistic analysis function (quantile only)
## Fits a log-logistic model (log-price ~ log-odds) per month and plots results
## Accepts a pre-computed monthly_median to avoid recomputing create_monthly_median
## Returns list(log_odds = <ggplot>, parameters = <ggplot>) for assembly into 2x2
run_log_logistic <- function(monthly_median) {
  # Log-odds versus log-price
  monthly_median <-
    monthly_median |>
    mutate(
      Q = max(quantile),
      p = (quantile - 0.5) / Q,
      log_odds = qlogis(p),
      log_price = log(median_price)
    )

  selected_dates <- as_date(c(
    "2007-08-01",
    "2008-07-01",
    "2010-01-01",
    "2020-01-01"
  ))

  log_odds_plot <-
    monthly_median |>
    filter(Date %in% selected_dates) |>
    ggplot(aes(x = log_odds, y = log_price, colour = factor(Date))) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    labs(
      # title = "Log-Price versus Log-Odds",
      x = "Log-odds",
      y = "Log-price",
      colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Parameters
  parameters <-
    monthly_median |>
    group_by(Date) |>
    summarise(
      model = list(lm(log_price ~ log_odds)),
      .groups = "drop"
    ) |>
    mutate(coefs = map(model, tidy), stats = map(model, glance)) |>
    unnest(stats) |>
    dplyr::select(Date, coefs, r.squared, adj.r.squared) |>
    unnest(coefs) |>
    dplyr::select(Date, term, estimate, std.error, r.squared, adj.r.squared) |>
    pivot_wider(names_from = term, values_from = c(estimate, std.error)) |>
    rename(
      ln_mu = `estimate_(Intercept)`,
      slope = estimate_log_odds,
      slope_se = std.error_log_odds
    ) |>
    mutate(
      mu = exp(ln_mu),
      slope_upper = slope + 2 * slope_se,
      slope_lower = slope - 2 * slope_se
    ) |>
    arrange(Date) |>
    drop_na()

  parameters_plot <-
    parameters |>
    ggplot(aes(x = Date, y = slope)) +
    geom_line() +
    geom_ribbon(
      aes(ymin = slope_lower, ymax = slope_upper),
      alpha = 0.5,
      fill = "grey"
    ) +
    geom_point(
      data = parameters |> filter(Date %in% selected_dates),
      aes(colour = factor(Date)),
      size = 2
    ) +
    labs(x = NULL, y = "Slope") +
    theme_minimal() +
    guides(colour = "none")

  list(
    log_odds = log_odds_plot,
    parameters = parameters_plot,
    parameters_df = parameters
  )
}

## Growth ridge analysis (quantile only)
## The travelling wave is the ridge: the departure of the smoothed growth field
## from its instantaneous logistic (linear-in-z) fit. At each month we regress
## the per-quantile 2x12-MA log return on the log-odds and keep the residual
##— this is r(q,t), the localised bump that advects. Regressing g on z recovers
## intercept ≈ dμ/dt and slope ≈ ṡ, so the residual is g − (dμ/dt + ṡ·z).
run_ridge <- function(
    monthly_median,
    selected_dates = as_date(c(
      "2000-06-01",
      "2001-08-01",
      "2003-01-01",
      "2004-06-01"
    ))
) {
  ridge_df <- 
    monthly_median |>
    filter(!is.na(`2x12-MA_log_return`)) |>
    mutate(
      Q = max(quantile),
      q = (quantile - 0.5) / Q,
      log_odds = qlogis(q)
    ) |>
    group_by(Date) |>
    filter(n() >= 4) |> # need ≥4 points to fit the linear-in-z trend
    mutate(ridge = residuals(lm(`2x12-MA_log_return` ~ log_odds))) |>
    ungroup()
  
  # Profiles at selected dates: ridge vs log-odds, with a loess guide.
  # The peak should march from positive log-odds (early in a cycle) to
  # negative (late) — the travelling bump, seen directly in amplitude.
  profiles <- 
    ridge_df |>
    filter(Date %in% selected_dates) |>
    ggplot(aes(x = log_odds, y = ridge, colour = factor(Date))) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
    geom_point(size = 0.6, alpha = 0.4) +
    geom_smooth(se = FALSE, method = "loess", span = 0.4, linewidth = 0.9) +
    labs(
      x = "Log-odds",
      y = expression("Ridge: growth residual (yr"^-1 * ")"),
      colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Ridge heat map: the bump's amplitude over the full period.
  # Colour convention matches your rank heatmaps:
  # above-trend growth (positive ridge) renders red.
  # ridge_bound <- stats::quantile(abs(ridge_df$ridge), 0.98, na.rm = TRUE)
  heatmap <- ridge_df |>
    plot_ly(
      x = ~Date,
      y = ~quantile,
      z = ~ridge,
      type = "heatmap",
      colorscale = "RdBu",
      # zmid = 0,
      # zmin = -ridge_bound,
      # zmax = ridge_bound,
      showscale = FALSE # ,
      # colorbar = list(
      #   orientation = "h",
      #   x = 0.5,
      #   xanchor = "center",
      #   y = -0.2,
      #   yanchor = "top",
      #   title = list(text = "Ridge", side = "top"),
      # )
    ) |>
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "", showticklabels = FALSE, ticks = "")
    )
  
  list(profiles = profiles, heatmap = heatmap, ridge_df = ridge_df)
}

# Model-implied relative capital density under the replicator reading:
# the empirical counterpart of the capital-density panel. The
# cumulative integral restarts at restart_dates # (default 2009-08-01, where 
# the log returns turn positive again after the correction), because we take t0
# within a cycle: integrating through the correction otherwise carries its
# deposit as a permanent level offset in every later month. Pass NULL for one 
# unbroken series. Ridge factor only; uniform f0 at each window start;
# cumsum(ridge)/12; the first ~12 months of each window are partial by the MA.
run_decile_capital_trace <- function(
    ridge_df,
    restart_dates = as_date("2009-08-01"),
    n_deciles = 10
) {
  Q <- max(ridge_df$quantile)
  breaks <- c(
    as_date("1995-01-01"),
    sort(restart_dates),
    as_date("2030-01-01")
  )
  
  trace_df <- ridge_df |>
    mutate(window = cut(Date, breaks = breaks, labels = FALSE)) |>
    group_by(quantile, window) |>
    arrange(Date, .by_group = TRUE) |>
    mutate(cum_ridge = cumsum(ridge) / 12) |>
    ungroup() |>
    mutate(decile = ceiling(quantile * n_deciles / Q)) |>
    group_by(Date, decile) |>
    summarise(
      window = first(window),
      mean_exp = mean(exp(cum_ridge)),
      n = n(),
      .groups = "drop"
    ) |>
    group_by(Date) |>
    mutate(
      rel_density = mean_exp / (sum(mean_exp * n) / sum(n)),
      share = mean_exp * n / sum(mean_exp * n)
    ) |>
    ungroup()
  
  trace_plot <- trace_df |>
    ggplot(aes(x = Date, y = rel_density, colour = factor(decile))) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(aes(group = interaction(decile, window))) +
    labs(
      x = NULL,
      y = "Model-implied relative capital density",
      colour = "(1 = lowest decile)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(
      colour = guide_legend(nrow = 1),
      size = "none", 
      alpha = "none")
  
  list(trace_plot = trace_plot, trace_df = trace_df)
}

## Kappa: wave peak position q*(t) and log-odds z*(t) with linear fits per cycle
run_kappa <- function(monthly_median, group_by_var) {
  wave_peak_base <- monthly_median |>
    filter(!is.na(`2x12-MA_log_return`)) |>
    group_by(Date)

  if (group_by_var == "quantile") {
    wave_peak <- wave_peak_base |>
      summarise(
        n_groups = max(quantile),
        q_star = mean(quantile[growth_rank <= ceiling(n_groups * 0.1)] - 0.5) /
          n_groups,
        .groups = "drop"
      )
  } else {
    wave_peak <- wave_peak_base |>
      mutate(position = row_number(median_price)) |>
      summarise(
        n_groups = n_distinct(`Local Authority`),
        q_star = mean(position[growth_rank <= ceiling(n_groups * 0.1)] - 0.5) /
          n_groups,
        .groups = "drop"
      )
  }

  wave_peak <- wave_peak |>
    mutate(z_star = log(q_star / (1 - q_star)))

  cycle_1_start <- as_date("1999-08-01")
  cycle_1_end <- as_date("2005-08-01")
  cycle_2_start <- as_date("2013-12-01")
  cycle_2_end <- as_date("2019-12-01")

  wave_peak <- wave_peak |>
    mutate(
      cycle = case_when(
        Date >= cycle_1_start & Date <= cycle_1_end ~ 1L,
        Date >= cycle_2_start & Date <= cycle_2_end ~ 2L,
        TRUE ~ NA_integer_
      )
    )

  kappa_estimates <- wave_peak |>
    filter(!is.na(cycle)) |>
    group_by(cycle) |>
    group_modify(~ fit_kappa(.x)) |>
    ungroup()

  kappa_invariance <- test_kappa_invariance(kappa_estimates)

  mk_results <- wave_peak |>
    filter(!is.na(cycle), is.finite(z_star)) |>
    group_by(cycle) |>
    summarise(
      mk_tau = Kendall::MannKendall(z_star)$tau,
      mk_p = Kendall::MannKendall(z_star)$sl,
      .groups = "drop"
    )

  kappa_plot <- wave_peak |>
    filter(!is.na(cycle), q_star < 1) |>
    ggplot(aes(x = Date, y = z_star, colour = factor(cycle))) +
    geom_point(alpha = 0.4, size = 1) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
    labs(x = NULL, y = "") + # Wave peak log-odds z*
    theme_minimal() +
    theme(legend.position = "none")

  # Overlaid version: shift cycles so their start dates align on a common x-axis
  kappa_overlay <- wave_peak |>
    filter(!is.na(cycle), q_star < 1) |>
    mutate(
      cycle_start = if_else(cycle == 1L, cycle_1_start, cycle_2_start),
      years_since_start = as.numeric(Date - cycle_start) / 365.25
    )

  break_years <- 0:floor(max(kappa_overlay$years_since_start))
  dual_labels <- paste0(
    format(cycle_2_start %m+% years(break_years), "%b %Y"),
    "\n",
    format(cycle_1_start %m+% years(break_years), "%b %Y")
  )

  kappa_overlay_plot <- kappa_overlay |>
    ggplot(aes(x = years_since_start, y = z_star, colour = factor(cycle))) +
    geom_point(alpha = 0.4, size = 1) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
    scale_x_continuous(breaks = break_years, labels = dual_labels) +
    labs(x = NULL, y = "", colour = "Cycle") +
    theme_minimal() +
    theme(legend.position = "bottom")

  list(
    kappa = kappa_plot,
    kappa_overlay = kappa_overlay_plot,
    kappa_estimates = kappa_estimates,
    kappa_invariance = kappa_invariance,
    mk_results = mk_results,
    wave_peak = wave_peak
  )
}

## Kappa detailed: grid search over cycle windows + advanced overlay plot
run_kappa_detailed <- function(
  monthly_median,
  group_by_var,
  parameters = NULL
) {
  wave_peak_base <- monthly_median |>
    filter(!is.na(`2x12-MA_log_return`)) |>
    group_by(Date)

  if (group_by_var == "quantile") {
    wave_peak <- wave_peak_base |>
      summarise(
        n_groups = max(quantile),
        q_star = mean(quantile[growth_rank <= ceiling(n_groups * 0.1)] - 0.5) /
          n_groups,
        .groups = "drop"
      )
  } else {
    wave_peak <- wave_peak_base |>
      mutate(position = row_number(median_price)) |>
      summarise(
        n_groups = n_distinct(`Local Authority`),
        q_star = mean(position[growth_rank <= ceiling(n_groups * 0.1)] - 0.5) /
          n_groups,
        .groups = "drop"
      )
  }

  wave_peak <- wave_peak |>
    mutate(z_star = log(q_star / (1 - q_star)))

  if (!is.null(parameters)) {
    wave_peak <- wave_peak |>
      left_join(
        parameters |> dplyr::select(Date, ln_mu, slope),
        by = "Date"
      )
  } else {
    wave_peak <- wave_peak |> mutate(ln_mu = NA_real_, slope = NA_real_)
  }

  cycle_1_start <- as_date("1999-08-01")
  cycle_1_end <- as_date("2005-08-01")
  cycle_2_start <- as_date("2013-12-01")
  cycle_2_end <- as_date("2019-12-01")

  base_len_months <- as.integer(
    interval(cycle_1_start, cycle_1_end) %/% months(1)
  )

  # Grid search: ±12 months starts × ±6 months length (8,125 combos)
  grid <- tidyr::crossing(
    delta_c1 = -12L:12L,
    delta_c2 = -12L:12L,
    delta_len = -6L:6L
  )

  kappa_grid <- pmap_dfr(
    grid,
    function(delta_c1, delta_c2, delta_len) {
      try_kappa(
        wp = wave_peak,
        c1_start = cycle_1_start %m+% months(delta_c1),
        c2_start = cycle_2_start %m+% months(delta_c2),
        len_months = base_len_months + delta_len
      )
    }
  )

  # Label wave_peak with cycle membership for the base windows
  wave_peak <- wave_peak |>
    mutate(
      cycle = case_when(
        Date >= cycle_1_start & Date <= cycle_1_end ~ 1L,
        Date >= cycle_2_start & Date <= cycle_2_end ~ 2L,
        TRUE ~ NA_integer_
      )
    )

  # Segment coordinates aligned to years since each cycle's base start
  kappa_segments <- kappa_grid |>
    filter(!is.na(kappa_1)) |>
    mutate(
      c1_x_start = as.numeric(c1_start - cycle_1_start) / 365.25,
      c1_x_end = as.numeric(c1_end - cycle_1_start) / 365.25,
      c1_y_start = intercept_1,
      c1_y_end = intercept_1 + kappa_1 * as.numeric(c1_end - c1_start) / 365.25,
      c2_x_start = as.numeric(c2_start - cycle_2_start) / 365.25,
      c2_x_end = as.numeric(c2_end - cycle_2_start) / 365.25,
      c2_y_start = intercept_2,
      c2_y_end = intercept_2 + kappa_2 * as.numeric(c2_end - c2_start) / 365.25
    )

  # Wave peak points aligned to common x-axis
  kappa_overlay <- wave_peak |>
    filter(!is.na(cycle), q_star < 1) |>
    mutate(
      cycle_start = if_else(cycle == 1L, cycle_1_start, cycle_2_start),
      years_since_start = as.numeric(Date - cycle_start) / 365.25
    )

  # Dual x-axis labels with padding for shifted segments
  pad_years <- ceiling(max(abs(grid$delta_c1), abs(grid$delta_c2)) / 12)
  break_years <- -pad_years:(floor(max(kappa_overlay$years_since_start)) +
    pad_years)
  dual_labels <- paste0(
    format(cycle_1_start %m+% years(break_years), "%b %Y"),
    "\n",
    format(cycle_2_start %m+% years(break_years), "%b %Y")
  )

  # Advanced overlay plot
  kappa_overlay_plot <- ggplot() +
    geom_segment(
      data = kappa_segments |> filter(r_sq_1 > kappa_r2, r_sq_2 > kappa_r2),
      aes(x = c1_x_start, xend = c1_x_end, y = c1_y_start, yend = c1_y_end),
      alpha = 0.02,
      linewidth = 0.3,
      colour = "#F8766D"
    ) +
    geom_segment(
      data = kappa_segments |> filter(r_sq_1 > kappa_r2, r_sq_2 > kappa_r2),
      aes(x = c2_x_start, xend = c2_x_end, y = c2_y_start, yend = c2_y_end),
      alpha = 0.02,
      linewidth = 0.3,
      colour = "#00BFC4"
    ) +
    geom_point(
      data = kappa_overlay,
      aes(x = years_since_start, y = z_star, colour = factor(cycle)),
      alpha = 0.4,
      size = 1
    ) +
    geom_smooth(
      data = kappa_overlay,
      aes(x = years_since_start, y = z_star, colour = factor(cycle)),
      method = "lm",
      se = TRUE,
      linewidth = 0.8
    ) +
    scale_x_continuous(breaks = break_years, labels = dual_labels) +
    coord_cartesian(xlim = range(break_years)) +
    scale_colour_manual(values = c("1" = "#F8766D", "2" = "#00BFC4")) +
    labs(x = NULL, y = "z*", colour = "Cycle") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Summary statistics across grid windows passing the kappa-fit r^2 filter.
  # Long-format: 5 variables (kappa, s_mean, mu_dot, V, V_calc) x 2 cycles.
  decomposition_median_IQR <- kappa_grid |>
    filter(!is.na(kappa_1), r_sq_1 > kappa_r2, r_sq_2 > kappa_r2) |>
    pivot_longer(
      cols = matches("^(kappa|s_mean|mu_dot|V|V_calc)_(1|2)$"),
      names_to = c("variable", "cycle"),
      names_pattern = "^(.*)_(1|2)$",
      values_to = "value"
    ) |>
    group_by(variable, cycle) |>
    summarise(
      median = median(value, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    kappa_overlay = kappa_overlay_plot,
    kappa_segments = kappa_segments,
    decomposition_median_IQR = decomposition_median_IQR
  )
}

## Z-space analysis: standardise prices via monthly logistic cross-section,
## then produce binned + kernel-smoothed growth heatmaps in z-space.
## Only used for psqm deciles (n_quantiles = 10).
run_z_space <- function(data) {
  # Step 1: Fit logistic cross-section each month
  # For each month, regress median log-price of deciles on logit of empirical
  # quantile midpoint to get location mu(t) and scale s(t)
  monthly_logistic_fit <- data |>
    mutate(
      Date = floor_date(`Date of Transfer`, "month"),
      log_price = log(Price_inc_Stamp_Duty)
    ) |>
    group_by(Date) |>
    mutate(decile = ntile(Price_inc_Stamp_Duty, 10)) |>
    group_by(Date, decile) |>
    summarise(
      median_log_price = median(log(Price_inc_Stamp_Duty)),
      .groups = "drop_last"
    ) |>
    mutate(
      q_mid = (decile - 0.5) / 10,
      logit_q = log(q_mid / (1 - q_mid))
    ) |>
    summarise(
      fit = list(lm(median_log_price ~ logit_q)),
      .groups = "drop"
    ) |>
    mutate(
      mu = map_dbl(fit, ~ coef(.x)[1]),
      s = map_dbl(fit, ~ coef(.x)[2]),
      r_squared = map_dbl(fit, ~ summary(.x)$r.squared)
    ) |>
    dplyr::select(Date, mu, s, r_squared)

  # Step 2: Standardise each transaction
  standardised_transactions <- data |>
    mutate(
      Date = floor_date(`Date of Transfer`, "month"),
      log_price = log(Price_inc_Stamp_Duty)
    ) |>
    left_join(monthly_logistic_fit, by = "Date") |>
    mutate(z = (log_price - mu) / s)

  # Step 3: Bin by equal z intervals
  z_min <- -3.5
  z_max <- 3.5
  z_bin_width <- 0.7 # 10 bins spanning the main distribution
  z_breaks <- seq(z_min, z_max, by = z_bin_width)

  equal_z <- standardised_transactions |>
    mutate(z_bin = cut(z, breaks = z_breaks, include.lowest = TRUE)) |>
    filter(!is.na(z_bin)) |>
    group_by(z_bin, Date) |>
    summarise(
      median_price = median(Price_inc_Stamp_Duty),
      n_transactions = n(),
      .groups = "drop"
    ) |>
    mutate(
      z_lower = as.numeric(sub(".*?([-.0-9]+).*", "\\1", z_bin)),
      z_mid = z_lower + z_bin_width / 2
    ) |>
    arrange(z_bin, Date) |>
    group_by(z_bin) |>
    mutate(
      log_return = log(median_price / lag(median_price, n = 12)),
      `12-MA_log_return` = slide_dbl(
        log_return,
        mean,
        .before = 5,
        .after = 6,
        .complete = FALSE
      ),
      `2x12-MA_log_return` = slide_dbl(
        `12-MA_log_return`,
        mean,
        .before = 1,
        .after = 0,
        .complete = FALSE
      )
    ) |>
    ungroup() |>
    group_by(Date) |>
    mutate(growth_rank = row_number(desc(`2x12-MA_log_return`))) |>
    ungroup()

  # Binned heatmap: growth rank by z-bin
  equal_z_plot <- equal_z |>
    plot_ly(
      x = ~Date,
      y = ~z_mid,
      z = ~ -growth_rank,
      type = "heatmap",
      colorscale = "RdBu"
    ) |>
    heatmap_axis_layout() |>
    style(showscale = FALSE)

  # Step 4: Continuous surface — kernel-smoothed growth field g(z, t)
  z_grid <- seq(-3.5, 3.5, by = 0.1)
  t_months <- sort(unique(standardised_transactions$Date))

  kernel_bandwidth_z <- 1

  # Epanechnikov kernel (compact support, optimal MSE)
  kern <- function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)

  smoothed_surface <- map_dfr(t_months, function(this_date) {
    month_data <- standardised_transactions |>
      filter(Date == this_date) |>
      filter(!is.na(z), is.finite(z), z >= -4, z <= 4)

    if (nrow(month_data) < 50) {
      return(NULL)
    }

    # Nadaraya-Watson kernel regression: E[log_price | z]
    smoothed_log_price <- map_dbl(z_grid, function(z0) {
      weights <- kern((month_data$z - z0) / kernel_bandwidth_z)
      if (sum(weights) < 1e-10) {
        return(NA_real_)
      }
      weighted.mean(month_data$log_price, w = weights)
    })

    tibble(
      Date = this_date,
      z = z_grid,
      smoothed_log_price = smoothed_log_price
    )
  })

  # Growth field: annual log return at each z-grid point
  growth_surface <- smoothed_surface |>
    arrange(z, Date) |>
    group_by(z) |>
    mutate(
      g = smoothed_log_price - lag(smoothed_log_price, n = 12),
      g_12ma = slide_dbl(g, mean, .before = 5, .after = 6, .complete = FALSE),
      g_smooth = slide_dbl(
        g_12ma,
        mean,
        .before = 1,
        .after = 0,
        .complete = FALSE
      )
    ) |>
    ungroup()

  # Growth surface ranked heatmap
  growth_surface_ranked <- growth_surface |>
    filter(!is.na(g_smooth)) |>
    group_by(Date) |>
    mutate(
      growth_rank = rank(desc(g_smooth)),
      n_bins = n()
    ) |>
    ungroup()

  growth_surface_ranked_plot <- growth_surface_ranked |>
    plot_ly(
      x = ~Date,
      y = ~z,
      z = ~ -growth_rank,
      type = "heatmap",
      colorscale = "RdBu"
    ) |>
    heatmap_axis_layout() |>
    style(showscale = FALSE)

  # Combine both heatmaps into a subplot
  subplot(
    equal_z_plot,
    growth_surface_ranked_plot,
    nrows = 2,
    shareX = TRUE,
    shareY = FALSE
  )
}

z_equivalence <- function(monthly_median, log_logistic_params) {
  ## 1. Compute z_rank and z_price at every (Date, quantile) point ------------

  combined <- monthly_median |>
    mutate(
      Q = max(quantile),
      q = (quantile - 0.5) / Q,
      z_rank = qlogis(q),
      log_price = log(median_price)
    ) |>
    inner_join(
      log_logistic_params |> dplyr::select(Date, ln_mu, slope),
      by = "Date"
    ) |>
    mutate(
      z_price = (log_price - ln_mu) / slope,
      residual = z_price - z_rank
    )

  ## 2. TOP panel: equal-width z_price bins, growth rank -------------------

  z_bin_width <- 0.7
  z_breaks <- seq(-3.5, 3.5, by = z_bin_width)

  z_binned <- combined |>
    filter(!is.na(`2x12-MA_log_return`), is.finite(z_price)) |>
    mutate(z_bin = cut(z_price, breaks = z_breaks, include.lowest = TRUE)) |>
    filter(!is.na(z_bin)) |>
    group_by(Date, z_bin) |>
    summarise(
      log_return = mean(`2x12-MA_log_return`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(Date) |>
    mutate(growth_rank = row_number(desc(log_return))) |>
    ungroup() |>
    mutate(
      z_lower = as.numeric(sub(".*?(-?[0-9.]+).*", "\\1", as.character(z_bin))),
      z_mid = z_lower + z_bin_width / 2
    )

  z_bin_plot <- z_binned |>
    plot_ly(
      x = ~Date,
      y = ~z_mid,
      z = ~ -growth_rank,
      type = "heatmap",
      colorscale = "RdBu"
    ) |>
    heatmap_axis_layout() |>
    style(showscale = FALSE)

  ## 3. BOTTOM panel: kernel-smoothed continuous surface on a REGULAR z-grid.
  ##    Quantile medians are irregularly spaced in z_price (denser near z = 0),
  ##    so they cannot be drawn as a plotly heatmap directly — the grid cells
  ##    come back mostly empty and you get sparse speckle. We instead
  ##    Nadaraya-Watson smooth the growth field onto an evenly spaced z-grid,
  ##    then rank per Date (Epanechnikov kernel, bandwidth 1), driven by quantile
  ##    medians and z_price rather than raw transactions. Grid resolution (step
  ##    0.1) is fixed and independent of n_quantiles; finer input quantiles give
  ##    a denser, less noisy smooth, but the rendered grid is the same either way.

  z_grid <- seq(-3.5, 3.5, by = 0.1)
  kernel_bandwidth_z <- 1
  kern <- function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0) # Epanechnikov

  growth_input <- combined |>
    filter(!is.na(`2x12-MA_log_return`), is.finite(z_price))

  t_months <- sort(unique(growth_input$Date))

  smoothed_surface <- map_dfr(t_months, function(this_date) {
    md <- growth_input |> filter(Date == this_date)
    if (nrow(md) < 10) {
      return(NULL)
    }
    g_hat <- map_dbl(z_grid, function(z0) {
      w <- kern((md$z_price - z0) / kernel_bandwidth_z)
      if (sum(w) < 1e-10) {
        return(NA_real_)
      }
      weighted.mean(md$`2x12-MA_log_return`, w = w)
    })
    tibble(Date = this_date, z = z_grid, g = g_hat)
  })

  z_quantile_plot <- smoothed_surface |>
    filter(!is.na(g)) |>
    group_by(Date) |>
    mutate(growth_rank = rank(desc(g))) |>
    ungroup() |>
    plot_ly(
      x = ~Date,
      y = ~z,
      z = ~ -growth_rank,
      type = "heatmap",
      colorscale = "RdBu"
    ) |>
    heatmap_axis_layout() |>
    style(showscale = FALSE)

  z_space_combined <- subplot(
    z_bin_plot,
    z_quantile_plot,
    nrows = 2,
    shareX = TRUE,
    shareY = FALSE
  )

  ## 4. Equivalence verification: z_price vs z_rank at selected dates --------

  selected_dates <- as_date(c(
    "2007-08-01",
    "2010-01-01",
    "2020-01-01"
  ))

  equivalence_scatter <- combined |>
    filter(Date %in% selected_dates) |>
    ggplot(aes(x = z_rank, y = z_price, colour = factor(Date))) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey40"
    ) +
    geom_point(size = 0.6, alpha = 0.6) +
    labs(
      x = expression(z[rank] == log(q / (1 - q))),
      y = expression(z[price] == (l - mu) / s),
      colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ## 5. Residual heatmap: where the log-logistic structure deviates ----------
  ##    At fitted points, residual = OLS_residual(t, q) / s(t); this surfaces
  ##    the time- and quantile-localised deviations.

  residual_heatmap <- combined |>
    plot_ly(
      x = ~Date,
      y = ~quantile,
      z = ~residual,
      type = "heatmap",
      colorscale = "RdBu",
      zmid = 0
    ) |>
    heatmap_axis_layout()

  ## 6. Summary statistics ---------------------------------------------------

  ## NOTE on interior/tails split: the |z_rank| < 2 threshold partitions
  ## differently at different resolutions. At deciles, only the top and bottom
  ## decile fall in the tails bucket (2 of 10); at 317 quantiles the cut is much
  ## finer. Do not compare interior/tail numbers across the two resolution runs
  ## as if they measure the same band.
  summary_stats <- combined |>
    filter(is.finite(residual)) |>
    summarise(
      n_points = n(),
      correlation = cor(z_rank, z_price),
      mean_abs_residual_overall = mean(abs(residual)),
      max_abs_residual = max(abs(residual)),
      mean_abs_residual_interior = mean(abs(residual[abs(z_rank) < 2])),
      mean_abs_residual_tails = mean(abs(residual[abs(z_rank) >= 2]))
    )

  list(
    combined = combined,
    z_bin_plot = z_bin_plot,
    z_quantile_plot = z_quantile_plot,
    z_space_combined = z_space_combined,
    equivalence_scatter = equivalence_scatter,
    residual_heatmap = residual_heatmap,
    summary_stats = summary_stats
  )
}

## Combined 2x1: top row = Cullen-Frey year; top = ppd, bottom = psqm
plot_combined_cullen_frey <- function(plots) {
  right_panel_theme <- theme(axis.title.y = element_blank())

  wrap_plots(
    plots$LA$ppd$cullen_frey_year,
    plots$LA$psqm$cullen_frey_year,
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "bottom")
}

## Combined 2x2 log-logistic plot (patchwork)
## Top row: log-odds vs log-price; bottom row: slope parameter; left: ppd, right: psqm
plot_combined_log_logistic <- function(plots_Q) {
  # Suppress y-axis label on right-hand panels (already shown on left)
  right_panel_theme <- theme(axis.title.y = element_blank())

  wrap_plots(
    plots_Q$ppd$log_odds,
    plots_Q$psqm$log_odds + right_panel_theme,
    plots_Q$ppd$parameters,
    plots_Q$psqm$parameters + right_panel_theme,
    nrow = 2,
    guides = "collect"
  ) &
    theme(legend.position = "bottom")
}

## Print a patchwork (or any ggplot) safely: if the RStudio Plots pane is too small,
## fall back to a sized PNG written to a temp file and displayed via browseURL / file.show.
## width/height are in inches (passed to png()).
print_plot <- function(p, width = 14, height = 10) {
  tryCatch(
    print(p),
    error = function(e) {
      if (grepl("too small", conditionMessage(e), ignore.case = TRUE)) {
        tmp <- tempfile(fileext = ".png")
        png(tmp, width = width, height = height, units = "in", res = 150)
        print(p)
        dev.off()
        message("Plot saved to: ", tmp)
        if (interactive()) utils::browseURL(tmp)
      } else {
        stop(e)
      }
    }
  )
}

## Top-level function: runs the full analysis for both datasets and both groupings,
## then produces all combined 2x2 and 2x1 plots
run_all <- function(
  n_quantiles = 317,
  ma_type = "symmetric",
  static = TRUE,
  subplots = TRUE
) {
  datasets <- list(ppd = ppd, psqm = psqm)

  ## ---- Local Authority ----

  plots_LA <- imap(datasets, \(data, nm) {
    run_analysis(
      data,
      group_by_var = "Local Authority",
      n_quantiles = n_quantiles,
      ma_type = ma_type,
      static = static,
      subplots = subplots,
      price_xlim = if (nm == "psqm") 25000 else NULL
    )
  })

  ## ---- Quantile ----

  plots_Q <- imap(datasets, \(data, nm) {
    run_analysis(
      data,
      group_by_var = "quantile",
      n_quantiles = n_quantiles,
      ma_type = ma_type,
      static = static,
      subplots = subplots,
      price_xlim = if (nm == "psqm") 25000 else NULL
    )
  })

  ## ---- Combined 2x2 and 2x1 plots ----

  plots <- list(LA = plots_LA, Q = plots_Q)

  # Growth plots, do this for the Local Authorities and 317 quantiles only
  plot_combined_growth(plots) |> print_plot(width = 14, height = 10)

  # Growth rank plots, do this for both Local Authorities and quantiles
  plot_combined_rank(plots, n_quantiles = n_quantiles) |>
    print_plot(width = 14, height = 10)

  # Log return and growth rank (2x1 per dataset), reusing plots already computed by run_analysis
  plot_combined_log_return_rank(plots_LA$ppd, n_quantiles = n_quantiles) |>
    print_plot(width = 14, height = 6)
  plot_combined_log_return_rank(plots_LA$psqm, n_quantiles = n_quantiles) |>
    print_plot(width = 14, height = 6)
  plot_combined_log_return_rank(plots_Q$ppd, n_quantiles = n_quantiles) |>
    print_plot(width = 14, height = 6)
  plot_combined_log_return_rank(plots_Q$psqm, n_quantiles = n_quantiles) |>
    print_plot(width = 14, height = 6)

  # Adjacency matrices, do this for the Local Authorities and quantiles only
  plot_combined_adj(plots) |> print_plot(width = 14, height = 10)

  # Diagonal concentration time series
  plot_combined_diag_concentration(plots) |> print_plot(width = 14, height = 10)

  # Static combined plots (only produced when static = TRUE)
  if (static) {
    # Price pdf histogram by year — individual plots for ppd and psqm
    print_plot(plots_LA$ppd$price_pdf, width = 14, height = 10)
    print_plot(plots_LA$psqm$price_pdf, width = 14, height = 10)

    # 2x1: Cullen-Frey year; top=ppd, bottom=psqm
    print_plot(plot_combined_cullen_frey(plots), width = 14, height = 10)

    # 2x2 mean/sd and 2x2 PCA
    print_plot(plot_combined_mean_sd(plots), width = 14, height = 10)
    print_plot(plot_combined_pca(plots), width = 14, height = 10)

    # LA maps derived from ppd only (median price and k-means clusters)
    print_plot(plots_LA$ppd$map_median_price, width = 10, height = 10)
    print_plot(plots_LA$ppd$map_kmeans, width = 10, height = 10)

    # Spearman rank correlation vs first month (LA only, ppd left, psqm right)
    plot_combined_spearman(plots_LA) |> print_plot(width = 14, height = 10)

    # LA-only 2x2: Cullen-Frey price by LA (top) | log return by LA (bottom)
    plots_la_static <- list(
      ppd = analyse_static_la(ppd, plots_LA$ppd$monthly_median),
      psqm = analyse_static_la(psqm, plots_LA$psqm$monthly_median)
    )
    print_plot(
      plot_combined_la_static_2x2(plots_la_static),
      width = 14,
      height = 10
    )

    # 2x2 log return histogram (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
    print_plot(plot_combined_log_return_hist(plots), width = 14, height = 10)
  }

  ## ---- Log-logistic (quantile only) ----

  # Reuse the already-computed monthly_median from plots_Q to avoid calling create_monthly_median again
  plots_log_logistic <- map(plots_Q, \(p) run_log_logistic(p$monthly_median))
  parameters_ppd <<- plots_log_logistic$ppd$parameters_df
  parameters_psqm <<- plots_log_logistic$psqm$parameters_df
  # 2x2: top = log-odds vs log-price, bottom = slope parameter; left=ppd, right=psqm
  print_plot(
    plot_combined_log_logistic(plots_log_logistic),
    width = 14,
    height = 10
  )

  ## ---- Growth ridge (quantile psqm) ----
  ridge_psqm <- run_ridge(plots_Q$psqm$monthly_median)
  print_plot(ridge_psqm$profiles, width = 12, height = 5)
  print_plot(ridge_psqm$heatmap, width = 14, height = 5)
  
  ## ---- Decile capital trace (quantile psqm) ----
  decile_trace_psqm <- run_decile_capital_trace(ridge_psqm$ridge_df)
  print_plot(decile_trace_psqm$trace_plot, width = 12, height = 5)
  
  ## ---- Kappa (all groupings) ----

  # Basic kappa fits for 2x2 combined plot (all 4 combinations)
  plots_kappa <- list(
    LA = list(
      ppd = run_kappa(plots_LA$ppd$monthly_median, "Local Authority"),
      psqm = run_kappa(plots_LA$psqm$monthly_median, "Local Authority")
    ),
    Q = list(
      ppd = run_kappa(plots_Q$ppd$monthly_median, "quantile"),
      psqm = run_kappa(plots_Q$psqm$monthly_median, "quantile")
    )
  )
  print_plot(plot_combined_kappa(plots_kappa), width = 14, height = 10)
  
  # OLS-slope-basis cross-check of the grid decomposition below; supports
  # the residual claim. Single-window by design — it's a cross-check, not
  # a headline result.
  # decomp_terms_Q_psqm <<- fit_decomp_terms(plots_log_logistic$psqm$parameters_df, plots_kappa$Q$psqm$wave_peak)
  
  # Export per-cycle estimates and inferential tests to the global env
  # kappa_estimates_LA_ppd <<- plots_kappa$LA$ppd$kappa_estimates
  # kappa_estimates_LA_psqm <<- plots_kappa$LA$psqm$kappa_estimates
  # kappa_estimates_Q_ppd <<- plots_kappa$Q$ppd$kappa_estimates
  # kappa_estimates_Q_psqm <<- plots_kappa$Q$psqm$kappa_estimates # κ₁, κ₂ point estimates + OLS/HAC SEs

  # kappa_invariance_LA_ppd <<- plots_kappa$LA$ppd$kappa_invariance
  # kappa_invariance_LA_psqm <<- plots_kappa$LA$psqm$kappa_invariance
  # kappa_invariance_Q_ppd <<- plots_kappa$Q$ppd$kappa_invariance
  # kappa_invariance_Q_psqm <<- plots_kappa$Q$psqm$kappa_invariance # cross-cycle difference-in-coefficients z-test

  # mk_results_LA_ppd <<- plots_kappa$LA$ppd$mk_results
  # mk_results_LA_psqm <<- plots_kappa$LA$psqm$mk_results
  # mk_results_Q_ppd <<- plots_kappa$Q$ppd$mk_results
  # mk_results_Q_psqm <<- plots_kappa$Q$psqm$mk_results # Mann-Kendall τ per cycle

  # V-decomposition: needs wave_peak (from run_kappa) + parameters_df (from run_log_logistic).
  # Only meaningful for the quantile groupings since log-logistic is fit on quantiles.
  # fit_V_decomp_Q_ppd <- fit_V(
  #   plots_kappa$Q$ppd$wave_peak,
  #   plots_log_logistic$ppd$parameters_df
  # )
  fit_V_decomp_Q_psqm <- fit_V(
    plots_kappa$Q$psqm$wave_peak,
    plots_log_logistic$psqm$parameters_df
  )

  # V_estimates_Q_ppd <<- fit_V_decomp_Q_ppd$V_estimates
  # V_estimates_Q_psqm <<- fit_V_decomp_Q_psqm$V_estimates
  # s_estimates_Q_ppd <<- fit_V_decomp_Q_ppd$s_estimates
  # s_estimates_Q_psqm <<- fit_V_decomp_Q_psqm$s_estimates # tab:empirical_summary

  # Advanced kappa analysis: grid search + overlay (all four combinations).
  # kappa_detailed_LA_ppd <- run_kappa_detailed(
  #   plots_LA$ppd$monthly_median,
  #   "Local Authority",
  #   plots_log_logistic$ppd$parameters_df
  # )
  # kappa_detailed_LA_psqm <- run_kappa_detailed(
  #   plots_LA$psqm$monthly_median,
  #   "Local Authority",
  #   plots_log_logistic$psqm$parameters_df
  # )
  # kappa_detailed_Q_ppd <- run_kappa_detailed(
  #   plots_Q$ppd$monthly_median,
  #   "quantile",
  #   plots_log_logistic$ppd$parameters_df
  # )
  kappa_detailed_Q_psqm <- run_kappa_detailed(
    plots_Q$psqm$monthly_median,
    "quantile",
    plots_log_logistic$psqm$parameters_df
  )

  # kappa_segments_LA_ppd <<- kappa_detailed_LA_ppd$kappa_segments
  # kappa_segments_LA_psqm <<- kappa_detailed_LA_psqm$kappa_segments
  # kappa_segments_Q_ppd <<- kappa_detailed_Q_ppd$kappa_segments
  kappa_segments_Q_psqm <<- kappa_detailed_Q_psqm$kappa_segments # fig:10_psqm_kappa

  # Vsk_LA_ppd <<- kappa_detailed_LA_ppd$decomposition_median_IQR
  # Vsk_LA_psqm <<- kappa_detailed_LA_psqm$decomposition_median_IQR
  # Vsk_Q_ppd <<- kappa_detailed_Q_ppd$decomposition_median_IQR
  Vsk_Q_psqm <<- kappa_detailed_Q_psqm$decomposition_median_IQR # tab:empirical_summary

  # Grid-aggregated three-term decomposition of dℓ*/dt, on the same surviving
  # windows that feed Table 3. Per-row κ from kappa_segments enters only the
  # V≈μ̇−s̄κ comparison; drift, cross, intrinsic are κ-free. Identity-check
  # scalars (pooled median for the caption, per-variant max as correctness
  # guard) attached as attributes. tab:velocity_decomposition.
  wave_peak_velocity_decomposition_Q_psqm <<- 
    velocity_decomposition_grid(
      plots_log_logistic$psqm$parameters_df,
      plots_kappa$Q$psqm$wave_peak,
      kappa_detailed_Q_psqm$kappa_segments
  )

  print_plot(kappa_detailed_Q_psqm$kappa_overlay, width = 14, height = 5)

  # Z-space analysis (psqm deciles only)
  if (n_quantiles == 10) {
    z_space_plot <- run_z_space(datasets$psqm)
    print(z_space_plot)
  }

  # Dual z-space: z_price (from log-logistic fit) vs z_rank
  # z_equivalence_psqm <<- z_equivalence(
  #   plots_Q$psqm$monthly_median,
  #   plots_log_logistic$psqm$parameters_df
  # )
  # print(z_equivalence_psqm$summary_stats)
  # print(z_equivalence_psqm$equivalence_scatter)
  # print(z_equivalence_psqm$z_space_combined)
  # print(z_equivalence_psqm$residual_heatmap)

  invisible(NULL)
}

## Map animation: 2x12-MA log return per Local Authority
## Renders one PNG frame per month, stitches into an MP4, then deletes the PNGs.
## Requires: monthly_median (from create_monthly_median), uk_shapefile, magick, av
create_map_animation <- function(monthly_median) {
  animation_data <-
    monthly_median |>
    mutate(
      animation_return = pmin(
        pmax(`2x12-MA_log_return`, -0.18232156),
        0.18232156
      )
    ) |>
    dplyr::select(Date, `Local Authority`, animation_return) |>
    filter(!is.na(animation_return)) |>
    arrange(Date)

  ## Build a symmetric colour scale fixed across all frames
  colour_limits <- c(-0.18232156, 0.18232156)

  ## Pre-split by date so each iteration receives its slice directly
  animation_data_split <- split(animation_data, animation_data$Date)

  ## Render one PNG per month (walk() for side effects, no return value needed)
  walk(animation_data_split, \(month_data) {
    d <- as_date(month_data$Date[[1]])
    month_label <- format(d, "%B %Y")

    month_sf <-
      uk_shapefile |>
      left_join(
        month_data |> dplyr::select(`Local Authority`, animation_return),
        by = c("LAD24NM" = "Local Authority")
      )

    p <-
      month_sf |>
      ggplot() +
      geom_sf(aes(fill = animation_return), colour = NA) +
      scale_fill_distiller(
        palette = "RdBu",
        limits = colour_limits,
        name = "2×12-MA log return",
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(title = month_label) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "bottom"
      )

    out_file <- file.path(
      "Graphs/Map animation",
      paste0("frame_", format(d, "%Y_%m"), ".png")
    )
    ggsave(out_file, p, width = 7, height = 9, dpi = 150, bg = "white")
    message("Saved: ", out_file)
  })

  ## Stitch frames into an MP4 video
  frames_paths <- sort(list.files(
    "Graphs/Map animation",
    pattern = "\\.png$",
    full.names = TRUE
  ))

  av_encode_video(
    input = frames_paths,
    output = "Graphs/Map animation/Local Authority log returns.mp4",
    framerate = 5
  )

  ## Delete PNG frames now that the video has been created
  file.remove(frames_paths)
}

## Function to plot counts of tail transactions over time using a 12-month rolling window
plot_tail_transactions <- function(ppd) {
  # Add a month date
  ppd <-
    ppd |>
    mutate(Date = floor_date(`Date of Transfer`, "month"))

  # Per-month price list, used as the rolling-window input
  monthly_prices <-
    ppd |>
    group_by(Date) |>
    summarise(prices = list(Price_inc_Stamp_Duty), .groups = "drop") |>
    arrange(Date)

  # 12-month window (.before = 5, .after = 6), then 2x12 over those windows
  rolling_bounds <-
    monthly_prices |>
    mutate(
      window_12 = slide(
        prices,
        ~ unlist(.x),
        .before = 5,
        .after = 6,
        .complete = FALSE
      ),
      window_prices = slide(
        window_12,
        ~ unlist(.x),
        .before = 1,
        .after = 0,
        .complete = FALSE
      )
    )

  # Quantiles to plot
  qs <- c(1e-3, 1e-4, 1e-5)
  q_levels <- c("10^-3", "10^-4", "10^-5")

  # For each q, compute lower/upper thresholds per month and count tail transactions
  extreme_counts <-
    map2_dfr(qs, q_levels, function(q, lab) {
      bounds_df <-
        rolling_bounds |>
        mutate(
          lower = map_dbl(window_prices, ~ quantile(.x, q, na.rm = TRUE)),
          upper = map_dbl(window_prices, ~ quantile(.x, 1 - q, na.rm = TRUE))
        ) |>
        dplyr::select(Date, lower, upper)

      ppd |>
        inner_join(bounds_df, by = "Date") |>
        mutate(
          tail = case_when(
            Price_inc_Stamp_Duty <= lower ~ "Lower",
            Price_inc_Stamp_Duty >= upper ~ "Upper",
            TRUE ~ NA_character_
          )
        ) |>
        filter(!is.na(tail)) |>
        count(Date, tail, name = "n") |>
        complete(
          Date = bounds_df$Date,
          tail = c("Lower", "Upper"),
          fill = list(n = 0)
        ) |>
        mutate(q_label = lab)
    }) |>
    mutate(q_label = factor(q_label, levels = q_levels))

  print(
    ggplot(
      extreme_counts,
      aes(x = Date, y = n, colour = q_label, linetype = tail)
    ) +
      geom_line() +
      scale_colour_discrete(labels = scales::label_parse()) +
      labs(
        x = NULL,
        y = "Number of transactions",
        colour = "q",
        linetype = "Tail"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  )
}

##  ---- Run ----
#
#   Parameters
#     n_quantiles : 317 = all LAs individually; < 317 = bin into ntiles
#     ma_type     : "symmetric" | "predictive"
#     static      : TRUE = produce static (non-plotly) plots; FALSE = skip
#     subplots    : TRUE = produce region & property-type subplots (slow, quantile only)
#
#   All runs — output order:
#     1.  2x2  Growth heat map                        (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     2.  2x2  Growth rank heat map                   (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     3.  2x1  Log return + rank  ×4                  (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     4.  2x2  Adjacency matrix                       (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     5.  2x1  Diagonal concentration time series     (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     6.  2x2  Log-logistic: log-odds (top), slope (bottom)  (ppd, psqm)
#     7.  2x2  Kappa: wave peak log-odds z*(t)        (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     8.    1  Kappa: advanced overlay                (Q/psqm)
#     9.  1x2  Z-space growth heatmaps (deciles only) (Q/psqm)
#
#     Commented out above as too slow and only a robustness check
#     10.   1  Equivalence of z[rank] and z[price]    (Q/psqm)
#     11. 1x2  Z-space equivalents growth heatmaps    (Q/psqm)
#     12.   1  Z-space equivalents residuals          (Q/psqm)
#
#   static = TRUE — additional outputs (after #4, before #5):
#     1.    1  Price pdf histogram by year            (ppd)
#     2.    1  Price pdf histogram by year            (psqm)
#     3.  2x2  Cullen-Frey year                       (ppd, psqm)
#     4.  2x2  Mean & SD of log return                (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     5.  2x2  PCA k-means clusters                   (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#     6.  map  Median price map                       (LA/ppd)
#     7.  map  K-means cluster map                    (LA/ppd)
#     8.  2x2  Spearman rho + rank scatter            (LA/ppd, LA/psqm)
#     9.  2x2  Cullen-Frey: price by LA (top) | log return by LA (bottom)  (ppd, psqm)
#     10. 2x2  Log return histogram                   (LA/ppd, LA/psqm, Q/ppd, Q/psqm)
#
#   subplots = TRUE — additional outputs (quantile grouping only):
#     Region subplots, Property type subplots
#

run_all(
  n_quantiles = 317,
  ma_type = "symmetric",
  static = TRUE,
  subplots = FALSE
)

# run_all(n_quantiles = 317, ma_type = "predictive", static = FALSE, subplots = FALSE)

# run_all(n_quantiles = 10, ma_type = "symmetric", static = FALSE, subplots = TRUE)
# run_all(n_quantiles = 10, ma_type = "predictive", static = FALSE, subplots = FALSE)

## Standalone function call for the map animation, as it takes time to run
# monthly_median_la <- create_monthly_median(ppd, group_by_var = "Local Authority")
# create_map_animation(monthly_median_la)

## Standalone function call for the tail transactions plot, as it takes time to run
# plot_tail_transactions(ppd)

## Decomposition of the Growth Field, harmonic mean slope values for cycle 1 and cycle 2
parameters_psqm |> filter(Date <= "2009-03-01") |> summarise(h_mean = n() / sum(1 / slope, na.rm = TRUE))
parameters_psqm |> filter(Date >= "2009-03-01") |> summarise(h_mean = n() / sum(1 / slope, na.rm = TRUE))

## Values for 'Decomposition of the Wave Peak Velocity'
attr(wave_peak_velocity_decomposition_Q_psqm, "identity_check_median_abs_resid")          # pooled median |exact_sum − measured|
attr(wave_peak_velocity_decomposition_Q_psqm, "identity_check_max_abs_resid_by_variant")  # raw / smoothed maxima - the latter is used in the text
attr(wave_peak_velocity_decomposition_Q_psqm, "n_windows") # Check that all windows of Figure 2 are used.
