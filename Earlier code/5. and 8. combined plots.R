## We assume that '5. Local Authority analysis' and '8. Quantile analysis' are open and each monthly_median is created in turn for pp-stampduty and pp-stampduty-psqm

## Log return plots

# Or assign to LA_psqm
LA <-
  monthly_median |>
  plot_ly(
    x = ~Date,
    y = ~`Local Authority`,
    z = ~display_return,
    zmid = 0,
    type = "heatmap",
    colorscale = "RdBu",
    showscale = FALSE
  ) |>
  layout(
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "", ticks = "", showticklabels = FALSE)
  )

# Or assign to Q_psqm
Q <-
  monthly_median |>
  plot_ly(
    x = ~Date,
    y = ~quantile,
    z = ~display_return,
    zmid = 0,
    type = "heatmap",
    colorscale = "RdBu",
    showscale = FALSE
  ) |>
  layout(
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "", ticks = "", showticklabels = FALSE)
  )

# Combine all plots into a subplot
subplot_combined <-
  subplot(
    LA,
    LA_psqm,
    Q,
    Q_psqm,
    nrows = 2,
    shareX = TRUE,
    shareY = FALSE
  )

# Dummy trace for shared colorbar only
colorbar <-
  plot_ly(
    z = matrix(c(-0.2623643, 0.2623643), nrow = 2),
    type = "heatmap",
    colorscale = "RdBu",
    showscale = TRUE,
    opacity = 0, # hide the heatmap itself
    colorbar = list(
      title = "Log return",
      tickvals = c(-0.2623643, -0.1397619, 0, 0.1397619, 0.2623643),
      ticktext = c("-30%", "-15%", "0%", "15%", "30%"),
      orientation = "h",
      thickness = 10,
      x = 0.44675,
      y = 0
    )
  ) |>
  layout(
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )

# Use subplot and add the shared colorbar trace
subplot(subplot_combined, colorbar, nrows = 2, heights = c(0.95, 0.025))


## Rank plots

# Or assign to LA_psqm_rank
LA_rank <-
  monthly_median |>
  plot_ly(
    x = ~Date,
    y = ~`Local Authority`,
    z = ~ -Rank,
    type = "heatmap",
    colorscale = "RdBu",
    showscale = FALSE
  ) |>
  layout(
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "", ticks = "", showticklabels = FALSE)
  )

# Or assign to Q_psqm_rank
Q_rank <-
  monthly_median |>
  plot_ly(
    x = ~Date,
    y = ~quantile,
    z = ~ -Rank,
    type = "heatmap",
    colorscale = "RdBu",
    showscale = FALSE
  ) |>
  layout(
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "", ticks = "", showticklabels = FALSE)
  )

# Plot rank
legend_numbers <- seq(50, 300, by = 50)
legend_numbers <- seq(1, 10, by = 1) # for deciles

# Dummy trace for shared colorbar only
colorbar <-
  plot_ly(
    z = matrix(c(-317, -1), nrow = 2), # or -10 for deciles
    type = "heatmap",
    colorscale = "RdBu",
    showscale = TRUE,
    opacity = 0, # hide the heatmap itself
    colorbar = list(
      title = "Return rank",
      tickvals = -unique(legend_numbers),
      ticktext = unique(legend_numbers),
      orientation = "h",
      thickness = 10,
      x = 0.44675,
      y = 0
    )
  ) |>
  layout(
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )

# Combine all plots into a subplot
subplot_combined <-
  subplot(
    LA_rank,
    LA_psqm_rank,
    Q_rank,
    Q_psqm_rank,
    nrows = 2,
    shareX = TRUE,
    shareY = FALSE
  )

# Use subplot and add the shared colorbar trace
subplot(subplot_combined, colorbar, nrows = 2, heights = c(0.95, 0.025))

## Log return and rank deciles

subplot_combined <-
  subplot(
    Q_psqm,
    Q_psqm_rank,
    nrows = 2,
    shareX = TRUE,
    shareY = FALSE
  )

subplot_combined

## Adjacency matrices

# Plot adjacency matrix, assign to LA_adj, LA_psqm_adj, Q_adj, Q_psqm_adj
LA_adj <-
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

# Combine all plots into a subplot
subplot_combined <-
  subplot(
    LA_adj,
    LA_psqm_adj,
    Q_adj,
    Q_psqm_adj,
    nrows = 2,
    shareX = TRUE,
    shareY = TRUE
  )

subplot_combined
