# Re-start R session to ensure only the necessary libraries are attached
library(tidyverse)
library(slider)
library(plotly)
library(ggrepel)
library(janitor)
library(igraph)

options(scipen = 100)

setwd("/Users/chrisbeech/Documents/UK housing analysis")

## Read back data

price_paid_data <-
  read_csv("Data/pp-stampduty.csv") # or pp-stampduty-psqm

# Add a little bit of noise to the price_paid_data to ensure repeated values don't stop the median being calculated (not necessary for pp-stampduty-psqm)
price_paid_data$Price_inc_Stamp_Duty <-
  price_paid_data$Price_inc_Stamp_Duty +
  rnorm(nrow(price_paid_data), mean = 0, sd = 1)

## Calculate median, log return, and rank

monthly_median <-
  price_paid_data |>
  mutate(Date = floor_date(`Date of Transfer`, "month")) |>
  group_by(`Local Authority`, Date) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) |> # or Price_per_inc_Stamp_Duty
  mutate(log_return = log(median_price / lag(median_price, n = 12))) |> # n = 12 for annual, n = 1 for monthly
  mutate(
    `12-MA_log_return` = slide_dbl(
      log_return,
      mean,
      .before = 5,
      .after = 6,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    ),
    `2x12-MA_log_return` = slide_dbl(
      `12-MA_log_return`,
      mean,
      .before = 1,
      .after = 0,
      .complete = FALSE # we use FALSE here to have a value up to the latest month
    )
  ) |>
  ungroup() |>
  group_by(Date) |>
  mutate(Rank = row_number(desc(`2x12-MA_log_return`))) |>
  ungroup()

# A different approach to the moving average (ensure Rank uses log_return):
#  mutate(loess_fit = predict(loess(median_price ~ as.numeric(row_number()), span = 0.06)), log_return = log(loess_fit / lag(loess_fit, n = 12)))

## Re-order the Local Authorities using the 2025 median; use levels(monthly_median$`Local Authority`) to see the effect

median_prices <-
  price_paid_data |>
  filter(`Date of Transfer` >= "2025-01-01") |>
  group_by(`Local Authority`) |>
  summarise(median_price = median(Price_inc_Stamp_Duty)) # or Price_per_inc_Stamp_Duty

ordered_local_authorities <-
  with(median_prices, reorder(`Local Authority`, median_price))

monthly_median$`Local Authority` <-
  factor(
    monthly_median$`Local Authority`,
    levels = levels(ordered_local_authorities)
  )

## Deciles only

# 1. Create ordered_local_authorities, as above
price_paid_data$`Local Authority` <-
  factor(
    price_paid_data$`Local Authority`,
    levels = levels(ordered_local_authorities)
  )

# 2. Replace `Local Authority` in price_paid_data with a decile
price_paid_data <-
  price_paid_data |>
  mutate(`Local Authority` = ntile(as.numeric(`Local Authority`), 10))

# 3. Calculate monthly_median, as above, using this revised price_paid_data, then plot as below

## Heatmaps

# Reduce the scale slightly, thereby increasingly the contrast
monthly_median <-
  monthly_median |>
  mutate(
    display_return = case_when(
      `2x12-MA_log_return` > 0.2623643 ~ 0.2623643,
      `2x12-MA_log_return` < -0.2623643 ~ -0.2623643,
      TRUE ~ `2x12-MA_log_return`
    )
  )

# Plot log return
monthly_median |>
  plot_ly(
    x = ~Date,
    y = ~`Local Authority`,
    z = ~display_return,
    zmid = 0,
    type = "heatmap",
    colorscale = "RdBu"
  ) |>
  colorbar(
    title = "Log return",
    tickvals = c(-0.2623643, -0.1397619, 0, 0.1397619, 0.2623643),
    ticktext = c("-30%", "-15%", "0%", "15%", "30%"),
    orientation = "h",
    thickness = 10,
    x = 0.44675,
    y = -0.05
  ) |>
  layout(
    # title = list(
    #   text = "Moving average of the annual log return of the monthly median, by Local Authority", # adjust the title as necessary
    #   font = list(family = "Helvetica", size = 20),
    #   x = 0.063
    # ),
    # margin = list(t = 33),
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "Local Authority", ticks = "", showticklabels = FALSE) #,
    # annotations = list(
    #   list(
    #     x = 0,
    #     y = 1,
    #     text = "Highest median price",
    #     showarrow = FALSE,
    #     xref = "paper",
    #     yref = "paper",
    #     textangle = -90,
    #     yanchor = "top"
    #   ),
    #   list(
    #     x = 0,
    #     y = 0,
    #     text = "Lowest median price",
    #     showarrow = FALSE,
    #     xref = "paper",
    #     yref = "paper",
    #     textangle = -90,
    #     yanchor = "bottom"
    #   )
    # )
  )

# Plot rank
legend_numbers <- seq(50, 300, by = 50)
# legend_numbers <- seq(1, 10, by = 1) # for deciles

monthly_median |>
  plot_ly(
    x = ~Date,
    y = ~`Local Authority`,
    z = ~ -Rank,
    type = "heatmap",
    colorscale = "RdBu"
  ) |>
  colorbar(
    title = "Return rank",
    tickvals = -unique(legend_numbers),
    ticktext = unique(legend_numbers),
    orientation = "h",
    thickness = 10,
    x = 0.44675,
    y = -0.05
  ) |>
  layout(
    # title = list(
    #   text = "Rank of the moving average of the annual return of the monthly median, by Local Authority", # adjust the title as necessary
    #   font = list(family = "Helvetica", size = 20),
    #   x = 0.063
    # ),
    # margin = list(t = 33),
    xaxis = list(title = "", tickformat = "%Y", range = c("1996", "2026")),
    yaxis = list(title = "Local Authority", ticks = "", showticklabels = FALSE) #,
    # annotations = list(
    #   list(
    #     x = 0,
    #     y = 1,
    #     text = "Highest median price",
    #     showarrow = FALSE,
    #     xref = "paper",
    #     yref = "paper",
    #     textangle = -90,
    #     yanchor = "top"
    #   ),
    #   list(
    #     x = 0,
    #     y = 0,
    #     text = "Lowest median price",
    #     showarrow = FALSE,
    #     xref = "paper",
    #     yref = "paper",
    #     textangle = -90,
    #     yanchor = "bottom"
    #   )
    # )
  )

## Mean and standard deviation of median price log_return

mean_sd <-
  monthly_median |>
  #  filter(Date >= "1996-01-01" & Date < "1997-01-01") |>
  filter(!is.na(log_return)) |>
  group_by(`Local Authority`) |>
  summarise(Mean = mean(log_return), `Standard Deviation` = sd(log_return))

mean_sd$`Local Authority` <- as.numeric(mean_sd$`Local Authority`) # Convert to a number, else theme_minimal() doesn't work!

mean_sd |>
  pivot_longer(
    cols = c(Mean, `Standard Deviation`),
    names_to = "variable",
    values_to = "Summary statistics"
  ) |>
  ggplot(aes(
    x = `Summary statistics`,
    y = `Local Authority`,
    colour = variable
  )) +
  geom_point(size = 1) +
  coord_cartesian(clip = 'off') +
  labs(
    # title = "Mean and standard deviation of the log return of the median price, by Local Authority",
    x = "Summary statistics",
    y = "Local Authority",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

## Principal Component Analysis K-Means Clustering

# Scale the data
cluster_data_scaled <-
  monthly_median |>
  arrange(desc(`Local Authority`)) |>
  filter(!is.na(log_return)) |>
  select(`Local Authority`, Date, log_return) |>
  pivot_wider(names_from = Date, values_from = log_return) |>
  column_to_rownames("Local Authority") |>
  as.matrix() |>
  scale()

# Determine optimal k
wss <-
  map_dbl(
    1:10,
    ~ kmeans(cluster_data_scaled, centers = .x, nstart = 1)$tot.withinss
  ) |>
  enframe(name = "k", value = "wss")

wss |>
  ggplot(aes(x = k, y = wss)) +
  geom_point() +
  geom_line() +
  labs(
    # title = "K-Means Elbow Plot",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

# K-Means Clustering
# set.seed(123) # seems to work fine without this
kmeans_result <- kmeans(cluster_data_scaled, centers = 2) # use the whole data set to preserve all original features

# Principal Components Analysis
pca_result <- prcomp(cluster_data_scaled)

# Extract variance
variance_explained <-
  pca_result$sdev^2 |>
  (\(x) x / sum(x))() |>
  enframe(name = "PC", value = "Variance_Explained")

# Principal Component Elbow plot
variance_explained |>
  slice_head(n = 10) |>
  ggplot(aes(x = PC, y = Variance_Explained)) +
  geom_point() +
  geom_line() +
  labs(
    # title = "Principal Component Elbow Plot",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

# Prepare and plot
pca_scores <- as_tibble(pca_result$x, rownames = "Local Authority")

pca_scores |>
  ggplot(aes(
    x = PC2,
    y = PC1,
    label = `Local Authority`,
    colour = as.factor(kmeans_result$cluster)
  )) +
  geom_point() +
  geom_text_repel(
    data = pca_scores,
    aes(x = PC2, y = PC1, label = `Local Authority`),
    size = 3
  ) +
  coord_cartesian(clip = 'off') +
  # labs(
  #   title = "Principal Component Analysis, K-Means Clustering, by Local Authority"
  # ) +
  theme_minimal() +
  theme(legend.position = "none")

## Network graph

# Convert to a number to match quantile - needed for the 2x2 plots
monthly_median$`Local Authority` <- as.numeric(monthly_median$`Local Authority`)

# Spatial edges - Rank pairs r and r + 1 at time t
spatial_edges <-
  monthly_median |>
  select(`Local Authority`, Date, Rank) |>
  filter(!is.na(Rank)) |>
  arrange(Date, Rank) |>
  group_by(Date) |>
  mutate(to = lead(`Local Authority`)) |>
  ungroup() |>
  filter(!is.na(to)) |>
  mutate(type = "spatial") |>
  select(from = `Local Authority`, to, type)

# Temporal edges - Rank at times t and t + 1
temporal_edges <-
  monthly_median |>
  select(`Local Authority`, Date, Rank) |>
  filter(!is.na(Rank)) |>
  arrange(Rank, Date) |>
  group_by(Rank) |>
  mutate(to = lead(`Local Authority`)) |>
  ungroup() |>
  filter(!is.na(to)) |>
  mutate(type = "temporal") |>
  select(from = `Local Authority`, to, type)

# Combine both edge types
edges <- bind_rows(spatial_edges, temporal_edges)

# Count frequency of each connection
edge_weights <-
  edges |>
  group_by(from, to) |>
  summarize(weight = n(), .groups = "drop") |>
  mutate(weight = weight / max(weight))

# Quick summary of weights
tabyl(edge_weights$weight)

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

row_ids <- rownames(adj_matrix)
col_ids <- colnames(adj_matrix)

row_ids_factor <- factor(row_ids, levels = row_ids)
col_ids_factor <- factor(col_ids, levels = col_ids)

adj_matrix_long <-
  data.frame(
    x = rep(col_ids_factor, each = length(row_ids_factor)),
    y = rep(row_ids_factor, times = length(col_ids_factor)),
    value = as.vector(adj_matrix)
  )

# Plot adjacency matrix
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
    # title = list(
    #   text = "Spatiotemporal adjacency matrix of the ranks, by Local Authority",
    #   font = list(family = "Helvetica", size = 20),
    #   x = 0.0475
    # ),
    # margin = list(t = 33),
    xaxis = list(
      title = "Local Authority",
      autorange = "reversed",
      ticks = "",
      showticklabels = FALSE
    ),
    yaxis = list(
      title = "Local Authority",
      ticks = "",
      showticklabels = FALSE
    ),
    annotations = list(
      list(
        x = -0.02,
        y = 1,
        text = "Highest median price",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        textangle = -90,
        yanchor = "top"
      ),
      list(
        x = -0.02,
        y = 0,
        text = "Lowest median price",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        textangle = -90,
        yanchor = "bottom"
      ),
      list(
        x = 0,
        y = -0.025,
        text = "Highest median price",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "bottom"
      ),
      list(
        x = 1,
        y = -0.025,
        text = "Lowest median price",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "bottom"
      )
    )
  )
