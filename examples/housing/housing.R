library(dplyr)
library(tidyr)

od <- readr::read_rds("examples/housing/_data/metro.rds")
d <- unnest(od, cols = c(data))

# sale_stats <- d |>
#   filter(metric == "Median Sale Price") |>
#   group_by(metro, state) |>
#   mutate(n = n()) |>
#   filter(n >= 25) |>
#   summarise(
#     # max_date = max(date),
#     # n = n(),
#     # max_ddiff = max(diff(date), na.rm = TRUE)
#     sale_latest = nth(value, -1),
#     sale_mean = mean(value),
#     sale_pct_chg_mnth = 100 * (nth(value, -1) - nth(value, -2)) / nth(value, -2),
#     sale_pct_chg_yr = 100 * (nth(value, -1) - nth(value, -12)) / nth(value, -12),
#     sale_pct_chg_2yr = 100 * (nth(value, -1) - nth(value, -24)) / nth(value, -24),
#     .groups = "drop"
#   )

list_stats <- d |>
  filter(metric == "Median List Price") |>
  group_by(metro, state) |>
  mutate(n = n()) |>
  filter(n >= 60) |>
  summarise(
    # max_date = max(date),
    # n = n(),
    # max_ddiff = max(diff(date), na.rm = TRUE)
    list_latest = nth(value, -1),
    list_mean = mean(value),
    list_pct_chg_mnth = 100 * (nth(value, -1) - nth(value, -2)) / nth(value, -2),
    list_pct_chg_yr = 100 * (nth(value, -1) - nth(value, -12)) / nth(value, -12),
    list_pct_chg_2yr = 100 * (nth(value, -1) - nth(value, -24)) / nth(value, -24),
    .groups = "drop"
  )

rent_stats <- d |>
  filter(metric == "ZORI All Homes") |>
  group_by(metro, state) |>
  mutate(n = n()) |>
  filter(n >= 60) |>
  summarise(
    # max_date = max(date),
    # n = n(),
    # max_ddiff = max(diff(date), na.rm = TRUE)
    rent_latest = nth(value, -1),
    rent_mean = mean(value),
    rent_pct_chg_mnth = 100 * (nth(value, -1) - nth(value, -2)) / nth(value, -2),
    rent_pct_chg_yr = 100 * (nth(value, -1) - nth(value, -12)) / nth(value, -12),
    rent_pct_chg_2yr = 100 * (nth(value, -1) - nth(value, -24)) / nth(value, -24),
    .groups = "drop"
  )

rent_price_stats <- d |>
  filter(metric %in% c("ZHVI Single-Family", "ZORI All Homes")) |>
  mutate(metric = recode(metric,
    "ZHVI Single-Family" = "zhvi", "ZORI All Homes" = "zori")) |>
  group_by(metro, state, metric) |>
  slice_max(date) |>
  ungroup() |>
  # last dates might not always match across list/price but oh well
  select(-date) |>
  pivot_wider(names_from = "metric", values_from = "value") |>
  mutate(price2rent_ratio = zhvi / (12 * zori)) |>
  select(-zhvi, -zori)

list_sold_stats <- d |>
  filter(metric %in% c("Median List Price", "Median Sale Price")) |>
  mutate(metric = recode(metric,
    "Median List Price" = "list", "Median Sale Price" = "sale")) |>
  group_by(metro, state, metric) |>
  slice_max(date) |>
  ungroup() |>
  # last dates might not always match across list/price but oh well
  select(-date) |>
  pivot_wider(names_from = "metric", values_from = "value") |>
  mutate(sale_list_pct_diff = 100 * (sale - list) / list) |>
  select(-list, -sale)

inv_stats <- d |>
  filter(metric == "For-Sale Inventory") |>
  group_by(metro, state) |>
  mutate(n = n()) |>
  filter(n >= 60) |>
  summarise(
    inv_latest = nth(value, -1),
    inv_mean = mean(value),
    inv_pct_chg_mnth = 100 * (nth(value, -1) - nth(value, -2)) / nth(value, -2),
    inv_pct_chg_yr = 100 * (nth(value, -1) - nth(value, -12)) / nth(value, -12),
    inv_pct_chg_2yr = 100 * (nth(value, -1) - nth(value, -24)) / nth(value, -24),
    .groups = "drop"
  )

cut_stats <- d |>
  filter(metric == "Share of Listings With a Price Cut") |>
  group_by(metro, state) |>
  mutate(n = n()) |>
  filter(n >= 60) |>
  summarise(
    cut_latest = 100 * nth(value, -1),
    cut_mean = mean(100 * value),
    cut_pct_chg_mnth = 100 * (nth(value, -1) - nth(value, -2)) / nth(value, -2),
    cut_pct_chg_yr = 100 * (nth(value, -1) - nth(value, -12)) / nth(value, -12),
    cut_pct_chg_2yr = 100 * (nth(value, -1) - nth(value, -24)) / nth(value, -24),
    .groups = "drop"
  )

pd <- list_stats |>
  left_join(list_sold_stats, by = c("metro", "state")) |>
  left_join(inv_stats, by = c("metro", "state")) |>
  left_join(cut_stats, by = c("metro", "state")) |>
  left_join(rent_price_stats, by = c("metro", "state")) |>
  left_join(od, by = c("metro", "state"))

pd <- pd |>
  mutate(zillow_link = href(sprintf("http://www.zillow.com/homes/%s_rb/",
    gsub(", ", "-", metro))))


# filter(d, metric == "Share of Listings With a Price Cut") |> pull(value) |> range()
# price cut goes from 0 to 50

# filter(d, metric == "Median List Price") |> pull(value) |> log10() |> hist()
# filter(d, metric == "For-Sale Inventory") |> pull(value) |> log10() |> hist()

# d |>
#   filter(metric == "For-Sale Inventory") |>
#   group_by(metro, state) |>
#   summarise(dff = diff(range(value)), .groups = "drop") |>
#   pull(dff) |>
#   hist()

prng <- filter(d, metric == "Median List Price") |> pull(value) |> range()
irng <- filter(d, metric == "For-Sale Inventory") |> pull(value) |> range()
drng <- range(d$date)
drng[1] <- as.Date("2005-01-01") # don't go all the way back to 2000
drng[1] <- drng[1] + 10

plot_fn <- function(metro, data) {
  x <- data[[1]]

  bb <- list(
    type = "rect",
    x0 = 0, x1 = 1, xref = "paper",
    y0 = 0, y1 = 1, yref = "paper",
    line = list(width = 1, color = "#444444")
  )

  library(plotly)
  f1 <- plot_ly() |>
    add_lines(data = filter(x, metric == "ZHVI Single-Family"),
      type = "scatter", mode = "markers+lines", x = ~ date, y = ~ value,
      name = "Zillow Home Value Index (Single-Family)", color = I("black"), opacity = 0.5
    ) |>
    add_trace(data = filter(x, metric == "Median Sale Price"),
      type = "scatter", mode = "markers+lines", x = ~ date, y = ~ value,
      name = "Median Sale Price"
    ) |>
    add_trace(data = filter(x, metric == "Median List Price"),
      type = "scatter", mode = "markers+lines", x = ~ date, y = ~ value,
      name = "Median List Price"
    ) |>
    layout(
      yaxis = list(title = "Price", type = "log", range = log10(prng)),
      shapes = list(bb),
      legend = list(x = 0.001, y = 0.999)
    )
  f2 <- plot_ly() |>
    add_trace(data = filter(x, metric == "For-Sale Inventory"),
      type = "scatter", mode = "markers+lines", x = ~ date, y = ~ value,
      name = "For-Sale Inventory"
  ) |>
    layout(
      yaxis = list(title = "Inventory"), # type = "log", range = log10(irng)),
      shapes = list(bb)
    )
  f3 <- plot_ly() |>
    add_trace(data = filter(x, metric == "Share of Listings With a Price Cut"),
      type = "scatter", mode = "markers+lines", x = ~ date, y = ~ 100 * value,
      name = "Share of Listings With a Price Cut"
  ) |>
    layout(
      yaxis = list(title = "% Listings w/ Price Cut",
        range = list(0, 50)),
      shapes = list(bb)
    )
  subplot(list(f1, f2, f3), nrows = 3, shareX = TRUE, titleY = TRUE,
    heights = c(0.6, rep(0.2, 2)), margin = 0) |>
    layout(
      xaxis = list(title = NULL, range = c("2005-01-01", NA)),
      hovermode = "x unified",
      # showlegend = FALSE,
      margin = list(r = 3, l = 3, t = 3, b = 0, pad = 0)
    ) |>
    config(displaylogo = FALSE)
}

pd <- pd |>
  mutate(plot = panel_lazy(plot_fn))

pd <- set_var_labels(pd,
  metro = "Metropolitan area",
  state = "State",
  list_latest = "Latest median list price",
  list_mean = "Mean of all median list prices",
  list_pct_chg_mnth = "Percent change in median list price (month)",
  list_pct_chg_yr = "Percent change in median list price (year)", 
  list_pct_chg_2yr = "Percent change in median list price (2 years)",
  sale_list_pct_diff = "Percent difference between latest median sale and list price",
  inv_latest = "Latest for-sale inventory",
  inv_mean = "Mean of monthly for-sale inventory values",
  inv_pct_chg_mnth = "Percent change in for-sale inventory (month)",
  inv_pct_chg_yr = "Percent change in for-sale inventory (year)",
  inv_pct_chg_2yr = "Percent change in for-sale inventory (2 years)",
  cut_latest = "Latest share of listings with a price cut",
  cut_mean = "Mean of monthly share of listings with a price cut values",
  cut_pct_chg_mnth = "Percent change in share of listings with a price cut (month)",
  cut_pct_chg_yr = "Percent change in share of listings with a price cut (year)",
  cut_pct_chg_2yr = "Percent change in share of listings with a price cut (2 years)",
  price2rent_ratio = "Price-to-rent ratio (latest median list price / 12 * latest ZORI)",
  zillow_link = "Link to homes for sale on Zillow for this metro area"
)

nms <- names(pd)
pd <- set_tags(pd,
  list_price = nms[grepl("^list_", nms)],
  inventory = nms[grepl("^inv_", nms)],
  price_cut = nms[grepl("^cut_", nms)],
  other_metrics = c("price2rent_ratio", "sale_list_pct_diff")
)

td <- pd |>
  as_trelliscope_df(
    name = "US Metro Housing",
    description = "Home prices and other statistics for US metro areas (data from Zillow)",
    key_cols = "metro",
    path = "docs/housing"
  ) |>
  set_panel_options(
    plot = panel_options(width = 750, height = 500) #, force = TRUE)
  ) |>
  set_default_labels(c("metro", "list_latest", "zillow_link")) |>
  set_default_layout(ncol = 1, sidebar = TRUE,
    visible_filters = c("state", "list_latest", "price2rent_ratio", "sale_list_pct_diff"))

# unlink("docs/housing", recursive = TRUE)

view_trelliscope(td) # about 6 minutes







count(mdat, metric)

tmp <- filter(mdat, metro == "Seattle, WA",
  grepl("Sale Price|List Price", metric))

ggplot(tmp, aes(date, value, color = metric, group = metric)) +
  geom_point() +
  geom_line() +
  theme_minimal()

tmp <- mdat |>
  filter(metro == "Seattle, WA", grepl("Percent|Share", metric)) |>
  mutate(value = 100 * value)

ggplot(tmp, aes(date, value, color = metric, group = metric)) +
  geom_point() +
  geom_line() +
  theme_minimal()

tmp <- mdat |>
  filter(metro == "Seattle, WA", grepl("Inventory", metric))

ggplot(tmp, aes(date, value, color = metric, group = metric)) +
  geom_point() +
  geom_line() +
  theme_minimal()



tmp <- filter(cdat, county == "Benton County", state == "WA")
tmp2 <- tmp |>
  select(zhvi, date, home_type) |>
  group_by(home_type) |>
  slice_max(date)

p <- ggplot(tmp, aes(date, zhvi, color = home_type, group = home_type,
  linewidth = home_type)) +
  geom_line() +
  geom_text(data = tmp2, aes(label = home_type), hjust = 0, nudge_x = 20) +
  scale_color_manual(
    breaks = c("1 bed", "2 bed", "3 bed", "4 bed", "5+ bed", "all"),
    values = c("#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#08589e", "black"),
    # values = c("#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026", "black"),
    guide = "none"
  ) +
  scale_discrete_manual("linewidth",
    breaks = c("1 bed", "2 bed", "3 bed", "4 bed", "5+ bed", "all"),
    values = c(0.5, 0.5, 0.5, 0.5, 0.5, 1.5),
    guide = "none"
  ) +
  theme_minimal()

library(plotly)
ggplotly(p)









slope_fn <- function(x, y)
  coef(lm(y ~ x))[2]

d <- nest(housing, data = !one_of(c("county", "state"))

d <- d %>%
  mutate(
    cogs = map(data, function(x, state, county) {
      tibble(
        slope = slope_fn(x$time, x$medListPriceSqft),
        mean_list = mean(x$medListPriceSqft, na.rm = TRUE),
        mean_sold = mean(x$medSoldPriceSqft, na.rm = TRUE),
        n_obs = length(which(!is.na(x$medListPriceSqft)))
      )
    }),
    zillow_link = sprintf("http://www.zillow.com/homes/%s_rb/",
      gsub(" ", "-", paste(county, state)))
  ) %>%
  unnest(cogs) %>%
  filter(n_obs > 1)

d <- d %>%
  mutate(
    panel = map_plot(data, function(x) {
      plot_ly(data = x, x = ~time, y = ~medListPriceSqft,
        type = "scatter", mode = "markers") %>%
        layout(
          xaxis = list(title = "time"),
          yaxis = list(title = "median list price / sq ft"))
    })
  )

d %>%
  trelliscope(name = "list_vs_time",
    desc = "monthly median list price vs. time for 2984 US counties from 2008–2016",
    nrow = 2, ncol = 4)