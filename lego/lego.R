# see _download.R and _prep.R for more details

library(trelliscope)
library(dplyr)

sets <- readr::read_rds("lego/_data/sets.rds")
color_ct <- readr::read_csv("lego/_data/set_color_ct.csv")

plot_fn <- function(inv_id) {
  x <- filter(color_ct, {{ inv_id }} == inv_id)
  library(plotly, warn.conflicts = FALSE)
  pct_txt <- paste0("(", round(100 * x$n / sum(x$n)), "%)")
  plot_ly(
    type = "treemap",
    labels = paste0(x$name, "<br>", x$n, " parts ", pct_txt,
      ifelse(x$is_trans, "<br>translucent", "")),
    values = x$n,
    parents = "",
    marker = list(colors = x$rgb)
  ) |>
    layout(margin = list(t = 0, b = 0, l = 0, r = 0)) |>
    config(displayModeBar = FALSE)
}

plot_fn(sets$inv_id[1])

lbls <- c("name", "year", "theme", "n_parts", "rebrickable_url")

d <- sets %>%
  mutate(
    img_url = panel_url(img_url),
    decade = factor(10 * (year %/% 10)),
    color_map = panel_lazy(plot_fn),
    inv_id = as.character(inv_id),
    n_parts = number(ifelse(is.na(n_parts), 0, n_parts),
      digits = 0, log = FALSE),
    n_minifigs = number(ifelse(is.na(n_minifigs), 0, n_minifigs),
      digits = 0, log = FALSE),
    year = number(year, locale = FALSE, digits = 0),
    retail = currency(retail),
    rebrickable_url = href(rebrickable_url),
    brickeconomy_url = href(paste0("https://www.brickeconomy.com/set/",
      set_num)),
    minifig_link = filter_cat_href("lego figures", "fig_num", minifig_ids,
      labels = "name")
  )

d <- d |>
  set_var_labels(
    set_num = "Set number",
    name = "Set name",
    year = "Year released",
    img_url = "Image URL",
    theme = "Set theme",
    sub_theme = "Set sub-theme",
    inv_id = "Inventory ID",
    rebrickable_url = "View the set on rebrickable.com",
    n_parts = "Total number of parts",
    n_spare_parts = "Number of spare parts",
    n_part_types = "Number of part types",
    n_colors = "Number of distinct colors",
    prim_part_num = "Most common part number",
    prim_part_name = "Most common part name",
    prim_color_name = "Most common color name",
    prim_color_hex = "Most common color hex code",
    n_minifigs = "Number of minifigs",
    n_minifig_parts = "Number of minifig parts",
    n_versions = "Number of versions of the set",
    retail = "Retail price (USD) from BrickEconomy.com",
    decade = "Decade released",
    brickeconomy_url = "View the set on BrickEconomy.com",
    minifig_link = "Link to display of minifigs in this set"
  )

d <- d |> set_tags(
  meta = c("name", "year", "theme", "sub_theme"),
  parts = c("n_parts", "n_spare_parts", "n_part_types", "prim_part_num",
    "prim_part_name"),
  colors = c("n_colors", "prim_color_name", "prim_color_hex"),
  links = c("rebrickable_url", "brickeconomy_url")
)

dt <- d |>
  as_trelliscope_df(
    name = "LEGO sets",
    description = "Information for over 17k LEGO sets",
    path = "_public/bricks",
    order = -1 # want this one to show by default
  ) |>
  set_panel_options(
    img_url = panel_options(width = 600, height = 500)
  ) |>
  set_default_labels(lbls) |>
  set_default_sort("n_parts", dir = "desc") |>
  set_default_layout(ncol = 4, visible_filters = c("year", "theme")) |>
  add_inputs(
    input_radio(name = "own_it", label = "I own this set",
      options = c("no", "yes")),
    input_checkbox(name = "want_it", label = "I want this set",
      options = "yes")
  ) |>
  add_view(
    name = "90s sets",
    filter_string("decade", values = "1990"),
    state_labels(lbls),
    state_layout(ncol = 4, visible_filters = "decade"),
    state_sort("year", dir = "asc")
  ) |>
  add_view(
    name = "Sets with 1000+ parts",
    filter_range("n_parts", min = 1000),
    state_labels(lbls),
    state_layout(ncol = 4),
    state_sort("n_parts", dir = "desc")
  ) |>
  add_view(
    name = "Star Wars sets",
    filter_string("theme", values = "Star Wars"),
    state_labels(lbls),
    state_layout(ncol = 4),
    state_sort("n_parts", dir = "desc")
  ) |>
  set_info_html("lego/info.html")

view_trelliscope(dt)

figs <- readr::read_rds("lego/_data/minifigs.rds")

d2 <- figs %>%
  mutate(
    img_url = panel_url(img_url)
  ) |>
  as_trelliscope_df(
    name = "LEGO minfigures",
    description = "Information for ~14k LEGO minifigures",
    path = "_public/bricks"
  ) |>
  set_panel_options(
    img_url = panel_options(width = 600, height = 500)
  ) |>
  set_default_labels(c("name", "n_sets")) |>
  set_default_sort("n_sets", dir = "desc") |>
  set_default_layout(ncol = 4)

view_trelliscope(d2)


# ggplot(d, aes(year, n_parts)) + geom_point() + theme_minimal()
# ggplot(d, aes(n_parts, retail)) +
#   geom_point(alpha = 0.4) +
#   theme_minimal() +
#   labs(x = "Number of Parts", y = "Retail Price (USD)")
