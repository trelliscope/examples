# see _download.R and _prep.R for more details

library(trelliscope)
library(dplyr)

sets <- readr::read_rds("examples/lego/_data/sets.rds")
color_ct <- readr::read_csv("examples/lego/_data/set_color_ct.csv")

x <- get_test_subset(color_ct, sets, by = "inv_id")
x <- filter(color_ct, inv_id == 114094)

plot_fn <- function(x) {
  library(plotly, warn.conflicts = FALSE)
  plot_ly(
    type = "treemap",
    labels = paste0(x$name, "<br>", x$n, " parts",
      ifelse(x$is_trans, "<br>translucent", "")),
    values = x$n,
    parents = "",
    marker = list(colors = x$rgb)
  ) |>
    layout(title = list(text = "asdf"), margin = list(t = 0, b = 0, l = 0, r = 0)) |>
    config(displayModeBar = FALSE)
}

lbls <- c("name", "year", "theme", "n_parts", "rebrickable_url")

d <- sets %>%
  mutate(
    img_url = panel_url(img_url),
    decade = factor(10 * (year %/% 10)),
    color_map = panel_lazy(plot_fn, color_ct, by = "inv_id"),
    inv_id = as.character(inv_id),
    n_parts = number(ifelse(is.na(n_parts), 0, n_parts),
      digits = 0, log = FALSE),
    n_minifigs = number(ifelse(is.na(n_minifigs), 0, n_minifigs),
      digits = 0, log = FALSE),
    year = number(year, locale = FALSE, digits = 0),
    retail = currency(retail),
    brickeconomy_url = href(paste0("https://www.brickeconomy.com/set/",
      set_num)),
  ) |>
  as_trelliscope_df(
    name = "lego sets",
    description = "lego sets",
    path = "docs/lego"
  ) |>
  set_panel_options(
    img_url = panel_options(width = 600, height = 500)
  ) |>
  set_default_labels(lbls) |>
  set_default_sort("n_parts", dir = "desc") |>
  set_default_layout(ncol = 4) |>
  add_inputs(
    input_radio(name = "own_it", label = "I own this set",
      options = c("no", "yes")),
    input_checkbox(name = "want_it", label = "I want this set",
      options = "yes")
  ) |>
  add_view(
    name = "90s sets",
    filter_string("decade", values = "1990"),
    # state_labels(lbls),
    state_layout(ncol = 4),
    state_sort("year", dir = "asc")
  ) |>
  add_view(
    name = "Sets with 1000+ parts",
    filter_range("n_parts", min = 1000),
    # state_labels(lbls),
    # state_layout(ncol = 4),
    state_sort("n_parts", dir = "desc")
  ) |>
  add_view(
    name = "Star Wars sets",
    filter_string("theme", values = "Star Wars"),
    # state_labels(lbls),
    # state_layout(ncol = 4),
    state_sort("n_parts", dir = "desc")
  ) |>
  set_info_html("examples/lego/info.html")

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
    brickeconomy_url = "View the set on BrickEconomy.com"
  )

d <- d |> set_tags(
  meta = c("name", "year", "theme", "sub_theme"),
  parts = c("n_parts", "n_spare_parts", "n_part_types", "prim_part_num",
    "prim_part_name"),
  colors = c("n_colors", "prim_color_name", "prim_color_hex"),
  links = c("rebrickable_url", "brickeconomy_url")
)

d$figures_link <- href(unlist(lapply(d$minifig_ids, function(ids) {
  if (is.null(ids))
    return(NA_character_)
  paste0("#selectedDisplay=lego%20figures&labels=name&filter=var:fig_num;type:category;regexp:;metatype:string;val:", paste(ids, collapse = "#"))
})))

view_trelliscope(d)

figs <- readr::read_rds("examples/lego/_data/minifigs.rds")

d2 <- figs %>%
  mutate(
    img_url = panel_url(img_url)
  ) |>
  as_trelliscope_df(
    name = "lego figures",
    description = "lego figures",
    path = "docs/lego"
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
