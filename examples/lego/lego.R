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

d <- sets %>%
  mutate(
    img_url = panel_url(img_url),
    color_map = panel_lazy(plot_fn, color_ct, by = "inv_id"),
    inv_id = as.character(inv_id),
    n_parts = number(ifelse(is.na(n_parts), 0, n_parts),
      digits = 0, log = FALSE),
    n_minifigs = number(ifelse(is.na(n_parts), 0, n_parts),
      digits = 0, log = FALSE),
    year = number(year, locale = FALSE, digits = 0)
  ) |>
  as_trelliscope_df(
    name = "lego sets",
    description = "lego sets",
    path = "docs/lego"
  ) |>
  set_panel_options(
    img_url = panel_options(width = 600, height = 500)
  ) |>
  set_default_labels(c("name", "year", "theme", "n_parts",
    "rebrickable_url")) |>
  set_default_sort("n_parts", dir = "desc") |>
  set_default_layout(ncol = 4) |>
  add_inputs(
    input_radio(name = "own_it", label = "I own this set",
      options = c("no", "yes")),
    input_checkbox(name = "want_it", label = "I want this set",
      options = "yes"),
    email = "johndoe123@fakemail.net"
  )

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
