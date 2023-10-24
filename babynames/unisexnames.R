library(trelliscope)
library(tidyverse)

nms <- arrow::read_feather("babynames/_data/babynames.feather")

nms |>
  summarise(n = sum(n), .by = "sex") |>
  mutate(pct = 100 * n  / sum(n))

# get baby names that are used for both sexes
uni <- nms |>
  group_by(name) |>
  summarise(
    n_f = sum(n[sex == "F"]),
    n_m = sum(n[sex == "M"]),
    prop_f = n_f / (n_f + n_m))
# call it unisex if at least 10% Male or Female
uni <- filter(uni, prop_f > 0.1 & prop_f < 0.9)

# for each decade of data and each name, compute Male/Female distribution
# and make the dataset complete (each time/name combination has both sexes)
unidat <- nms |>
  filter(name %in% uni$name) |>
  mutate(year = floor(year / 10) * 10) |>
  summarise(n_f = sum(n[sex == "F"]), n = sum(n),
    .by = c("name", "year"))

nd <- nest(unidat, data = -name)

# make a panel function
panel_fn <- function(name, data) {
  x <- data[[1]]
  p1 <- ggplot(x, aes(xmin = year, xmax = year + 10, ymin = 0, ymax = n)) +
    geom_rect(color = "white") +
    theme_minimal() +
    scale_x_continuous(limits = c(1880, 2020), expand = c(0, 0)) +
    scale_y_continuous(n.breaks = 4, labels = scales::comma,
      limits = c(0, max(x$n)), expand = c(0, 0)) +
    theme(legend.position = "none",
      # panel.background = element_rect(colour = "#dedede"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      text = element_text(family = "Poppins")) +
    labs(x = "Year", y = "Count", title = name)

  lgnd <- tibble(y = c(0, 100), x = 2020, txt = c("Male", "Female"))

  p2 <- ggplot(x) +
    geom_rect(aes(xmin = year, xmax = year + 10, ymin = 100 * n_f / n,
      alpha = n, ymax = 0), fill = "#58C1AB") +
    geom_rect(aes(xmin = year, xmax = year + 10, ymin = 100 * n_f / n,
      alpha = n, ymax = 100), fill = "#7B16EF") +
    geom_text(data = lgnd, aes(x = x, y = y, label = txt), size = 5,
      hjust = 1, vjust = c(0, 1), nudge_x = -0.5, nudge_y = c(1, -1),
      family = "Poppins") + # color = "#777777"
    scale_x_continuous(limits = c(1880, 2020), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0)) +
    theme_minimal() +
    theme(legend.position = "none",
      panel.background = element_rect(colour = "#dedede"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      text = element_text(family = "Poppins")) +
    labs(x = "Year", y = "Sex Distribution (Percent)")

  thm <- theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank())

  cowplot::plot_grid(p1 + thm, p2, ncol = 1, rel_heights = c(1, 3), align = "v") |>
    suppressWarnings()
}

nd <- mutate(nd, plot = panel_lazy(panel_fn))

# try it out on some names
# filter(nd, name == "Skyler")$plot
# filter(nd, name == "Charlie")$plot
# filter(nd, name == "Justice")$plot
# filter(nd, name == "Jammie")$plot
# filter(nd, name == "Jacque")$plot
# filter(nd, name == "Alva")$plot
# filter(nd, name == "Taylor")$plot

summ <- unidat |>
  arrange(name, year) |>
  summarise(
    first_letter = substr(name[1], 1, 1),
    first2_letters = substr(name[1], 1, 2),
    pct_female = 100 * mean(sum(n_f) / sum(n)),
    n_all_years = sum(n),
    n_2010s = sum(n[year == 2010]),
    pct_female_2010s =
      100 * mean(sum(n_f[year == 2010]) / ifelse(n_2010s == 0, NA, n_2010s)),
    diff50 = abs(50 - pct_female),
    diff50_2010s = abs(50 - pct_female_2010s),
    .by = "name"
  )

summ |>
  arrange(diff50_2010s) |>
  filter(n_2010s >= 10000)
summ |>
  arrange(diff50) |>
  filter(n_2010s >= 10000)

d <- left_join(nd, summ, by = "name") |>
  mutate(babynames_link = href(paste0("https://babynames.com/name/", name)))

# https://www.momjunction.com/baby-names/amalamani/

d <- d |>
  set_var_labels(
    first_letter = "First letter of name",
    first2_letters = "First two letters of name",
    pct_female = "Percent of times the name was used for a female baby",
    n_all_years = "Total number of babies with this name over all years",
    n_2010s = "Total number of babies with this name in the 2010s",
    pct_female_2010s = "Percent of times the name was used for a female baby in the 2010s",
    diff50 = "How far off from 50/50 is the name over all years?",
    diff50_2010s = "How far off from 50/50 is the name in the 2010s?",
    babynames_link = "Link to babynames.com page for this name"
  )

dt <- as_trelliscope_df(d, name = "Unisex baby names",
  desc = "Distribution of unisex names by decade for names that are at least 10% mixed across sex",
  path = "_public/babynames")

dt <- dt |>
  set_panel_options(plot = panel_options(height = 470, width = 400)) |>
  set_default_labels(c("name", "pct_female", "babynames_link")) |>
  set_default_layout(ncol = 5) |>
  set_default_sort("n_all_years", "desc") |>
  # set_default_filters(
  #   filter_range("n_2010s", min = 5000)
  # ) |>
  add_inputs(
    input_text(name = "comments", label = "Comments about this name"),
    input_radio(name = "link",
      label = "Like this name?", options = c("no", "yes"))
  )

view_trelliscope(dt)
