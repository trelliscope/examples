library(trelliscope)
library(tidyverse)

nms <- arrow::read_feather("babynames/_data/babynames.feather")
tots <- read_csv("babynames/_data/total_by_year.csv")

# see how many names are logged each year
tmp <- summarise(nms, n = sum(n), .by = "year")
plot(tmp)

name_pct <- nms |>
  summarise(n = sum(n), .by = c("name", "year", "sex")) |>
  left_join(tots, by = "year") |>
  mutate(pct = 100 * n / total)

nrng <- range(name_pct$n)

# see what highest ylim value will be for each name
tmp <- summarise(name_pct, pct = max(pct), .by = "name")
hist(log10(tmp$pct))

summ <- nms |>
  summarise(
    first_letter = substr(name[1], 1, 1),
    first2_letters = substr(name[1], 1, 2),
    tot = sum(n),
    tot_male = sum(n[sex == "M"]),
    tot_female = sum(n[sex == "F"]),
    pct_female = 100 * tot_female / tot,
    first_year = min(year),
    primary_sex = case_when(
      pct_female >= 75 ~ "Female",
      pct_female <= 25 ~ "Male",
      .default = "Unisex"
    ),
    .by = "name"
  )

summ <- left_join(summ,
  nms |>
    filter(year >= 2010, year < 2020) |>
    summarise(
      pct_female_2010s = 100 * sum(n[sex == "F"]) / sum(n),
      .by = "name"
    ),
  by = "name"
) |>
  mutate(
    pct_female_2010s = if_else(is.na(pct_female_2010s), 0, pct_female_2010s)
  )
# add first year appeared and primary sex
# text search should be regular expression
# and it should be throttled

# change in rank from decades
ranks <- nms |>
  mutate(decade = floor(year / 10) * 10) |>
  summarise(n = sum(n), .by = c("name", "decade")) |>
  complete(name, decade, fill = list(n = 0)) |>
  filter(decade %in% c(1880, 1980, 2000, 2010)) |>
  summarise(tot = sum(n), .by = c("name", "decade")) |>
  mutate(rank = 11010 - base::rank(tot, ties.method = "min"), .by = "decade") |>
  pivot_wider(id_cols = name, values_from = rank,
    names_from = decade, names_prefix = "rank_") |>
  mutate(
    rank_chg_1880 = rank_1880 - rank_2010,
    rank_chg_1980 = rank_1980 - rank_2010,
    rank_chg_2000 = rank_2000 - rank_2010
  ) |>
  rename_at(vars(starts_with("rank")), function(x) paste0(x, "s"))

summ <- left_join(summ, ranks, by = "name")


nd <- left_join(
  summ,
  nest(name_pct, data = -name),
  by = "name"
)

# make a panel function
plot_pct_fn <- function(name, data) {
  x <- data[[1]]
  ggplot(x, aes(x = year, y = pct * 1000, fill = sex)) +
    geom_col(alpha = 0.75, color = "white", linewidth = 0.1) +
    theme_minimal() +
    scale_x_continuous(limits = c(1879, 2023), expand = c(0, 0),
      breaks = seq(1880, 2020, 20)) +
    scale_y_continuous(labels = scales::comma) +
    # scale_y_log10(labels = scales::comma,
    #   limits = nrng, expand = c(0, 0)) +
    scale_fill_manual(values = c("#7B16EF", "#58C1AB"), breaks = c("F", "M")) +
    theme(legend.position = "none",
      # panel.background = element_rect(colour = "#dedede"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      text = element_text(family = "Poppins")) +
    labs(x = "Year", y = "Babies with name per 100k babies named", title = name) +
    annotate("text", x = 2022, y = 0, label = "purple = female, teal = male", hjust = 1, vjust = 1, family = "Poppins", color = "gray", size = 3)
}

plot_count_fn <- function(name, data) {
  x <- data[[1]]
  ggplot(x, aes(x = year, y = n, color = sex)) +
    geom_point(alpha = 0.75) +
    theme_minimal() +
    scale_x_continuous(limits = c(1879, 2023), expand = c(0, 0),
      breaks = seq(1880, 2020, 20)) +
    # scale_y_continuous(labels = scales::comma) +
    scale_y_log10(labels = scales::comma, limits = c(3, nrng[2])) +
    scale_color_manual(values = c("#7B16EF", "#58C1AB"), breaks = c("F", "M")) +
    theme(legend.position = "none",
      # panel.background = element_rect(colour = "#dedede"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      text = element_text(family = "Poppins")) +
    labs(x = "Year", y = "Babies with name", title = name) +
    annotate("text", x = 2022, y = 3, label = "purple = female, teal = male", hjust = 1, vjust = 1, family = "Poppins", color = "gray", size = 3)
}

nd <- nd |>
  mutate(
    yearly_prop = panel_lazy(plot_pct_fn),
    yearly_count = panel_lazy(plot_count_fn),
    first_year = number(first_year, locale = FALSE, digits = 0),
    babynames_link = href(paste0("https://babynames.com/name/", name))
  )
# https://www.momjunction.com/baby-names/amalamani/

# try it out on some names
filter(nd, name == "Skyler")$yearly_prop
filter(nd, name == "Charlie")$yearly_prop
filter(nd, name == "Charlie")$yearly_count
filter(nd, name == "Justice")$yearly_prop
filter(nd, name == "Jammie")$yearly_prop
filter(nd, name == "Jacque")$yearly_prop
filter(nd, name == "Alva")$yearly_prop
filter(nd, name == "Taylor")$yearly_prop

nd <- nd |>
  set_var_labels(
    first_letter = "First letter of name",
    first2_letters = "First two letters of name",
    tot = "Total babies given this name since 1880",
    tot_male = "Total boys given this name since 1880",
    tot_female = "Total girls given this name since 1880",
    pct_female = "Percent of times the name was used for a female baby since 1880",
    pct_female_2010s = "Percent of times the name was used for a female baby in the 2010s",
    rank_1880s = "Rank of baby name in the 1880s (1 is most popular)",
    rank_1980s = "Rank of baby name in the 1980s (1 is most popular)",
    rank_2000s = "Rank of baby name in the 2000s (1 is most popular)",
    rank_2010s = "Rank of baby name in the 2010s (1 is most popular)",
    rank_chg_1880s = "Change in rank from 2010s to 1880s (negative means became less popular)",
    rank_chg_1980s = "Change in rank from 2010s to 1980s (negative means became less popular)",
    rank_chg_2000s = "Change in rank from 2010s to 2000s (negative means became less popular)",
    babynames_link = "Link to babynames.com page for this name",
    first_year = "First year the name appears",
    primary_sex = "Primary sex the name is used for (unisex if 25-75% is male or female)",
    yearly_prop = "Plot of yearly popularity of name",
    yearly_count = "Plot of yearly count of name"
  )

dt <- as_trelliscope_df(nd, name = "Baby names",
  desc = "Popularity of baby names by year (for names that have been given at least 1,000 times since 1880)",
  path = "_public/babynames")

dt <- dt |>
  # head(n = 50) |>
  set_panel_options(
    yearly_prop = panel_options(height = 500, width = 500), # , format = "svg"),
    yearly_count = panel_options(height = 500, width = 500), # , format = "svg")
  ) |>
  set_default_labels(c("name", "tot", "babynames_link")) |>
  set_default_layout(ncol = 4, sidebar = TRUE,
    visible_filters = c("primary_sex", "tot", "first_year")) |>
  set_default_sort("tot", "desc") |>
  # set_default_filters(
  #   filter_range("n_2010s", min = 5000)
  # ) |>
  add_inputs(
    input_text(name = "comments", label = "Comments about this name"),
    input_radio(name = "link",
      label = "Like this name?", options = c("no", "yes"))
  )

# unlink("_public/babynames/displays/Baby_names/", recursive = TRUE)

view_trelliscope(dt)
