library(trelliscope)
library(dplyr)
library(ggplot2)

# summarize gapminder by country
d <- gap |>
  arrange(year) |>
  group_by(country, continent, iso_alpha2) |>
  mutate(pct_chg = 100 * (life_exp - lag(life_exp)) / lag(life_exp)) |>
  summarise(
    mean_lexp = number(mean(life_exp)),
    mean_gdp = currency(mean(gdp_percap)),
    max_lexp_pct_chg = max(pct_chg, na.rm = TRUE),
    dt_lexp_max_pct_chg = as.Date(paste0(year[which.max(pct_chg)], "-01-01")),
    dttm_lexp_max_pct_chg = as.POSIXct(dt_lexp_max_pct_chg),
    wiki_link = href(paste0("https://en.wikipedia.org/wiki/", country[1])),
    .groups = "drop"
  )

# add some ggplots
p <- ggplot(gap, aes(year, life_exp)) +
  geom_point() +
  facet_panels(vars(country, continent)) +
  theme_minimal() +
  labs(y = "Life expectancy")
# note that if you print p you will get a trelliscope as before...

d1 <- as_panels_df(p, panel_col = "lexp_time", as_plotly = TRUE)

p <- ggplot(gap, aes(year, life_exp)) +
  geom_line(color = "#4E79A7", linewidth = 1) +
  geom_point(color = "#4E79A7", size = 3) +
  facet_panels(vars(country, continent), unfacet = "line") +
  theme_minimal(base_size = 14) +
  labs(y = "Life expectancy")

d2 <- as_panels_df(p, panel_col = "lexp_time_unfacet")

# join with our main dataset
d <- left_join(d, d1, by = c("country", "continent"))
d <- left_join(d, d2, by = c("country", "continent"))

d <- mutate(d,
  flag = panel_url(paste0(
    "https://raw.githubusercontent.com/hafen/countryflags/master/png/512/",
    iso_alpha2, ".png")))

# now set variable labels and tags
# (this could be done after it's a trelliscope df as well)
d <- d |>
  set_var_labels(
    mean_lexp = "Mean life expectancy",
    mean_gdp = "Mean GDP per capita",
    max_lexp_pct_chg = "Max % year-to-year change in life expectancy",
    dt_lexp_max_pct_chg = "Date of max % year-to-year change in life expectancy",
    dttm_lexp_max_pct_chg = "Date-time of max % year-to-year change in life expectancy",
    wiki_link = "Link to country Wikipedia entry",
    lexp_time = "Plot of life expectancy over time",
    lexp_time_unfacet = "Plot of life expectancy over time (unfaceted)",
    flag = "Country flag"
  )

d <- d |>
  set_tags(
    stats = c("mean_lexp", "mean_gdp", "max_lexp_pct_chg"),
    info = c("country", "continent", "iso_alpha2")
  )

dt <- as_trelliscope_df(d, name = "Life expectancy",
  description = "Life expectancy over time by country",
  path = "_public/gapminder")

# set panel options (if not specified, defaults are used)
dt <- dt |>
  set_panel_options(
    lexp_time = panel_options(width = 600, height = 400, format = "svg")
  ) |>
  set_primary_panel("lexp_time_unfacet")

# now set all the other stuff...
dt <- dt |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(ncol = 3, sidebar = TRUE) |>
  set_default_sort(c("continent", "mean_lexp"), dir = c("asc", "desc")) |>
  set_default_filters(
    filter_string("continent", values = "Africa"),
    filter_range("mean_lexp", max = 50)
  ) |>
  add_view(
    name = "Countries with high life expectancy (mean >= 60)",
    filter_range("mean_lexp", min = 60),
    state_sort("mean_gdp", dir = "desc")
  ) |>
  add_inputs(
    input_text(name = "comments", label = "Comments about this panel",
      height = 6),
    input_radio(name = "looks_correct",
      label = "Data looks correct?", options = c("no", "yes"))
  )

view_trelliscope(dt)
