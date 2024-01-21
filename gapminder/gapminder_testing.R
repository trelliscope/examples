library(trelliscope)
library(dplyr)
library(ggplot2)
# remotes::install_github("hafen/mustachewidget")
library(mustachewidget)

# stripped down (in size) version of gapminder to commit to js lib github

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
  theme_void()
# note that if you print p you will get a trelliscope as before...

d1 <- as_panels_df(p, panel_col = "lexp_time")

d <- left_join(d, d1, by = c("country", "continent"))

plot_fn <- function(country)
  mustache("<div style='width: 100%; height: 100%; border: 4px solid red; text-align: center;'>{{country}}</div>", list(country = country))

d <- d %>%
  mutate(html_panel = panel_lazy(plot_fn))

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
    lexp_time = "Plot of life expectancy over time (svg)",
    lexp_time_png = "Plot of life expectancy over time (png)",
    html_panel = "Dummy 'plot' to test html panels",
    flag = "Country flag"
  )

d <- d |>
  set_tags(
    stats = c("mean_lexp", "mean_gdp", "max_lexp_pct_chg"),
    info = c("country", "continent", "iso_alpha2")
  )

dt <- as_trelliscope_df(d, name = "Life expectancy",
  description = "Life expectancy over time by country",
  path = "_public/gapminder_testing")

# set panel options (if not specified, defaults are used)
dt <- dt |>
  set_panel_options(
    lexp_time = panel_options(width = 600, height = 400, format = "svg"),
    html_panel = panel_options(width = 300, height = 600)
  ) |>
  set_primary_panel("lexp_time")

# now set all the other stuff...
dt <- dt |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(ncol = 3, sidebar = TRUE,
    visible_filters = c("continent", "mean_lexp")) |>
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
