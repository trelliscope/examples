# remotes::install_github("trelliscope/trelliscope@multi-panels")
# library(trelliscope)
# load_all("../trelliscope")
library(dplyr)
library(ggplot2)

# summarize gapminder by country
d <- gapminder |>
  arrange(year) |>
  group_by(country, continent, iso_alpha2) |>
  mutate(pct_chg = 100 * (lifeExp - lag(lifeExp)) / lag(lifeExp)) |>
  summarise(
    mean_lexp = number(mean(lifeExp)),
    mean_gdp = currency(mean(gdpPercap)),
    max_lexp_pct_chg = max(pct_chg, na.rm = TRUE),
    dt_lexp_max_pct_chg = as.Date(paste0(year[which.max(pct_chg)], "-01-01")),
    dttm_lexp_max_pct_chg = as.POSIXct(dt_lexp_max_pct_chg),
    wiki_link = href(paste0("https://en.wikipedia.org/wiki/", country[1])),
    .groups = "drop"
  )

# add some ggplots
p <- ggplot(gapminder, aes(year, lifeExp)) +
  geom_point() +
  facet_panels(vars(country, continent))
# note that if you print p you will get a trelliscope as before...

d1 <- as_panels_df(p, panel_col = "lexp_time")
d2 <- as_panels_df(p, panel_col = "lexp_time_pl", as_plotly = TRUE)

# these are data frames with ggplot columns as before
d1
d2

# view some of the panels
d1$lexp_time[[1]]
d2$lexp_time_pl[[1]]

# join with our main dataset
d <- left_join(d, d1, by = c("country", "continent"))
d <- left_join(d, d2, by = c("country", "continent"))

# also add panel_local and panel_url columns for flags
# flag2 is also added not explicitly as a panel column
flags_dir <- "_ignore/multi_panel/gapminder_tmp/flags"
flag_base_url <- "https://raw.githubusercontent.com/hafen/countryflags/master/png/512/"

d <- mutate(d,
  # flag = panel_local(file.path(flags_dir, paste0(iso_alpha2, ".png"))),
  flag_url = panel_url(paste0(flag_base_url, iso_alpha2, ".png"))
)

d

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
    flag = "Flag",
    flag_url = "Flag URL"
  )

d <- d |>
  set_tags(
    stats = c("mean_lexp", "mean_gdp", "max_lexp_pct_chg"),
    info = c("country", "continent", "iso_alpha2")
  )

dt <- as_trelliscope_df(d, name = "gapminder",
  path = "docs/gapminder")

# set panel options (if not specified, defaults are used)
dt <- dt |>
  set_panel_options(
    lexp_time = panel_options_lazy(width = 600, height = 400, format = "svg")
  ) |>
  set_primary_panel("lexp_time_pl")

# now set all the other stuff...
dt <- dt |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(ncol = 4) |>
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
      label = "Does the data look correct?", options = c("no", "yes")),
    email = "johndoe123@fakemail.net"
  )

view_trelliscope(dt)

# add some ggplots
p <- ggplot(gapminder, aes(year, lifeExp)) +
  geom_point() +
  facet_panels(vars(country, continent))
# note that if you print p you will get a trelliscope as before...

d1 <- as_panels_df(p, panel_col = "lexp_time")
d2 <- as_panels_df(p, panel_col = "lexp_time_pl", as_plotly = TRUE)

# these are data frames with ggplot columns as before
d1
d2

# view some of the panels
d1$lexp_time[[1]]
d2$lexp_time_pl[[1]]

# join with our main dataset
d <- left_join(d, d1, by = c("country", "continent"))
d <- left_join(d, d2, by = c("country", "continent"))

# also add panel_local and panel_url columns for flags
# flag2 is also added not explicitly as a panel column
flags_dir <- "_ignore/multi_panel/gapminder_tmp/flags"
flag_base_url <- "https://raw.githubusercontent.com/hafen/countryflags/master/png/512/"

d <- mutate(d,
  flag = panel_local(file.path(flags_dir, paste0(iso_alpha2, ".png"))),
  flag_url = panel_url(paste0(flag_base_url, iso_alpha2, ".png"))
)

d

# now set variable labels and tags
# (this could be done after it's a trelliscope df as well)
d <- d |>
  set_var_labels(
    mean_lexp = "Mean life expectancy",
    mean_gdp = "Mean GDP per capita",
    max_lexp_pct_chg = "Max % year-to-year change in life expectancy",
    dt_lexp_max_pct_chg = "Date of max % year-to-year change in life expectancy",
    wiki_link = "Link to country Wikipedia entry",
    flag = "Flag",
    flag_url = "Flag URL"
  )

d <- d |>
  set_tags(
    stats = c("mean_lexp", "mean_gdp", "max_lexp_pct_chg"),
    info = c("country", "continent", "iso_alpha2")
  )

dt <- as_trelliscope_df(d, name = "gapminder",
  path = "docs/gapminder")

# set panel options (if not specified, defaults are used)
dt <- dt |>
  set_panel_options(
    lexp_time = panel_options_lazy(width = 600, height = 400, format = "svg", prerender = FALSE)
  ) |>
  set_primary_panel("lexp_time_pl")

# now set all the other stuff...
dt <- dt |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(ncol = 4) |>
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
      label = "Does the data look correct?", options = c("no", "yes")),
    email = "johndoe123@fakemail.net"
  )

view_trelliscope(dt)
    mean_lexp = number(mean(lifeExp)),
    mean_gdp = currency(mean(gdpPercap)),
    max_lexp_pct_chg = max(pct_chg, na.rm = TRUE),
    dt_lexp_max_pct_chg = as.Date(paste0(year[which.max(pct_chg)], "-01-01")),
    wiki_link = href(paste0("https://en.wikipedia.org/wiki/", country[1])),
    .groups = "drop"
  )

# add some ggplots
p <- ggplot(gapminder, aes(year, lifeExp)) +
  geom_point() +
  facet_panels(vars(country, continent))
# note that if you print p you will get a trelliscope as before...

d1 <- as_panels_df(p, panel_col = "lexp_time")
d2 <- as_panels_df(p, panel_col = "lexp_time_pl", as_plotly = TRUE)

# these are data frames with ggplot columns as before
d1
d2

# view some of the panels
d1$lexp_time[[1]]
d2$lexp_time_pl[[1]]

# join with our main dataset
d <- left_join(d, d1, by = c("country", "continent"))
d <- left_join(d, d2, by = c("country", "continent"))

flag_base_url <- "https://raw.githubusercontent.com/hafen/countryflags/master/png/512/"

d <- mutate(d,
  flag_url = panel_url(paste0(flag_base_url, iso_alpha2, ".png"))
)

d

# now set variable labels and tags
# (this could be done after it's a trelliscope df as well)
d <- d |>
  set_var_labels(
    mean_lexp = "Mean life expectancy",
    mean_gdp = "Mean GDP per capita",
    max_lexp_pct_chg = "Max % year-to-year change in life expectancy",
    dt_lexp_max_pct_chg = "Date of max % year-to-year change in life expectancy",
    wiki_link = "Link to country Wikipedia entry",
    flag_url = "Flag"
  )

d <- d |>
  set_tags(
    stats = c("mean_lexp", "mean_gdp", "max_lexp_pct_chg"),
    info = c("country", "continent", "iso_alpha2")
  )

dt <- as_trelliscope_df(d, name = "gapminder",
  path = "docs/gapminder")

# set panel options (if not specified, defaults are used)
dt <- dt |>
  set_panel_options(
    lexp_time = panel_options_lazy(width = 600, height = 400, format = "svg")
  ) |>
  set_primary_panel("lexp_time_pl")

# now set all the other stuff...
dt <- dt |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(ncol = 4) |>
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
      label = "Does the data look correct?", options = c("no", "yes")),
    email = "johndoe123@fakemail.net"
  )

view_trelliscope(dt)


a <- as_trelliscope_df(mars_rover, name = "mars_rover", path = "_ignore/multi_panel/mars_rover")

view_trelliscope(a)




