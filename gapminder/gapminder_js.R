d <- gap |>
  arrange(year) |>
  group_by(country, continent, iso_alpha2) |>
  mutate(pct_chg = 100 * (life_exp - lag(life_exp)) / lag(life_exp)) |>
  summarise(
    mean_lexp = mean(life_exp),
    mean_gdp = mean(gdp_percap),
    max_lexp_pct_chg = max(pct_chg, na.rm = TRUE),
    dt_lexp_max_pct_chg = as.Date(paste0(year[which.max(pct_chg)], "-01-01")),
    dttm_lexp_max_pct_chg = as.POSIXct(dt_lexp_max_pct_chg) + 1,
    wiki_link = paste0("https://en.wikipedia.org/wiki/", country[1]),
    .groups = "drop"
  ) |>
  mutate(flag = panel_url(paste0(
    "https://raw.githubusercontent.com/hafen/countryflags/master/png/512/",
    iso_alpha2, ".png")))

dir.create("_public/gapminder_js", showWarnings = FALSE)
jsonlite::write_json(d, "_public/gapminder_js/gapminder.json", auto_unbox = TRUE, pretty = TRUE)
# TODO: fix Can't convert `x` <number_vec> to <character>
readr::write_csv(d, "_public/gapminder_js/gap.csv")
