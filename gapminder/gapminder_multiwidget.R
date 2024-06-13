# remotes::install_github("trelliscope/trelliscope")
library(trelliscope)
library(dplyr)
# remotes::install_github("hafen/mustachewidget")
library(mustachewidget)
library(plotly)

plot_fn <- function(country) {
  if (country == "New Zealand") {
    mustache("<div style='width: 100%; height: 100%; border: 4px solid red; text-align: center; box-sizing: border-box;'>{{country}}</div>", list(country = country))
  } else {
    data <- filter(gap, country == {{ country }})
    plot_ly(data) %>%
      add_trace(x = ~ year, y = ~ life_exp, type = "scatter", mode = "lines+markers")
  }
}

d <- gap |>
  filter(continent == "Oceania") |>
  summarise(mean_life_exp = mean(life_exp), .by = "country") |>
  mutate(
    test_panel = panel_lazy(plot_fn)
  ) |>
  as_trelliscope_df(
    name = "gapminder_multiwidget",
    path = "_public/gapminder_multiwidget"
  )

view_trelliscope(d)
