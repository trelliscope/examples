library(trelliscope)
library(dplyr)
library(ggplot2)

# summarize gapminder by country
d <- gap |>
  summarise(
    mean_lexp = number(mean(life_exp)),
    mean_gdp = currency(mean(gdp_percap)),
    .by = c("country", "continent")
  )

plot_fn <- function(country) {
  data <- filter(gap, country == {{ country }})
  ggplot(data, aes(year, life_exp)) +
    geom_point() +
    theme_minimal() +
    labs(y = "Life expectancy")
}

d <- mutate(d, lexp_time = panel_lazy(plot_fn))

dt <- as_trelliscope_df(d, name = "Life expectancy",
  description = "Life expectancy over time by country",
  path = "_public/gapminder_websocket")
  # path = "../trelliscopejs-lib/_examples/gapminder_websocket")

dt <- dt |>
  set_panel_options(lexp_time = panel_options(prerender = FALSE))

view_trelliscope(dt)

