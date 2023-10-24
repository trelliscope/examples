# -------------------------------------------------------- #
# download data from rebrickable - only run once           #
# -------------------------------------------------------- #

library(rvest)

page <- read_html("https://rebrickable.com/downloads/")

hrefs <- html_elements(page, "li > span > a") %>% html_attr("href")

for (href in hrefs) {
  fl <- strsplit(basename(href), "\\?")[[1]][1]
  message(fl)
  dest <- file.path("lego/_data/rebrickable", fl)
  download.file(href, destfile = dest)
}

# retail prices
# sets <- readr::read_rds("examples/lego/_data/sets.rds")

# nsets <- sets |>
#   mutate(
#     "Set Number" = set_num,
#     Quantity = 1,
#     "Includes Spares" = TRUE,
#     "Inventory ID" = inv_id,
#     .keep = "none"
#   ) |>
#   readr::write_csv("/tmp/rebrickable_sets_unnamed-set-list.csv")

# tmp <- bind_rows(
#   readr::read_csv("/tmp/BrickEconomy-Sets.csv"),
#   readr::read_csv("/tmp/BrickEconomy-Sets2.csv")
# )

# nsets2 <- filter(nsets, !(`Set Number` %in% tmp$`Number`))

# filter(sets, !set_num %in% tmp$Number)

# readr::write_csv(nsets2, "/tmp/rebrickable_sets_unnamed2-set-list.csv")

# rtl <- bind_rows(
#   readr::read_csv("/tmp/BrickEconomy-Sets.csv"),
#   readr::read_csv("/tmp/BrickEconomy-Sets2.csv")
# ) |>
#   rename_all(tolower) |>
#   select(number, retail) |>
#   mutate(retail = as.numeric(gsub("\\$", "", retail))) |>
#   rename(set_num = number) |>
#   mutate(retail = ifelse(retail == 0, NA, retail))

# readr::write_csv(rtl, "examples/lego/_data/retail_prices.csv")
