tmp <- tempfile(fileext = ".zip")
download.file("https://www.ssa.gov/oact/babynames/names.zip", tmp, quiet = TRUE)
unzip(tmp, exdir = "babynames/_data/raw")
unlink(tmp)

# From: http://www.ssa.gov/oact/babynames/limits.html
ff <- list.files("babynames/_data/raw",
  pattern = "\\.txt$", full.names = TRUE)

year <- as.numeric(substr(basename(ff), 4, 7))

d <- lapply(ff, \(x) {
  res <- readr::read_csv(x, col_names = FALSE) |>
    dplyr::mutate(year = as.numeric(substr(basename(x), 4, 7)))
}) |>
  dplyr::bind_rows()

names(d) <- c("name", "sex", "n", "year")

summ <- d |>
  summarise(
    n = sum(n),
    ny = n_distinct(year),
    .by = c("name", "sex")
  )

# filter(summ, n >= 500, ny >= 50) |>
#   arrange(n, ny)

d2 <- filter(d,
  name %in% filter(summ, n >= 1000, ny >= 20)$name)

arrow::write_feather(d2, "babynames/_data/babynames.feather")


# ---------------- total names registered ---------------- #

html <- rvest::read_html("https://www.ssa.gov/oact/babynames/numberUSbirths.html")

tbl <- rvest::html_nodes(html, "table")[[1]] |>
  rvest::html_table() |>
  dplyr::mutate_at(-1, readr::parse_number)
names(tbl) <- c("year", "M", "F", "total")

readr::write_csv(select(tbl, year, total),
  file = "babynames/_data/total_by_year.csv")

tbl |>
  select(-total) |>
  tidyr::pivot_longer(cols = -year, names_to = "sex", values_to = "total") |>
  readr::write_csv(, file = "babynames/_data/total_by_year_sex.csv")
