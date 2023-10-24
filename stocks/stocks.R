library(trelliscope)
library(dplyr)
library(quantmod)
library(vroom)
library(plotly)
library(dpseg)
source("stocks/_fns.R")

# system("cd stocks/_data/stock-data && git pull")

dpath <- "stocks/_data/stock-data/output"

# -------------------- read price data ------------------- #

ff <- list.files(file.path(dpath, "prices"), full.names = TRUE)
names(ff) <- tools::file_path_sans_ext(basename(ff))

d <- lapply(ff, read_and_calc) # about a minute
pd <- bind_rows(d)

# ------------------- read fundamentals ------------------ #

md <- get_fundamentals(dpath)

# --------------------- read equities -------------------- #

eqs <- readr::read_csv(file.path(dpath, "equities.csv.gz"),
  show_col_types = FALSE, progress = FALSE) |>
  filter(!(symbol == "ECC" & exchange == "ASE"))

# ----------------------- join data ---------------------- #

keep <- c("symbol", "summary", "sector", "industry_group", "industry",
  "exchange", "market", "state", "website", "market_cap")

d <- dplyr::inner_join(
  dplyr::inner_join(
    dplyr::select(md, -all_of(c("price (usd)", "country"))),
    dplyr::select(eqs, all_of(keep)) |> distinct(),
    by = "symbol"
  ),
  pd,
  by = "symbol"
) |>
  distinct() |>
  rename(market_cap_cat = market_cap) |>
  mutate(
    website = href(website),
    tradingview = href(paste0("https://www.tradingview.com/chart/?symbol=",
      symbol)),
    total_assets = number(total_assets + 1, log = TRUE),
    cost_to_borrow = number(cost_to_borrow, log = TRUE),
    cash_on_hand = number(cash_on_hand + 1, log = TRUE),
    total_debt = number(total_debt + 1, log = TRUE),
    employees_count = number(employees_count + 1, log = TRUE),
    total_liabilities = ifelse(total_liabilities <= 0, NA, total_liabilities),
    total_liabilities = number(total_liabilities, log = TRUE),
    marketcap = number(marketcap, log = TRUE)
    # net_assets = number(net_assets, log = TRUE),
    # dividend_yield_ttm = number(dividend_yield_ttm, log = TRUE)
  )

# readr::write_csv(dplyr::select(d, symbol),
#   "stocks/_data/stock-data/src/symbols.csv")

d <- set_stock_data_labels(d)
d <- set_tags(d,
  info = intersect(names(d), names(eqs)),
  fundamentals = setdiff(intersect(names(d), names(md)), c("name", "symbol")),
  metrics = setdiff(intersect(names(d), names(pd)), "symbol"),
)

d <- mutate(d, chart = panel_lazy(plot_fn))
# d$chart[[4]]

# --------------------- build display -------------------- #

td <- d |>
  as_trelliscope_df(
    name = "stocks",
    description = "Stock prices over the past year for selected companies",
    key_cols = c("symbol", "name"),
    path = "_public/stocks"
  ) |>
  set_panel_options(
    prices = panel_options(width = 600, height = 500, force = TRUE)
  ) |>
  set_default_labels(c("symbol", "name", "website", "tradingview")) |>
  set_default_layout(ncol = 1, sidebar = TRUE,
    visible_filters = c("industry_group", "rsi_cat", "obv_dir", "cur_rsi"))

view_trelliscope(td) # about 6 minutes
