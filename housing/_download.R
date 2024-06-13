# https://www.zillow.com/research/data/
# https://documenter.getpostman.com/view/9197254/UVsFz93V

cnty_meta <- list(
  list(
    desc = "ZHVI Single-Family Homes Time Series ($)",
    type = "all",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  ),
  list(
    desc = "ZHVI 1-Bedroom Time Series ($)",
    type = "1 bed",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  ),
  list(
    desc = "ZHVI 2-Bedroom Time Series ($)",
    type = "2 bed",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  ),
  list(
    desc = "ZHVI 3-Bedroom Time Series ($)",
    type = "3 bed",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  ),
  list(
    desc = "ZHVI 4-Bedroom Time Series ($)",
    type = "4 bed",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  ),
  list(
    desc = "ZHVI 5+ Bedroom Time Series ($)",
    type = "5+ bed",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  )
)

cnty_nm_map <- c(
  "county" = "RegionName",
  "state" = "State",
  "metro" = "Metro",
  "state_fips" = "StateCodeFIPS",
  "muni_fips" = "MunicipalCodeFIPS"
)

cdat <- lapply(cnty_meta, function(x) {
  readr::read_csv(x$file) |>
  tidyr::pivot_longer(-c(1:9), names_to = "date", values_to = "zhvi") |>
  dplyr::mutate(date = lubridate::as_date(date)) |>
  dplyr::filter(!is.na(zhvi)) |>
  dplyr::select(-c("RegionID", "SizeRank", "RegionType", "StateName")) |>
  dplyr::rename(!!! cnty_nm_map) |>
  dplyr::mutate(home_type = x$type)
}) |>
  bind_rows()

tidyr::nest(cdat, data = -c(1:5)) |>
  readr::write_rds("housing/_data/county.rds")

# this has weekly as well... change "month" to "week" in the url
metro_meta <- list(
  list(
    desc = "ZHVI Single-Family",
    file = "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1696990222"
  ),
  list(
    desc = "Median List Price",
    file = "https://files.zillowstatic.com/research/public_csvs/mlp/Metro_mlp_uc_sfrcondo_sm_month.csv?t=1696990222"
  ),
  list(
    desc = "Median Sale Price",
    file = "https://files.zillowstatic.com/research/public_csvs/median_sale_price/Metro_median_sale_price_uc_sfrcondo_sm_month.csv?t=1696990222"
  ),
  list(
    desc = "Percent of Homes Sold Below List",
    file = "https://files.zillowstatic.com/research/public_csvs/pct_sold_below_list/Metro_pct_sold_below_list_uc_sfrcondo_sm_month.csv?t=1696990222"
  ),
  list(
    desc = "Median Days to Close",
    file = "https://files.zillowstatic.com/research/public_csvs/median_days_to_close/Metro_median_days_to_close_uc_sfrcondo_sm_month.csv?t=1696990222"
  ),
  list(
    desc = "Share of Listings With a Price Cut",
    file = "https://files.zillowstatic.com/research/public_csvs/perc_listings_price_cut/Metro_perc_listings_price_cut_uc_sfrcondo_sm_month.csv?t=1696990222"
  ),
  list(
    desc = "For-Sale Inventory", # (Smooth, SFR Only, Monthly)
    file = "https://files.zillowstatic.com/research/public_csvs/invt_fs/Metro_invt_fs_uc_sfr_sm_month.csv?t=1696990222"
  ),
  list(
    desc = "ZORI All Homes",
    file = "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfr_sm_sa_month.csv?t=1718258158"
    # file = "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_month.csv?t=1696990222"
  )
)

metro_nm_map <- c(
  "metro" = "RegionName",
  "state" = "StateName"
)

mdat <- lapply(metro_meta, function(x) {
  message(x$file)
  readr::read_csv(x$file) |>
  dplyr::filter(RegionType != "country") |>
  tidyr::pivot_longer(-c(1:5), names_to = "date", values_to = "value") |>
  dplyr::mutate(date = lubridate::as_date(date)) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::select(-c("RegionID", "SizeRank", "RegionType")) |>
  dplyr::rename(!!! metro_nm_map) |>
  dplyr::mutate(metric = x$desc)
}) |>
  bind_rows()

filter(mdat, metric == "Percent of Homes Sold Below List") |> pull(value) |> range()
# [1] 0.06917615 0.94397759
filter(mdat, metric == "Share of Listings With a Price Cut") |> pull(value) |> range()
# [1] 0.009259259 0.481533129

tidyr::nest(mdat, data = -c(1:2)) |>
  readr::write_rds("housing/_data/metro.rds")

# download.file("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv", destfile = "housing/_data/census.csv")

# cncs <- readr::read_csv("housing/_data/census.csv") |>
#   select(STATE, COUNTY, POPESTIMATE2022)
# names(cncs) <- c("state_fips", "muni_fips", "pop")

# readr::write_rds(cncs, "housing/_data/census.rds")

# metro_meta <- list(
#   list(
#     desc = "Median List Price (Smooth, All Homes, Monthly)",
#     file = "https://files.zillowstatic.com/research/public_csvs/mlp/Metro_mlp_uc_sfrcondo_sm_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Median List Price (Raw, All Homes, Monthly)",
#     file = "https://files.zillowstatic.com/research/public_csvs/mlp/Metro_mlp_uc_sfrcondo_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Median Sale Price (Smooth & Seasonally Adjusted, All Homes, Monthly)",
#     file = "https://files.zillowstatic.com/research/public_csvs/median_sale_price/Metro_median_sale_price_uc_sfrcondo_sm_sa_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Median Sale Price (Smooth, All Homes, Monthly)",
#     file = "https://files.zillowstatic.com/research/public_csvs/median_sale_price/Metro_median_sale_price_uc_sfrcondo_sm_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Median Sale Price (Raw, All Homes, Monthly)",
#     file = "https://files.zillowstatic.com/research/public_csvs/median_sale_price/Metro_median_sale_price_uc_sfrcondo_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Percent of Homes Sold Below List (Smooth, All Homes, Monthly View)",
#     file = "https://files.zillowstatic.com/research/public_csvs/pct_sold_below_list/Metro_pct_sold_below_list_uc_sfrcondo_sm_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Median Days to Close (Raw, All Homes, Monthly View)",
#     file = "https://files.zillowstatic.com/research/public_csvs/median_days_to_close/Metro_median_days_to_close_uc_sfrcondo_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Median Days to Close (Smooth, All Homes, Monthly View)",
#     file = "https://files.zillowstatic.com/research/public_csvs/median_days_to_close/Metro_median_days_to_close_uc_sfrcondo_sm_month.csv?t=1696990222"
#   ),
#   list(
#     desc = "Share of Listings With a Price Cut (Smooth, All Homes, Monthly)",
#     file = "https://files.zillowstatic.com/research/public_csvs/perc_listings_price_cut/Metro_perc_listings_price_cut_uc_sfrcondo_sm_month.csv?t=1696990222"
#   )
# )
