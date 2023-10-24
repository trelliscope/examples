library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

bucket <- s3_bucket("voltrondata-labs-datasets/nyc-taxi")

copy_files(from = bucket, to = "_ignore/arrow_websockets/nyc-taxi")

ds <- open_dataset("examples/12-bigdata/nyc-taxi") %>%
  mutate(
    day = lubridate::day(pickup_datetime),
    hour = lubridate::hour(pickup_datetime)
  )

nrow(ds)
# [1] 1,672,590,319

dsumm <- ds %>%
  group_by(year, month, day) %>%
  summarise(
    n = n()
    # med_dist = median(trip_distance),
    # med_fare = median(total_amount)
  ) %>%
  collect() %>%
  ungroup() %>%
  mutate(
    date = lubridate::make_date(year, month, day),
    wday = lubridate::wday(date, label = TRUE)
  ) %>%
  arrange(date)

ggplot(dsumm, aes(date, med_fare)) + geom_line()
ggplot(dsumm, aes(date, med_dist)) + geom_line()
ggplot(dsumm, aes(date, n)) + geom_line()

x <- filter(ds, year == 2019, month == 1, day == 1)

plot_fn <- function(x) {
  x <- x %>%
    count(hour) %>%
    collect()
  ggplot(x, aes(hour, n)) +
    geom_point() +
    geom_segment(aes(xend = hour, yend = 0)) +
    xlim(0, 23) +
    theme_minimal()
}

plot_fn(filter(ds, year == 2019, month == 1, day == 1))

dsumm <- dsumm %>%
  mutate(hourly_ct = panel_lazy(plot_fn, ds))

dsumm$hourly_ct[[1]]

dsumm <- dsumm %>%
  as_trelliscope_df(name = "NYC Taxi") %>%
  set_default_labels(c("date", "n", "wday")) %>%
  set_default_sort("date") %>%
  set_default_layout(ncol = 4) %>%
  set_panel_options(hourly_ct = panel_options(prerender = FALSE))

view_trelliscope(dsumm)

count(ds, vendor_name) %>% collect() %>% arrange(-n)
# vendor_name         n
# VTS         876306482
# CMT         779404714
# DDS          14169149
# NA            2709974

count(ds, rate_code) %>% collect() %>% arrange(-n)
# rate_code                      n
# Standard rate         1461241211
# NA                     173863437
# JFK                     28452154
# Negotiated               4572556
# Newark                   2551219
# Nassau or Westchester    1890544
# Group ride                 19198

locstats <- ds %>%
  mutate(
    has_latlon = !is.na(pickup_latitude) & !is.na(pickup_longitude) &
      !is.na(dropoff_latitude) & !is.na(dropoff_longitude),
    has_loc_id = !is.na(pickup_location_id) & !is.na(dropoff_location_id)
  ) %>%
  count(has_latlon, has_loc_id) %>%
  collect()
# has_latlon has_loc_id          n
# TRUE       FALSE      1249144675
# FALSE      FALSE            7685
# FALSE      TRUE        423437959
# ~ 75% has latlon, 25% has loc_id


sf::st_bbox(sf::st_transform(zones, 4326))

ds %>%
  filter(
    pickup_latitude < 40.63592,
    pickup_latitude > 40.49611,
    dropoff_latitude < 40.91554,
    dropoff_latitude > 40.49611,
    pickup_longitude < -73.70002,
    pickup_longitude > -74.2556,
    dropoff_longitude < -73.70002,
    dropoff_longitude > -74.2556
  ) %>%
  count() %>%
  collect()
# 1222059570 / 1249144675 = 97.8% of data in NYC

seq(40.49611, 40.91554, length = 10)
seq(-74.2556, -73.70002, length = 10)

# one square on 9x9 grid
ds %>%
  filter(
    pickup_latitude < 40.63592,
    pickup_latitude > 40.58932,
    dropoff_latitude < 40.63592,
    dropoff_latitude > 40.58932,
    pickup_longitude < -74.07041,
    pickup_longitude > -74.13214,
    dropoff_longitude < -74.07041,
    dropoff_longitude > -74.13214
  ) %>%
  count() %>%
  collect()
# 16379

locs <- ds %>%
  group_by(pickup_location_id, dropoff_location_id) %>%
  summarise(
    n = n(),
    med_dist = median(trip_distance),
    med_fare = median(total_amount)
  ) %>%
  collect() %>%
  ungroup() %>%
  arrange(-n) %>%
  mutate(pct = 100 * n / sum(n))



plot(seq(0, 1, 0.001), quantile(log10(locs$n[-1]), seq(0, 1, 0.001)))

loclook <- readr::read_csv("examples/12-bigdata/taxi_zone_lookup.csv")

a <- collect(head(ds))
# a$dropoff_datetime - a$pickup_datetime

zones <- sf::read_sf("examples/12-bigdata/taxi_zones")

mapview::mapview(zones)

filter(zones, LocationID %in% c(10, 263)) %>% mapview::mapview()

locs <- locs %>%
  left_join(
    rename_all(loclook, function(x) paste0("pickup_", x)),
    by = "pickup_location_id"
  ) %>%
  left_join(
    rename_all(loclook, function(x) paste0("dropoff_", x)),
    by = "dropoff_location_id"
  )

ggplot(locs, aes(med_dist, med_fare)) + geom_point()
ggplot(filter(locs, n >= 1000), aes(med_dist, med_fare)) + geom_point(alpha = 0.5)

x <- filter(ds,
  year == 2019,
  pickup_location_id == 10,
  dropoff_location_id == 265
) %>%
  collect()

# https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page

plot_fn <- function(x) {

}





system.time(ds %>%
  filter(total_amount > 100, year == 2015) %>%
  select(tip_amount, total_amount, passenger_count) %>%
  mutate(tip_pct = 100 * tip_amount / total_amount) %>%
  group_by(passenger_count) %>%
  summarise(
    median_tip_pct = median(tip_pct),
    n = n()
  ) %>%
  collect() %>%
  print())

d <- collect(ds)

range(d$year)

d %>%
  count(pickup_location_id, dropoff_location_id) %>%
  arrange(-n)

tmp <- arrow::read_parquet("~/Downloads/yellow_tripdata_2023-01.parquet")
