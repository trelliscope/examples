# https://api.nasa.gov
# hourly limit: 1,000 requests per hour
library(httr)

api_key <- Sys.getenv("NASA_API_KEY")

rovers <- c("curiosity", "opportunity", "perseverance", "spirit")

rvdat <- list()
for (rv in rovers) {
  url <- paste0("https://api.nasa.gov/mars-photos/api/v1/manifests/", rv,
    "?api_key=", api_key)
  tmp <- httr::GET(url)
  rvdat[[rv]] <- httr::content(tmp)
}

for (x in rvdat) {
  nm <- tolower(x[[1]]$name)
  message(nm)
  sols <- sapply(x[[1]]$photos, function(x) x$sol)
  res <- vector(mode = "list", length = length(sols))
  for (ii in cli::cli_progress_along(sols)) {
    url <- paste0("https://api.nasa.gov/mars-photos/api/v1/rovers/", nm,
      "/photos?sol=", sols[ii], "&api_key=", api_key)
    tmp <- httr::GET(url)
    tmp2 <- httr::content(tmp)
    if (!is.null(tmp2$error$code))
      stop(tmp2$error$code)
    res[[ii]] <- dplyr::bind_rows(lapply(tmp2$photos, function(p) {
      dplyr::tibble(
        id = p$id,
        sol = p$sol,
        camera_id = p$camera$id,
        camera_name = p$camera$name,
        camera_full_name = p$camera$full_name,
        img_src = p$img_src,
        earth_date = p$earth_date,
        rover_name = p$rover$name
      )
    }))
    Sys.sleep(3)
  }
  res2 <- dplyr::bind_rows(res)
  readr::write_rds(res2, paste0("examples/mars/_data/", nm, ".rds"))
}

ff <- list.files("examples/mars/_data", full.names = TRUE,
  pattern = "^cur|^opp|^per|^spi")

all <- dplyr::bind_rows(lapply(ff, readr::read_rds))

readr::write_rds(all, "examples/mars/_data/all.rds")



library(dplyr)
library(magick)
library(imager)
library(scales)
library(progress)

get_colorpal <- function(im, n = 8, cs = "RGB") {
  #print(cs)
  tmp <- im |>
    image_resize("100") |>
    image_quantize(max = n, colorspace = cs) |>
    magick2cimg() |>
    RGBtoHSV() |>
    as.data.frame(wide = "c") |>
    mutate(
      hex = hsv(rescale(c.1, from = c(0, 360)), c.2, c.3),
      hue = c.1,
      sat = c.2,
      value = c.3
    ) |>
    count(hex, hue, sat, value, sort = TRUE) |>
    mutate(colorspace = cs)
  return(select(tmp, colorspace, hex, hue, sat, value, n))
}

srcs <- all$img_src
res <- vector(mode = "list", length = length(srcs))
names(res) <- srcs

idx <- which(unlist(lapply(res, is.null)))
srcs2 <- srcs[idx]

pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = length(srcs2), clear = FALSE)

for (src in srcs2) {
  pb$tick()
  a <- try(image_read(src), silent = TRUE)
  if (!inherits(a, "try-error"))
    res[[src]] <- tibble(
      img_src = src,
      select(image_info(a), -colorspace),
      head(get_colorpal(a), 1)
    )
}

res2 <- bind_rows(res)

readr::write_rds(res2, "examples/mars/_data/stats.rds")

sum(res2$filesize, na.rm = TRUE) / 1024^3
# [1] 124 GB

d <- left_join(all,
  select(res2, -c(colorspace, matte, density)),
  by = "img_src")

arrow::write_parquet(d, "examples/mars/_data/mars.parquet")

tmp <- jsonlite::as_gzjson_raw(d)

jsonlite::write_json(d, "examples/mars/_data/mars.json")
gzip("examples/mars/_data/mars.json")

arrow::write_ipc_file(d, "examples/mars/_data/mars.arrow")
tmp <- base64enc::base64encode("examples/mars/_data/mars.arrow")
writeLines(paste0("__callback__(\"", tmp, "\")"),
  "examples/mars/_data/mars.arrow.jsonp")

tmp2 <- readLines(base64::encode("examples/mars/_data/mars.arrow", linebreaks = FALSE))
writeLines(paste0("__callback__(\"", tmp2, "\")"),
  "examples/mars/_data/mars2.arrow.jsonp")

tmp3 <- xfun::base64_encode("examples/mars/_data/mars.arrow")
writeLines(paste0("__callback__(\"", tmp3, "\")"),
  "examples/mars/_data/mars3.arrow.jsonp")
