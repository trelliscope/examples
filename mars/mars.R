# -------------------------------------------------------- #
# 1.1 million Mars Rover images                            #
# -------------------------------------------------------- #

library(trelliscope)
library(dplyr)

d <- arrow::read_ipc_file("mars/_data/mars.arrow")

dt <- d |>
  filter(nchar(img_src) != 63) |>
  mutate(
    img_src = panel_url(img_src),
    earth_date = as.Date(earth_date),
    camera = camera_full_name,
    hue = log2(hue + 1)
  ) |>
  select(-camera_name, -camera_full_name, -sat, -value, -n) |>
  as_trelliscope_df(
    name = "Mars Rover Photos",
    description = "Image data gathered by NASA's Curiosity, Opportunity, and Spirit rovers on Mars",
    path = "_public/mars",
    key_cols = "id"
  ) |>
  set_default_layout(ncol = 4, sidebar = TRUE,
    visible_filters = c("rover_name", "sol")) |>
  set_default_filters(
    filter_range("sol", min = 3000),
    filter_string("rover_name", values = "Curiosity")
  ) |>
  add_inputs(input_text(name = "comments",
    label = "Comments about this image", height = 5)) |>
  set_default_labels(c("rover_name", "camera", "earth_date"))

dt <- dt |>
  set_var_labels(
    id = "Image identifier",
    sol = "Mars solar day of image capture",
    class = "Image classification",
    camera = "Camera type",
    earth_date = "Earth date of image capture",
    rover_name = "Name of the Mars rover",
    width = "Image width in pixels",
    height = "Image height in pixels",
    filesize = "File size in bytes",
    hex = "Hex color code for primary quantized color",
    hue = "HSV hue value for primary quantized color (log2 + 1)"
    # sat = "HSV saturation for primary quantized color",
    # value = "HSV 'value' for primary quantized color"
  )

dt <- dt |>
  set_tags(
    dates = c("earth_date", "sol"),
    `image attributes` = c("width", "height", "filesize", "hex", "hue")
  )

dt <- dt |>
  set_theme(
    primary = "#c80000",
    primary2 = "#f00000",
    primary3 = "#960000",
    background = "#222222",
    background2 = "#444444",
    background3 = "#333333",
    bars = "#c80000",
    text = "#ffffff",
    text2 = "#ffffff",
    text_disabled = "#bcbcbc",
    logo = rover_icon_b64
  )

view_trelliscope(dt)
