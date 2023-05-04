load_all("../trelliscope")

d <- mars_rover %>%
  as_trelliscope_df(
    name = "mars rover",
    description = "Selected images taken by the Mars Curiosity rover from 2014-2015, with classifications",
    path = "docs/mars"
  ) %>%
  add_meta_labels(
    id = "Image identifier",
    sol = "Mars solar day of image capture",
    class = "Image classification",
    camera = "Camera type",
    earth_date = "Earth date of image capture",
    width = "Image width in pixels",
    height = "Image height in pixels",
    filesize = "File size in bytes",
    hex = "Hex color code for primary quantized color",
    hue = "HSV hue value for primary quantized color"
  ) |>
  add_meta_tags(
    earth_date = "dates",
    sol = "dates",
    width = "image attributes",
    height = "image attributes",
    filesize = "image attributes",
    hex = "image attributes",
    hue = c("statistics", "life expectancy")
  ) |>
  set_default_layout(ncol = 5) |>
  add_inputs(
    input_text(name = "comments", label = "Comments about this image",
      width = 100, height = 6),
    input_radio(name = "correct_class",
      label = "Is the image classified correctly?", options = c("no", "yes")),
    email = "johndoe123@fakemail.net",
    vars = "class"
  ) |>
  set_default_labels(c("comments", "earth_date", "class")) |>
  write_trelliscope(d)

d

view_trelliscope(d)
