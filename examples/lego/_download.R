# -------------------------------------------------------- #
# download data from rebrickable - only run once           #
# -------------------------------------------------------- #

library(rvest)

page <- read_html("https://rebrickable.com/downloads/")

hrefs <- html_elements(page, "li > span > a") %>% html_attr("href")

for (href in hrefs) {
  fl <- strsplit(basename(href), "\\?")[[1]][1]
  dest <- file.path("examples/lego/_data/rebrickable", fl)
  download.file(href, destfile = dest)
}
