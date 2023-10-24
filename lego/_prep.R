# ------------------ preprocess the data ----------------- #

library(tidyverse)

ff <- list.files("lego/_data/rebrickable/",
  pattern = "*.csv", full.names = TRUE)
db <- lapply(ff, read_csv)
names(db) <- basename(tools::file_path_sans_ext(tools::file_path_sans_ext(ff)))

thm <- db$themes
thm_tmp <- lapply(seq_len(nrow(thm)), function(ii) {
  cur_row <- thm[ii, ]
  if (is.na(cur_row$parent_id))
    return(
      tibble(
        theme_id = cur_row$id,
        theme = cur_row$name,
        sub_theme = "[none]"
      )
    )
  parent <- filter(thm, id == cur_row$parent_id)
  prev <- cur_row
  while (!is.na(parent$parent_id)) {
    prev <- parent
    parent <- filter(thm, id == parent$parent_id)
  }
  return(
    tibble(
      theme_id = cur_row$id,
      theme = parent$name,
      sub_theme = prev$name
    )
  )
}) |>
  bind_rows()

# inventory will map to an inventory id which we can use to find parts in
# the inventory_parts table
# (can be more than one inv_id for a set_num for different versions of the set)
sets <- db$sets |>
  left_join(thm_tmp, by = "theme_id") |>
  select(-theme_id) |>
  left_join(rename(db$inventories, inv_id = id), by = "set_num") |>
  mutate(rebrickable_url = paste0("https://rebrickable.com/sets/", set_num))

get_mode <- function(x, ct = 1) {
  if (length(ct) == 1)
    ct <- rep(ct, length(x))
  tail(names(sort(table(rep(x, ct)))), 1)
}

part_summ <- db$inventory_parts |>
  group_by(inventory_id) |>
  summarise(
    n_parts = sum(quantity),
    n_spare_parts = sum(quantity[is_spare]),
    n_part_types = length(unique(part_num)),
    n_colors = length(unique(color_id)),
    prim_part_num = get_mode(part_num, quantity),
    prim_color_id = as.integer(get_mode(color_id, quantity))
  ) |>
  rename(inv_id = inventory_id) |>
  left_join(db$parts |>
    select(part_num, name) |>
    rename(prim_part_num = part_num, prim_part_name = name)
  ) |>
  filter(inv_id %in% sets$inv_id)

part_summ <- part_summ |>
  left_join(
    db$colors |>
      select(id, name, rgb) |>
      rename(prim_color_id = id, prim_color_name = name, prim_color_hex = rgb),
    by = "prim_color_id"
  ) |>
  mutate(prim_color_hex = paste0("#", prim_color_hex)) |>
  select(-prim_color_id)

# TODO: could add in primary part category...
# db$parts
# db$part_categories

filter(sets, !inv_id %in% part_summ$inv_id) |> select(name, img_url, inv_id)

fig_summ <- db$inventory_minifigs |>
  left_join(select(db$minifigs, fig_num, num_parts), by = "fig_num") |>
  group_by(inventory_id) |>
  summarise(
    n_minifigs = sum(quantity),
    n_minifig_parts = sum(num_parts * quantity),
    minifig_ids = list(unique(fig_num))
  ) |>
  rename(inv_id = inventory_id) |>
  filter(inv_id %in% sets$inv_id)

filter(sets, !inv_id %in% fig_summ$inv_id) |> select(name, img_url, inv_id)

filter(sets, inv_id == 42856) |> select(img_url)

sets <- left_join(sets, part_summ, by = "inv_id")
sets <- left_join(sets, fig_summ, by = "inv_id")

sets2 <- filter(sets, !(is.na(n_parts) & is.na(n_minifigs)))

filter(sets2, num_parts != n_parts + n_minifig_parts) |>
  select(name, rebrickable_url, num_parts, n_parts, n_minifig_parts)

sets3 <- select(sets2, -num_parts)

count(sets3, set_num) |> arrange(-n)
filter(sets3, set_num == "657-2")$version

# just want one record per set_num and get max version
set_vers <- sets3 |>
  group_by(set_num) |>
  summarise(n = n(), n_versions = length(unique(version))) |>
  filter(n > 1)
all(set_vers$n == set_vers$n_versions)

tmp <- sets3 |>
  group_by(set_num) |>
  slice_max(version) |>
  ungroup() |>
  select(-version)

sets4 <- tmp |>
  left_join(select(set_vers, -n), by = "set_num")

rtl <- read_csv("lego/_data/retail_prices.csv")

sets5 <- sets4 |>
  left_join(rtl, by = "set_num")

readr::write_rds(sets5, "lego/_data/sets.rds")

set_nums <- sets4 |>
  select(set_num, minifig_ids) |>
  unnest(cols = c(minifig_ids)) |>
  group_by(minifig_ids) %>%
  summarize(
    n_sets = n(),
    set_nums = list(set_num)
  ) |>
  rename(fig_num = minifig_ids)

figs <- db$minifigs |>
  left_join(set_nums, by = "fig_num")

write_rds(figs, "lego/_data/minifigs.rds")

color_ct <- db$inventory_parts |>
  summarise(
    n = sum(quantity),
    .by = c("inventory_id", "color_id")
  ) |>
  rename(inv_id = inventory_id) |>
  left_join(db$colors, by = c("color_id" = "id")) |>
  select(inv_id, name, rgb, is_trans, n) |>
  mutate(rgb = paste0("#", rgb))

color_ct <- filter(color_ct, inv_id %in% sets4$inv_id)

write_csv(color_ct, "lego/_data/set_color_ct.csv")

db$part_categories |> print(n = 100)

db$inventory_parts

length(unique(db$inventory_parts$part_num))

db$inventory_parts |>
  count(part_num) |>
  arrange(-n)

db$inventory_parts |>
  filter(part_num == "2343") |>
  select(part_num, color_id, img_url) |>
  distinct()
