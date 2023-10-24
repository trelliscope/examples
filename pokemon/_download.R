load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_7261/datasets/pokemon.Rdata"))

pokemon <- pokemon |>
  mutate(evolves_from_species_id = as.integer(evolves_from_species_id))

readr::write_rds(pokemon, "pokemon/_data/pokemon.rds")
