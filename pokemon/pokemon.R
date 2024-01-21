library(trelliscopejs)
library(dplyr)
 
d <- readr::read_rds("pokemon/_data/pokemon.rds") |>
  mutate(
    url_image = panel_url(url_image),
    total = attack + defense + hp + special_attack + special_defense + speed
  ) |>
  select(-all_of(c("id", "species_id", "color_1", "color_2", "color_f",
    "shape_id", "pokebase")))

d <- set_var_labels(d,
  height = "The height of the Pokémon in decimetres",
  weight = "The weight of the Pokémon in hectograms",
  base_experience = "The base experience gained for defeating this Pokémon",
  type_1 = "The primary type of the Pokémon",
  type_2 = "The secondary type of the Pokémon",
  attack = "The base attack of the Pokémon",
  defense = "The base defense of the Pokémon",
  hp = "The base HP of the Pokémon",
  special_attack = "The base special attack of the Pokémon",
  special_defense = "The base special defense of the Pokémon",
  speed = "The base speed of the Pokémon",
  total = "Total of HP, attack, defense, special attack, special defense, speed",
  ability_1 = "The primary ability of the Pokémon",
  ability_2 = "The secondary ability of the Pokémon",
  ability_hidden = "The hidden ability of the Pokémon",
  egg_group_1 = "The primary egg group of the Pokémon",
  egg_group_2 = "The secondary egg group of the Pokémon",
  generation_id = "ID of the generation this species first appeared in",
  evolves_from_species_id = "The species from which this one evolves",
  evolution_chain_id = "ID of the species' evolution chain (a.k.a. family)",
  shape = "The body shape of the Pokémon"
)

pk <- d |>
  as_trelliscope_df(
    name = "pokemon",
    path = "_public/pokemon"
  ) |>
  set_default_layout(ncol = 6) |>
  set_default_labels(c("pokemon", "type_1", "ability_1", "pokedex"))

view_trelliscope(pk)

# xcard that shows picture and overall distribution of attack, defense, hp, special attack, special defense, speed and then shows where this one is in the distribution (filling the distribution with color)
