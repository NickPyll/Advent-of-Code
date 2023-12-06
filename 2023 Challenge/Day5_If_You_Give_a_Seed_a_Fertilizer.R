# Day 5 If You Give a Seed a Fertilizer----
## Part 1 ----
# 
# 
# 

library(tidyverse)
library(sqldf)

source('data/input_day5_test.r')

# Part 2

source('data/input_day5.r')

seed <-
  tribble(
    ~src_start, ~length,
    # 629551616, 310303897,
    # 265998072, 58091853,
    # 3217788227, 563748665,
    # 2286940694, 820803307,
    # 1966060902, 108698829,
    # 190045874, 3206262,
    # 4045963015, 223661537,
    1544688274, 293696584
    # ,
    # 1038807941, 31756878,
    # 1224711373, 133647424
  ) |>
  slice(rep(seq_len(n()), length)) |>
  group_by(src_start) |>
  mutate(counter = row_number()) |>
  ungroup() |>
  mutate(seed = src_start + counter - 1) |>
  select(seed)


# list.dfs <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
# list for an apply function but soooooo tired

mapping_seed_to_soil <-
  sqldf("SELECT a.seed,
                a.seed + b.dest_start - b.src_start as soil
         FROM seed a, seed_to_soil b
         WHERE a.seed >= src_start AND
               a.seed < src_start + length") |>
  full_join(seed, by = 'seed') |>
  mutate(soil = coalesce(soil, seed))

mapping_soil_to_fertilizer <-
  sqldf("SELECT a.seed,
                a.soil,
                a.soil + b.dest_start - b.src_start as fertilizer
         FROM mapping_seed_to_soil a, soil_to_fertilizer b
         WHERE a.soil >= src_start AND
               a.soil < src_start + length") |>
  full_join(mapping_seed_to_soil |> select(seed, soil), by = c('seed', 'soil')) |>
  mutate(fertilizer = coalesce(fertilizer, soil))

mapping_fertilizer_to_water <-
  sqldf("SELECT a.seed,
                a.soil,
                a.fertilizer,
                a.fertilizer + b.dest_start - b.src_start as water
         FROM mapping_soil_to_fertilizer a, fertilizer_to_water b
         WHERE a.fertilizer >= src_start AND
               a.fertilizer < src_start + length") |>
  full_join(mapping_soil_to_fertilizer |> select(seed, soil, fertilizer), by = c('seed', 'soil', 'fertilizer')) |>
  mutate(water = coalesce(water, fertilizer))

mapping_water_to_light <-
  sqldf("SELECT a.seed,
                a.soil,
                a.fertilizer,
                a.water,
                a.water + b.dest_start - b.src_start as light
         FROM mapping_fertilizer_to_water a, water_to_light b
         WHERE a.water >= src_start AND
               a.water < src_start + length") |>
  full_join(mapping_fertilizer_to_water |> select(seed, soil, fertilizer, water),
            by = c('seed', 'soil', 'fertilizer', 'water')) |>
  mutate(light = coalesce(light, water))

mapping_light_to_temperature <-
  sqldf("SELECT a.seed,
                a.soil,
                a.fertilizer,
                a.water,
                a.light,
                a.light + b.dest_start - b.src_start as temperature
         FROM mapping_water_to_light a, light_to_temperature b
         WHERE a.light >= src_start AND
               a.light < src_start + length") |>
  full_join(mapping_water_to_light |> select(seed, soil, fertilizer, water, light),
            by = c('seed', 'soil', 'fertilizer', 'water', 'light')) |>
  mutate(temperature = coalesce(temperature, light))

mapping_temperature_to_humidity <-
  sqldf("SELECT a.seed,
                a.soil,
                a.fertilizer,
                a.water,
                a.light,
                a.temperature,
                a.temperature + b.dest_start - b.src_start as humidity
         FROM mapping_light_to_temperature a, temperature_to_humidity b
         WHERE a.temperature >= src_start AND
               a.temperature < src_start + length") |>
  full_join(mapping_light_to_temperature |> select(seed, soil, fertilizer, water, light, temperature),
            by = c('seed', 'soil', 'fertilizer', 'water', 'light', 'temperature')) |>
  mutate(humidity = coalesce(humidity, temperature))

mapping_humidity_to_location <-
  sqldf("SELECT a.seed,
                a.soil,
                a.fertilizer,
                a.water,
                a.light,
                a.temperature,
                a.humidity,
                a.humidity + b.dest_start - b.src_start as location
         FROM mapping_temperature_to_humidity a, humidity_to_location b
         WHERE a.humidity >= src_start AND
               a.humidity < src_start + length") |>
  full_join(mapping_temperature_to_humidity |> select(seed, soil, fertilizer, water, light, temperature, humidity),
            by = c('seed', 'soil', 'fertilizer', 'water', 'light', 'temperature', 'humidity')) |>
  mutate(location = coalesce(location, humidity))


