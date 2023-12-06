seed <- c(79, 14, 55, 13) |> 
  as.data.frame() 

colnames(seed) <- c('seed')

seed_to_soil <-
  tribble(
    ~dest_start, ~src_start, ~length,
    50, 98, 2,
    52, 50, 48
  ) |> 
  as.data.frame()

soil_to_fertilizer <-
  tribble(
    ~dest_start, ~src_start, ~length,
    0, 15, 37,
    37, 52, 2,
    39, 0, 15
  ) |> 
  as.data.frame()

fertilizer_to_water <-
  tribble(
    ~dest_start, ~src_start, ~length,
    49, 53, 8,
    0, 11, 42,
    42, 0, 7,
    57, 7, 4
  ) |> 
  as.data.frame()

water_to_light <-
  tribble(
    ~dest_start, ~src_start, ~length,
    88, 18, 7,
    18, 25, 70
  ) |> 
  as.data.frame()

light_to_temperature <-
  tribble(
    ~dest_start, ~src_start, ~length,
    45, 77, 23,
    81, 45, 19,
    68, 64, 13
  ) |> 
  as.data.frame()

temperature_to_humidity <-
  tribble(
    ~dest_start, ~src_start, ~length,
    0, 69, 1,
    1, 0, 69
  ) |> 
  as.data.frame()

humidity_to_location <-
  tribble(
    ~dest_start, ~src_start, ~length,
    60, 56, 37,
    56, 93, 4
  ) |> 
  as.data.frame()
