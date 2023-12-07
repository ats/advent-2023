#seed to soil
# 50 98 2
# 52 50 48
#
# The first line has a destination range start of 50, a source range start of
# 98, and a range length of 2. This line means that the source range starts at
# 98 and contains two values: 98 and 99. The destination range is the same
# length, but it starts at 50, so its two values are 50 and 51. With this
# information, you know that seed number 98 corresponds to soil number 50 and
# that seed number 99 corresponds to soil number 51.

# The second line means that the source range starts at 50 and contains 48
# values: 50, 51, ..., 96, 97. This corresponds to a destination range starting
# at 52 and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53
# corresponds to soil number 55.

library(tidyr)
library(dplyr)

seeds <- read.csv("day_05/test_data.txt", sep=" ", header=FALSE, nrows = 1)
seeds <- seeds |> pivot_longer(cols = 2:length(seeds), values_to = "seed_num") |>
  select(seed_num)

get_map <- function(file="day_05/test_data.txt", start_row=1, nrow=-1) {
  data <- read.csv(file, header = FALSE, sep=" ", skip = start_row-1, nrows = nrow,
                   col.names = c("dest_start", "source_start", "range"), colClasses = "numeric")
  return(data)
}

# test data
# seed_soil <- get_map(start_row = 4, nrow = 2)
# soil_fertilizer <- get_map(start_row = 8, nrow = 3)
# fertilizer_water <- get_map(start_row = 13, nrow = 4)
# water_light <- get_map(start_row = 19, nrow = 2)
# light_temp <- get_map(start_row = 23, nrow = 3)
# temp_humidity <- get_map(start_row = 28, nrow = 2)
# humidity_location <- get_map(start_row = 32, nrow = 2)

# seed_soil <- get_map(file = "day_05/input.txt", start_row = 4, nrow = 36)
# soil_fertilizer <- get_map(file = "day_05/input.txt", start_row = 42, nrow = 24)
# fertilizer_water <- get_map(file = "day_05/input.txt", start_row = 13, nrow = 4)
# water_light <- get_map(file = "day_05/input.txt", start_row = 19, nrow = 2)
# light_temp <- get_map(file = "day_05/input.txt", start_row = 23, nrow = 3)
# temp_humidity <- get_map(file = "day_05/input.txt", start_row = 28, nrow = 2)
# humidity_location <- get_map(file = "day_05/input.txt", start_row = 32, nrow = 2)

# gross
source_max <- rbind(seed_soil, soil_fertilizer, fertilizer_water, water_light, light_temp,
      temp_humidity, humidity_location) |> select(source_start) |> max()
range_max <- rbind(seed_soil, soil_fertilizer, fertilizer_water, water_light, light_temp,
                   temp_humidity, humidity_location) |> select(range) |> max()

# functionalize that
make_map <- function(source_map=NULL, names=c("V1", "V2")) {
  # return(source_map)
  output_map <- tibble()
  for (item in 1:nrow(source_map)) {
    start <- source_map[item,]$source_start
    end <- source_map[item,]$source_start + source_map[item,]$range - 1
    dest_start <- source_map[item,]$dest_start
    dest_end <- source_map[item,]$dest_start + source_map[item,]$range - 1

    output_map <- rbind(output_map, cbind(start:end, dest_start:dest_end))
  }
  output_map <- output_map |> arrange(V1)
  fill <- tibble(0:(source_max+range_max))
  names(fill) <- "fill"
  output_map <- output_map |> full_join(fill, join_by(V1==fill)) |>
  arrange(fill)
  output_map$V2 <- coalesce(output_map$V2, output_map$V1)
  coalesce(output_map$V2, 0)
  names(output_map) <- names

  return(as_tibble(output_map))
}

s_soil <- make_map(seed_soil, names=c("seed", "soil"))
soil_fert <- make_map(soil_fertilizer, names=c("soil", "fertilizer"))
fert_wat <- make_map(fertilizer_water, names=c("fertilizer", "water"))
watr_lite <- make_map(water_light, names=c("water", "light"))
lite_temp <- make_map(light_temp, names=c("light", "temp"))
temp_humd <- make_map(temp_humidity, names=c("temp", "humidity"))
humd_loc <- make_map(humidity_location, names=c("humidity", "location"))

locations <- s_soil |> left_join(soil_fert, join_by(soil)) |>
  left_join(fert_wat, join_by(fertilizer)) |>
  left_join(watr_lite, join_by(water)) |>
  left_join(lite_temp, join_by(light)) |>
  left_join(temp_humd, join_by(temp)) |>
  left_join(humd_loc, join_by(humidity))

locations |> filter(seed %in% seeds$seed_num) |> select(location) |> min()

## prod data
seed_soil <- get_map(file = "day_05/input.txt", start_row = 4, nrow = 36)
soil_fertilizer <- get_map(file = "day_05/input.txt", start_row = 42, nrow = 24)
fertilizer_water <- get_map(file = "day_05/input.txt", start_row = 68, nrow = 34)
water_light <- get_map(file = "day_05/input.txt", start_row = 104, nrow = 46)
light_temp <- get_map(file = "day_05/input.txt", start_row = 152, nrow = 29)
temp_humidity <- get_map(file = "day_05/input.txt", start_row = 183, nrow = 30)
humidity_location <- get_map(file = "day_05/input.txt", start_row = 215, nrow = 38)

# walk one by one?

seeds <- read.csv("day_05/input.txt", sep=" ", header=FALSE, nrows = 1)
seeds <- seeds |> pivot_longer(cols = 2:length(seeds), values_to = "seed_num") |>
  select(seed_num)
seeds


make_map2 <- function(seed=NULL, tomap=NULL) {
  output <- tibble()
  map <- arrange(tomap, source_start)
  high_range <- NULL
  # for (nseed in 1:nrow(seeds)) {
  #   seed <- seeds[nseed,]$seed_num
  for (start in 1:nrow(map)) {
    if (map[start,]$source_start <= seed & seed <= (map[start,]$source_start + map[start,]$range)) {
    # if (map[start,]$source_start + map[start,]$range <= seed) {
      #print (map[start,])
      high_range <- map[start,]
    }
    if(is.null(high_range)) {
      dest_map <- seed
    } else {
      dest_map <- high_range$dest_start - high_range$source_start + seed
      # dest_map <- high_range$dest_start + seed
    }
    #print(dest_map)
    #rbind(output, seed, dest_map)
  }
  # }
  #print(output)
  return(dest_map)
}

seed <- seeds[1,]
map <- seed_soil


lowest_loc <- tibble()
for (seed in 1:nrow(seeds)) {
  A <- make_map2(seeds[seed,]$seed_num, tomap=seed_soil)
  B <- make_map2(A, tomap=soil_fertilizer)
  C <- make_map2(B, tomap=fertilizer_water)
  D <- make_map2(C, tomap=water_light)
  E <- make_map2(D, tomap=light_temp)
  F <- make_map2(E, tomap=temp_humidity)
  loc <- make_map2(F, tomap=humidity_location)
  lowest_loc <- rbind(lowest_loc, loc)

}
lowest_loc
min(lowest_loc)

