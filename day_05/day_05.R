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
                   col.names = c("dest_start", "source_start", "range"), colClasses = "integer")
  return(data)
}

seed_soil <- get_map(start_row = 4, nrow = 2)
soil_fertilizer <- get_map(start_row = 8, nrow = 3)
fertilizer_water <- get_map(start_row = 13, nrow = 4)
water_light <- get_map(start_row = 19, nrow = 2)
light_temp <- get_map(start_row = 23, nrow = 3)
temp_humidity <- get_map(start_row = 28, nrow = 2)
humidity_location <- get_map(start_row = 32, nrow = 2)

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
  fill <- tibble(0:nrow(output_map))
  names(fill) <- "fill"
  output_map |> full_join(fill, join_by(V1==fill))
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

