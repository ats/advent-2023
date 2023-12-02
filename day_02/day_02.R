# For example, the record of a few games might look like this:
#
# Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green Game 2: 1 blue, 2
# green; 3 green, 4 blue, 1 red; 1 green, 1 blue Game 3: 8 green, 6 blue, 20
# red; 5 blue, 4 red, 13 green; 5 green, 1 red Game 4: 1 green, 3 red, 6 blue; 3
# green, 6 red; 3 green, 15 blue, 14 red Game 5: 6 red, 1 blue, 3 green; 2 blue,
# 1 red, 2 green In game 1, three sets of cubes are revealed from the bag (and
# then put back again). The first set is 3 blue cubes and 4 red cubes; the
# second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is
# only 2 green cubes.
#
# The Elf would first like to know which games would have been possible if the
# bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
#
# In the example above, games 1, 2, and 5 would have been possible if the bag
# had been loaded with that configuration. However, game 3 would have been
# impossible because at one point the Elf showed you 20 red cubes at once;
# similarly, game 4 would also have been impossible because the Elf showed you
# 15 blue cubes at once. If you add up the IDs of the games that would have been
# possible, you get 8.
#
# Determine which games would have been possible if the bag had been loaded with
# only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the
# IDs of those games?

library(dplyr)
library(stringr)
library(tidyr)

data <- read.csv("day_02/input.txt", sep = "\t", col.names = "data", header = FALSE)
head(data)
# gsub("[0-9]+ (red|green|blue)", "", data$data)
blocks <- stringr::str_extract_all(data$data, "[0-9]+ (red|green|blue)", simplify = TRUE) |>
  # head() |>
  as.data.frame()
blocks
blocks$game_id <- 1:nrow(blocks)
blocks$raw <- data[1:nrow(blocks),]

red_games <- stringr::str_extract_all(blocks$raw,
               " (\\d+ )red", simplify = TRUE) |> as.data.frame()
red_games$game_id <- 1:nrow(red_games)
green_games <- stringr::str_extract_all(blocks$raw,
               " (\\d+ )green", simplify = TRUE) |> as.data.frame()
green_games$game_id <- 1:nrow(green_games)
blue_games <- stringr::str_extract_all(blocks$raw,
               " (\\d+ )blue", simplify = TRUE) |> as.data.frame()
blue_games$game_id <- 1:nrow(blue_games)
# head(red_games)

red_filter <- " (1 |2 |3 |4 |5 |6 |7 |8 |9 |10 |11 |12 )red"
blue_filter <- " (1 |2 |3 |4 |5 |6 |7 |8 |9 |10 |11 |12 |13 |14 )blue"
green_filter <- " (1 |2 |3 |4 |5 |6 |7 |8 |9 |10 |11 |12 |13 )green"

red_games |> pivot_longer(cols=starts_with("V")) |>
  filter(value != "") |>
  mutate(meets = grepl(red_filter, value)) |>
  group_by(game_id) |>
  filter(all(meets)==TRUE) -> good_red
green_games |> pivot_longer(cols=starts_with("V")) |>
  filter(value != "") |>
  mutate(meets = grepl(green_filter, value)) |>
  group_by(game_id) |>
  filter(all(meets)==TRUE) -> good_green
blue_games |> pivot_longer(cols=starts_with("V")) |>
  filter(value != "") |>
  mutate(meets = grepl(blue_filter, value)) |>
  group_by(game_id) |>
  filter(all(meets)==TRUE) -> good_blue

good_red$game_id |> unique()
good_green$game_id |> unique()
good_blue$game_id |> unique()

valid_table <- cbind(1:100,
      1:100 %in% good_red$game_id,
      1:100 %in% good_green$game_id,
      1:100 %in% good_blue$game_id) |> as.data.frame()
valid_table$sum = valid_table$V2 + valid_table$V3 + valid_table$V4
valid_table[valid_table$sum == 3,]

# part 2 The power of a set of cubes is equal to the numbers of red, green, and
# blue cubes multiplied together. The power of the minimum set of cubes in game
# 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up
# these five powers produces the sum 2286.

# For each game, find the minimum set of cubes that must have been present. What
# is the sum of the power of these sets?


red_games |> mutate(across(starts_with("V"), ~ gsub(" |[a-z]+", "", .x))) |>
  pivot_longer(cols=starts_with("V")) |>
  mutate(value = as.integer(value)) |>
  summarise(high_value = max(value, na.rm=TRUE), .by = game_id) -> max_reds
green_games |> mutate(across(starts_with("V"), ~ gsub(" |[a-z]+", "", .x))) |>
  pivot_longer(cols=starts_with("V")) |>
  mutate(value = as.integer(value)) |>
  summarise(high_value = max(value, na.rm=TRUE), .by = game_id) -> max_greens
blue_games |> mutate(across(starts_with("V"), ~ gsub(" |[a-z]+", "", .x))) |>
  pivot_longer(cols=starts_with("V")) |>
  mutate(value = as.integer(value)) |>
  summarise(high_value = max(value, na.rm=TRUE), .by = game_id) -> max_blues

cbind(max_reds$high_value, max_greens$high_value, max_blues$high_value) |>
  as_data_frame() |>
  mutate(power = V1*V2*V3) |>
  summarise(total_power = sum(power))

