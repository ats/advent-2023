# The engine schematic (your puzzle input) consists of a visual representation
# of the engine. There are lots of numbers and symbols you don't really
# understand, but apparently any number adjacent to a symbol, even diagonally,
# is a "part number" and should be included in your sum. (Periods (.) do not
# count as a symbol.)
#
# Here is an example engine schematic:

# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..

# In this schematic, two numbers are not part numbers because they are not
# adjacent to a symbol: 114 (top right) and 58 (middle right). Every other
# number is adjacent to a symbol and so is a part number; their sum is 4361.
#
# Of course, the actual engine schematic is much larger. What is the sum of all
# of the part numbers in the engine schematic?

# part 1
# add up all numbers and then subtract the disconnected numbers?

library(stringr)
data <- read.csv(file="day_03/input.txt", sep="\t", header = FALSE)
all_numbers <- str_extract_all(data, "\\d+", simplify = TRUE) |> as.numeric()
grand_total <- sum(all_numbers)

# "invalid parts" are numbers surrounded by periods OR at the "edge" of the
# picture and otherwise surrounded

# look down one line at a time
# combine line A (top) with line B (bottom)
# if any number is not touching a symbol, remove it from the calculated total

# row <- 1
# nextrow <- row + 1
# col <- 1
#
# test_row <- data[row,]
# check_row <- data[nextrow,]
# test_column <- 1
#
# # change it up: process for each number found in row?
# check_symbols <- str_locate_all(check_row, "[^0-9.]") |> as.data.frame()
#
#
# check_symbols$len <- 1+(check_symbols$end - check_symbols$start)

parts <- tibble()
for (row in 1:nrow(data)) {
  #row <- 2 # test
  test_row <- data[row,]
  test_numbers <- str_locate_all(test_row, "\\d+") |> as.data.frame() # positions of numbers test row
  test_numbers$len <- 1+(test_numbers$end - test_numbers$start)
  num_nums <- nrow(test_numbers)

# make a lil box around each test_number, flatten it out, and see if it has any symbols?
  for(try in 1:num_nums) {
    row_parts <- paste0(substr(data[row-1,], test_numbers[try,]$start-1, test_numbers[try,]$end+1),
    substr(data[row,], test_numbers[try,]$start-1, test_numbers[try,]$end+1),
    substr(data[row+1,], test_numbers[try,]$start-1, test_numbers[try,]$end+1))

    parts <- rbind(parts, row_parts)
    }
}
names(parts) <- "part"
parts <- parts |> mutate(
  part_number = as.numeric(str_extract(part, "\\d+")),
  good_part = grepl("[^0-9a-zA-Z.]", part))
#parts |> summarise(total=sum(part_number)) # 608945, matches grand_total above
parts |> filter(good_part == TRUE) |> summarise(total=sum(part_number))
#parts |> filter(good_part == FALSE) |> summarise(total=sum(part_number))


# part 2: gear ratios
# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..

# In this schematic, there are two gears. The first is in the top left; it has
# part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the
# lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear
# because it is only adjacent to one part number.) Adding up all of the gear
# ratios produces 467835.
row <- 2
gears <- tibble()
for (row in 1:(nrow(data))) {
  test_row <- data[row,]
  # try <- 2
  test_gears <- str_locate_all(test_row, "\\*") |> as.data.frame() # positions of numbers test row
  test_gears$len <- 1+(test_gears$end - test_gears$start)
  num_gears <- nrow(test_gears)

  if (num_gears > 0) {
    for (try in 1:num_gears) {
      position <- test_gears[try,]$start
      gear <- cbind(paste0(substr(data[row-1,], test_gears[try,]$start-1, test_gears[try,]$end+1),
        substr(data[row,], test_gears[try,]$start-1, test_gears[try,]$end+1),
        substr(data[row+1,], test_gears[try,]$start-1, test_gears[try,]$end+1)),
        row, try, position)


    gears <- rbind(gears, gear)
    }
  }
}

gears <- gears |> mutate(gear = grepl("\\d+.*?\\*.*\\d+", V1))
# gears$gear == TRUE means a proper gear
# run through my matching again but with a bigger range to get the full number in the match?
true_gears <- gears[gears$gear==TRUE,]

full_gears <- tibble()
for (record in 1:(nrow(true_gears))) {
    focal_row <- as.numeric(true_gears[record,]$row)
    position <- as.numeric(true_gears[record,]$position)
    gear_row <- as.numeric(true_gears[record,]$row)

    above <- substr(data[focal_row-1,], position-3, position+3)
    at <- substr(data[focal_row,],   position-3, position+3)
    below <- substr(data[focal_row+1,], position-3, position+3)

    gear <- cbind(#paste(above, at, below, sep="|"),
                  gear_row, focal_row, above, at, below)
    full_gears <- rbind(full_gears, gear)
}

# for above and below connections, use the string closest to the center
gregexpr("\\d+", "34..692" )


full_gears |>
  mutate(A = case_when(grepl("\\d+\\*", at) ~ str_extract(at, "(\\d+)\\*", group=1),

                       #grepl("\\D*\\d+\\D*$", above) ~ str_extract(above, "\\D*(\\d+)\\D*$", group=1)
                       ),
         B = case_when(grepl("\\*\\d+", at) ~ str_extract(at, "\\*(\\d+)", group=1),
                       #grepl("^\\D*(\\d+)\\D*", below) ~ str_extract(below, "^\\D*(\\d+)\\D*", group=1)
         ))

# full_gears |>
#   mutate(A = substr(str_extract(V1, "(\\d*)\\D*\\*\\D*(\\d*)", group=1), 1, 3),
#          B = substr(str_extract(V1, "(\\d*)\\D*\\*\\D*(\\d*)", group=2), 1, 3))



