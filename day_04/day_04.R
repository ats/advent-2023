# As far as the Elf has been able to figure out, you have to figure out which of
# the numbers you have appear in the list of winning numbers. The first match
# makes the card worth one point and each match after the first doubles the
# point value of that card.
#
# For example:
#
# Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53 Card 2: 13 32 20 16 61 | 61
# 30 68 82 17 32 24 19 Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1 Card 4:
# 41 92 73 84 69 | 59 84 76 51 58  5 54 83 Card 5: 87 83 26 28 32 | 88 30 70 12
# 93 22 82 36 Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

# In the above example, card 1 has five winning numbers (41, 48, 83, 86, and 17)
# and eight numbers you have (83, 86, 6, 31, 17, 9, 48, and 53). Of the numbers
# you have, four of them (48, 83, 17, and 86) are winning numbers! That means
# card 1 is worth 8 points (1 for the first match, then doubled three times for
# each of the three matches after the first).
#
# Card 2 has two winning numbers (32 and 61), so it is worth 2 points. Card 3
# has two winning numbers (1 and 21), so it is worth 2 points. Card 4 has one
# winning number (84), so it is worth 1 point. Card 5 has no winning numbers, so
# it is worth no points. Card 6 has no winning numbers, so it is worth no
# points. So, in this example, the Elf's pile of scratchcards is worth 13
# points.
#
# Take a seat in the large pile of colorful cards. How many points are they
# worth in total?

library(tibble)
library(dplyr)
library(tidyr)

data <- read.csv("day_04/input.txt", sep="", header = FALSE) |> as_tibble()
names(data) <- c("card", "card_num", glue::glue("win{c(1:10)}"),
                 "sep", glue::glue("my{c(1:25)}"))
data <- data |> mutate(card_num = as.integer(sub(":", "", card_num)))
data <- data |> pivot_longer(win1:win10, names_to = "win_num", values_to = "win_value") |>
  pivot_longer(my1:my25, names_to = "my_num", values_to = "my_value")

wins <- data |> filter(.by = card_num, win_value == my_value)

elf_doubler <- function(matches=NULL) {
  value <- 1
    if(matches > 1) {
    for(match in 2:matches){
      value <- value*2
    }
  }
  return(value)
}

wins <- wins |> count(card_num, name="matches") |> arrange(card_num) |>
  rowwise() |>
  mutate(score = elf_doubler(matches))

wins |>
  ungroup() |>
  summarise(total=sum(score))

# part 2
length(data$card_num |> unique()) # 206 cards
cards_list <- tibble(card = c(1:206))

deck <- cards_list |> left_join(wins, join_by(card == card_num))
deck$matches <- coalesce(deck$matches, 0)
deck$score <- coalesce(deck$score,0)
deck$counter <- 1

for (card in 1:nrow(deck)) {
  upper <- deck[card,]$matches # --> this is the value that I need to carry forward
  upper # upper is the number of additional cards whose counters should be incremented

  for (card_count in (card+1):(card+upper)) {
    if (upper > 0) {
      deck[card_count,]$counter <- deck[card_count,]$counter +
                                 deck[card,]$counter
    }
  }
}
sum(deck$counter)



