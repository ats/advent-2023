# So, 33332 and 2AAAA are both four of a kind hands, but 33332 is stronger
# because its first card is stronger. Similarly, 77888 and 77788 are both a full
# house, but 77888 is stronger because its third card is stronger (and both
# hands have the same first and second card).
#
# To play Camel Cards, you are given a list of hands and their corresponding bid
# (your puzzle input). For example:
  
# 32T3K 765
# T55J5 684
# KK677 28
# KTJJT 220
# QQQJA 483

# This example shows five hands; each hand is followed by its bid amount. Each
# hand wins an amount equal to its bid multiplied by its rank, where the weakest
# hand gets rank 1, the second-weakest hand gets rank 2, and so on up to the
# strongest hand. Because there are five hands in this example, the strongest
# hand will have rank 5 and its bid will be multiplied by 5.
#
# So, the first step is to put the hands in order of strength:
#
# 32T3K is the only one pair and the other hands are all a stronger type, so it
# gets rank 1. KK677 and KTJJT are both two pair. Their first cards both have
# the same label, but the second card of KK677 is stronger (K vs T), so KTJJT
# gets rank 2 and KK677 gets rank 3. T55J5 and QQQJA are both three of a kind.
# QQQJA has a stronger first card, so it gets rank 5 and T55J5 gets rank 4.

library(tidyr)
library(dplyr)
library(stringr)

test_data <- tibble(
  c("32T3K",  "T55J5", "KK677" , "KTJJT" , "QQQJA", "QQQQ2", "33333", "23456"),
  c(765, 684, 28, 220, 483, 2, 2, 2)
) 
names(test_data) <- c("hand", "bid")

# Every hand is exactly one type. From strongest to weakest, they are:
#

# Every hand is exactly one type. From strongest to weakest, they are:
#  - Five of a kind, where all five cards have the same label: AAAAA
#  - Four of a kind, where four cards have the same label and one card has a different label: AA8AA
#  - Full house, where three cards have the same label, and the remaining two cards share a different
#     label: 23332
#  - Three of a kind, where three cards have the same label, and the remaining two cards are each i
#     different from any other card in the hand: TTT98
#  - Two pair, where two cards share one label, two other cards share a second label, and the 
#     remaining card has a third label: 23432
#  - One pair, where two cards share one label, and the other three cards have a different label 
#     from the pair and each other: A23A4
#  - High card, where all cards' labels are distinct: 23456
# 
#
# If two hands have the same type, a second ordering rule takes effect. Start by
# comparing the first card in each hand. If these cards are different, the hand
# with the stronger first card is considered stronger. If the first card in each
# hand have the same label, however, then move on to considering the second card
# in each hand. If they differ, the hand with the higher second card wins;
# otherwise, continue with the third card in each hand, then the fourth, then
# the fifth.

sort_order <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")

atomic_hands <- str_extract_all(test_data$hand, "[2-9AKQJT*]", simplify = TRUE) |> 
  as.data.frame() |> 
  mutate(hand = row_number()) |> pivot_longer(cols=V1:V5, names_to = "card") |>
  group_by(hand) |> count(value)

atomic_hands |> mutate(type = case_when(
  5 %in% n ~ "Five of a kind",
  (4 %in% n) ~ "Four of a kind",
  (2 %in% n & 3 %in% n) ~ "Full house",
  3 %in% n & 1 %in% n ~ "Three of a kind",
  n == 2 ~ "Pair"
)) |> 
  mutate(type = case_when(
    "Three of a kind" %in% type & "Pair" %in% type ~ "Full house",
    "Three of a kind" %in% type ~ "Three of a kind",
    type == "Pair" ~ "Pair",
    .default = type
  )) |>
  group_by(hand) |> count(type) |>
  mutate(type = case_when(
    type == "Pair" & n == 2 ~ "Two pair",
    # is.na(type) ~ "High card",
    .default = type
  )) |> filter(!is.na(type)) -> ranked_hands


test_data <- test_data |> mutate(id = row_number()) |> left_join(ranked_hands, join_by(id==hand))
test_data

hand_order <- c("Five of a kind", "Four of a kind", "Full house", "Three of a kind", 
                "Two pair", "Pair")



