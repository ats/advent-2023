# For example:
#
#   1abc2
# pqr3stu8vwx
# a1b2c3d4e5f
# treb7uchet
# In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
#
# Consider your entire calibration document. What is the sum of all of the calibration values?
#

data <- read.csv("day_01/input.txt", header = FALSE)
stringr::str_replace_all(data$V1, "[a-z]", "") -> nums
nums <- as.data.frame(nums)

library(dplyr)
nums
nums |> rowwise() |>
  mutate(A = stringr::str_sub(nums, 1, 1),
         B = stringr::str_sub(nums, -1L, -1L),
         sum = paste0(A, B)) |>
  ungroup() |>
  summarise(total = sum(as.numeric(sum)))

# Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
#
# Equipped with this new information, you now need to find the real first and last digit on each line. For example:
#
# two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen
# In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

# -----
# a totally different approach
d <- as.data.frame(data)
a <- gsub("one", "o1e", gsub("two", "t2o", gsub("three", "th3e", gsub("four", "4",
       gsub("five", "fi5e", gsub("six", "s6x", gsub("seven", "se7en", gsub("eight", "ei8gt",
       gsub("nine", "n9ne", d$V1)))))))))
a <- gsub("[a-z]", "", a)
a <- as.data.frame(a)

a$col1 <- substr(a$a, 1, 1)
a$col2 <- substr(a$a, nchar(a$a), nchar(a$a))
a$numbers <- paste0(a$col1, a$col2)
sum(as.numeric(a$numbers))




