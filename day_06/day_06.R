# Time:      7  15   30
# Distance:  9  40  200

library(tidyr)
library(dplyr)

# test data
races <- tibble(c(7, 15, 30), c(9, 40, 200))

# input data
# Time:        53     91     67     68
# Distance:   250   1330   1081   1025
# races <- tibble(c(53, 91, 67, 68), c(250, 1330, 1081, 1025))

names(races) <- c("race_time", "record_distance")

go_the_distance <- function(max_time=0, goal_distance=0) {
  output <- tibble()
  for (hold in 0:max_time) {
    travel_time <- max_time - hold
    travel_distance <- hold * travel_time
    race <- (cbind(hold, travel_time, travel_distance, goal_distance, 
                   as.logical(travel_distance > goal_distance)))
    output <- rbind(output, race)
  }
  names(output)[5] <- "win"
  as_tibble(output)
}

# A <- go_the_distance(7, 9) |> summarise(wins = sum(V5))
# B <- go_the_distance(15, 40) |> summarise(wins = sum(V5))
# C <- go_the_distance(30, 200) |> summarise(wins = sum(V5))
# A * B * C

A <- go_the_distance(53, 250) |> summarise(wins = sum(V5))
B <- go_the_distance(91, 1330) |> summarise(wins = sum(V5))
C <- go_the_distance(67, 1081) |> summarise(wins = sum(V5))
D <- go_the_distance(68, 1025) |> summarise(wins = sum(V5))
A * B * C * D

# part 2

# go_the_distance(53916778, 250133010811025)
# no, of course that will take forever

go_the_distance2 <- function(max_time=0, goal_distance=0) {
  output <- tibble()
  for (hold in 0:max_time) {
    travel_time <- max_time - hold
    travel_distance <- hold * travel_time
    # race <- (cbind(hold, travel_time, travel_distance, goal_distance, 
    #                as.logical(travel_distance > goal_distance)))
    # output <- rbind(output, race)
    if (travel_distance > goal_distance) {
      breakpoint <- hold
      return(breakpoint)
    }
  }
  
  # names(output)[5] <- "win"
  # as_tibble(output)
}
go_the_distance2(91, 1330)

go_the_distance2(71530, 940200)


# countdown distance
go_the_distance3 <- function(max_time=0, goal_distance=0) {
  output <- tibble()
  for (hold in max_time:0) {
    travel_time <- max_time - hold
    travel_distance <- hold * travel_time
    # race <- (cbind(hold, travel_time, travel_distance, goal_distance, 
    #                as.logical(travel_distance > goal_distance)))
    # output <- rbind(output, race)
    if (travel_distance > goal_distance) {
      breakpoint <- hold
      return(breakpoint)
    }
  }
  
  # names(output)[5] <- "win"
  # as_tibble(output)
}

A2 <- go_the_distance2(71530, 940200)
B2 <- go_the_distance3(71530, 940200)
B2 - A2 + 1

# Time:        53     91     67     68
# Distance:   250   1330   1081   1025

A2 <- go_the_distance2(53916768, 250133010811025) # try getting just the lower bound breakpoint
B2 <- go_the_distance3(53916768, 250133010811025) # try getting just the lower bound breakpoint
B2 - A2 + 1
