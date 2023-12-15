library(tidyverse)

clean_scratch_card <- function(card) {

  clean_card <- list()
  clean_card$name <- card[1]
  clean_card$winners <- card[2] |> str_split_1(" ") |> keep(function(n) n != "") |> as.numeric()
  clean_card$your_numbers <- card[3] |> str_split_1(" ") |> keep(function(n) n != "") |> as.numeric()

  return(clean_card)
}

calc_card_points <- function(card) {

  number_of_winners <- card$your_numbers %in% card$winners |> sum()
  points <- floor(2 ^ (number_of_winners - 1))

  return(points)

}

scratch_cards <-
  readLines("day_4/day_4_inputs.txt") |>
  str_split(pattern = ":|\\|") |>
  map(clean_scratch_card)

total_points <-
  scratch_cards |>
  map_vec(calc_card_points) |> sum()

total_points

