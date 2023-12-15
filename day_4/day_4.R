library(tidyverse)

#---------------PART 1----------------

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

#---------------PART 2----------------

calc_card_wins <- function(card) {

  number_of_winners <- card$your_numbers %in% card$winners |> sum()

  return(number_of_winners)

}

expand_cards <- function(current_card_number, current_card_table) {

  if(current_card_number < number_of_unique_cards) {

    current_number_of_wins <-
      current_card_table$number_of_wins[current_card_number]

    current_number_of_cards <-
      current_card_table$number_of_cards[current_card_number]

    next_card_table <-
      current_card_table |>
      mutate(number_of_cards = case_when(
        card_number <= current_card_number ~ number_of_cards,
        card_number <= current_card_number + current_number_of_wins ~ number_of_cards + current_number_of_cards,
        card_number > current_card_number + current_number_of_wins ~ number_of_cards,
        .default = as.numeric(NA)
      ))

    expand_cards(current_card_number + 1, next_card_table)

  } else {

    return(current_card_table)

  }

}

expand_all_cards <- function(card_table) expand_cards(1, card_table)


number_of_unique_cards <- length(scratch_cards)

number_of_wins <-
  scratch_cards |>
  map_vec(calc_card_wins)

card_table <-
  tibble(card_number = 1:number_of_unique_cards,
         number_of_wins = number_of_wins,
         number_of_cards = rep(1,number_of_unique_cards))

card_table |> expand_all_cards() |> summarise(sum(number_of_cards))

