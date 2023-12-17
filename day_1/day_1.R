library(tidyverse)
library(glue)

#---------------PART 1----------------

readLines("day_1/day_1_inputs.txt") |>
  str_extract_all("[1-9]") |>
  map_vec(function(line) glue(first(line), last(line)) |> parse_integer()) |>
  sum()

#---------------PART 2----------------

reverse_string <- function(strings) {
  sapply(strings, function(s) {
    str_c(rev(str_split(s, pattern = "")[[1]]), collapse = "")
  })
}

numbers_as_words <-
  c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

one_digit_integer_pattern <-
  c(numbers_as_words, "[1-9]") |>
  paste(collapse = "|")

one_digit_integer_pattern_rev <-
  c(reverse_string(numbers_as_words), "[1-9]") |>
  paste(collapse = "|")

extract_first_and_last <- function(line) {
  # extract first
  first <-
    line |>
    str_extract(one_digit_integer_pattern)

  # extract last
  last <-
    line |>
    reverse_string() |>
    str_extract(one_digit_integer_pattern_rev) |>
    reverse_string()

  c(first, last)
}

convert_words_to_number <- function(numbers) {
  if_else(numbers %in% numbers_as_words,
    match(numbers, numbers_as_words) |> as.character(),
    numbers
  )
}

concat_digits_as_number <- function(numbers) {
  glue(first(numbers), last(numbers)) |> parse_integer()
}

readLines("day_1/day_1_inputs.txt") |>
  map(extract_first_and_last) |>
  map(convert_words_to_number) |>
  map_vec(concat_digits_as_number) |>
  sum()
