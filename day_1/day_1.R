library(tidyverse)
library(glue)

result <-
  readLines("day_1/day_1_inputs.txt") |>
  str_extract_all("[:digit:]")  |>
  map_vec(function(line) glue(first(line), last(line)) |> parse_integer()) |>
  sum()
