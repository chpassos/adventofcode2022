library(tidyverse)

rps <- readLines("day2_game.txt")
rps_tbl <- rps |> 
  as_tibble() |>
  separate(value, into = c("opponent", "answer"), sep = " ")

rps_tbl |>
  mutate(result = case_when(
    (opponent == "A" & answer == "X") ~ "draw",
    (opponent == "A" & answer == "Y") ~ "win",
    (opponent == "A" & answer == "Z") ~ "lose",
    (opponent == "B" & answer == "Y") ~ "draw",
    (opponent == "B" & answer == "Z") ~ "win",
    (opponent == "B" & answer == "X") ~ "lose",
    (opponent == "C" & answer == "Z") ~ "draw",
    (opponent == "C" & answer == "X") ~ "win",
    (opponent == "C" & answer == "Y") ~ "lose")) |>
  mutate(points_shape = case_when(
    answer == "X" ~ 1,
    answer == "Y" ~ 2,
    answer == "Z" ~ 3)) |>
  mutate(points_result = case_when(
    result == "lose" ~ 0,
    result == "draw" ~ 3,
    result == "win" ~ 6)) |>
  mutate(sum_points = points_shape + points_result) |>
  summarise(final_sum = sum(sum_points))
  
rps_tbl |>
  mutate(elf_answer = case_when(
    answer == "X" ~ "lose",
    answer == "Y" ~ "draw",
    answer == "Z" ~ "win")) |>
  mutate(my_move = case_when(
    (opponent == "C" & elf_answer == "win") ~ "rock",
    (opponent == "C" & elf_answer == "draw") ~ "scissor",
    (opponent == "C" & elf_answer == "lose") ~ "paper",
    (opponent == "A" & elf_answer == "win") ~ "paper",
    (opponent == "A" & elf_answer == "draw") ~ "rock",
    (opponent == "A" & elf_answer == "lose") ~ "scissor",
    (opponent == "B" & elf_answer == "win") ~ "scissor",
    (opponent == "B" & elf_answer == "draw") ~ "paper",
    (opponent == "B" & elf_answer == "lose") ~ "rock")) |>
  mutate(points_result = case_when(
    elf_answer == "win" ~ 6,
    elf_answer == "draw" ~ 3,
    elf_answer == "lose" ~ 0)) |>
  mutate(points_shape = case_when(
    my_move == "rock" ~ 1,
    my_move == "paper" ~ 2,
    my_move == "scissor" ~ 3)) |>
  mutate(sum_points = points_shape + points_result) |>
  summarise(all_sum = sum(sum_points))