library(tidyverse)

calories <- readLines("day1_calories.txt") |> as_tibble()

elves_status <- calories |>
  mutate(keepchange = case_when(
    value != "" ~ "keep",
    value == "" ~ "change")) 

counter <- 1
elf_vec <- vector(mode = "numeric", length = nrow(elves_status))

for(i in 1:nrow(elves_status)){
  if(elves_status["keepchange"][[1]][i] == "keep"){
    elf_vec[i] <- counter
  } else{
    elf_vec[i] <- 0
    counter <- counter + 1
  }
  elf_vec
}

top3_elves <- elves_status |>
  cbind(elf_vec) |>
  as_tibble() |>
  filter(elf_vec != 0) |>
  mutate(value = as.double(value)) |>
  group_by(elf_vec) |>
  summarise(sum_calories = sum(value)) |> 
  arrange(desc(sum_calories)) |>
  head(n = 3) 

sum(top3_elves["sum_calories"])