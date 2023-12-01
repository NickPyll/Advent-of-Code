# Day 1 Trebuchet?!----
# Strip first and last integer
# Combine to make two-digit integer
# Add all values

library(tidyverse)

input <- read.table('data/input_day1.txt')

test <-
  tribble(
    ~V1,
    'ohohohohoh9hshshshshs6',
    'threethree4567',
    '4sixteen9nnn',
    'twone6'
  )


trebuchet <- function(df){
  
  df |> 
    mutate(V1 = gsub("[^0-9]", "", V1),
           num2 = as.numeric(paste0(str_sub(V1, 1, 1),
                                    str_sub(V1, -1, -1)))) |> 
    summarise(sum = sum(num2)) |> 
    pull()

}

trebuchet(input)
trebuchet(test)

trebuchet_modified <- function(df){
  
  df |> 
    mutate(V1 = gsub('zero', '0o', V1),
           V1 = gsub('one', 'o1e', V1),
           V1 = gsub('two', 't2o', V1),
           V1 = gsub('three', 't3e', V1),
           V1 = gsub('four', '4', V1),
           V1 = gsub('five', '5e', V1),
           V1 = gsub('six', 6, V1),
           V1 = gsub('seven', '7n', V1),
           V1 = gsub('eight', 'e8t', V1),
           V1 = gsub('nine', 'n9e', V1),
           V1 = gsub("[^0-9]", "", V1),
           num2 = as.numeric(paste0(str_sub(V1, 1, 1),
                                    str_sub(V1, -1, -1)))) |> 
    summarise(sum = sum(num2)) |> 
    pull()
  
}

trebuchet_modified(input)
trebuchet_modified(test)
