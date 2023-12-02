# Day 1 Trebuchet?!----
## Part 1 ----
# Strip first and last single digit number
# Combine to make two-digit number
# Add all values

library(tidyverse)

input <- read.table('data/input_day1.txt')

test_1 <-
  tribble(
    ~V1,
    '1abc2',
    'pqr3stu8vwx',
    'a1b2c3d4e5f',
    'treb7uchet'
  )


trebuchet <- function(df){
  
  df |> 
    mutate(V1 = gsub("[^0-9]", "", V1),
           num2 = as.numeric(paste0(str_sub(V1, 1, 1),
                                    str_sub(V1, -1, -1)))) |> 
    summarise(sum = sum(num2)) |> 
    pull()

}

trebuchet(test_1)
trebuchet(input)

## Part 2 ----
# Replace instances of text (zero:nine) with numbers
# Strip first and last single digit number
# Combine to make two-digit number
# Add all values

test_2 <-
  tribble(
    ~V1,
    'two1nine',
    'eightwothree',
    'abcone2threexyz',
    'xtwone3four',
    '4nineeightseven2',
    'zoneight234',
    '7pqrstsixteen'
  )


trebuchet_modified <- function(df){
  
  df |> 
    mutate(
      V1 = str_replace_all(
             string = V1, 
             pattern = c('zero' = '0o', 
                         'one' = 'o1e', 
                         'two' = 't2o',
                         'three' = 't3e',               
                         'four' = '4',
                         'five' = '5e',
                         'six' = '6',
                         'seven' = '7n',
                         'eight' = 'e8t',
                         'nine' = 'n9e')),
      V1 = gsub("[^0-9]", "", V1),
      num2 = as.numeric(paste0(str_sub(V1, 1, 1),
                               str_sub(V1, -1, -1)))) |> 
    summarise(sum = sum(num2)) |> 
    pull()
}

trebuchet_modified(test_2)
trebuchet_modified(input)
