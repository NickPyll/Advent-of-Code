# --- Day 7: Bridge Repair ---

library(tidyverse)

file <- "2024 Challenge/data/input_day7_test.txt"
file <- "2024 Challenge/data/input_day7.txt"

generate_expressions <- function(input_string) {
  digits <- strsplit(input_string, " ")[[1]]
  n_operators <- length(digits) - 1
  operators <- expand.grid(rep(list(c("+", "*")), n_operators))
  expressions <- apply(operators, 1, function(op) {
    paste0(paste0(digits[-length(digits)], op, collapse = ""), digits[length(digits)])
  })
  return(expressions)
}

evaluate_left_to_right <- function(expression) {
  tokens <- unlist(strsplit(expression, "(?<=\\d)(?=\\D)|(?<=\\D)(?=\\d)", perl = TRUE))
  result <- as.numeric(tokens[1])

  for (i in seq(2, length(tokens), by = 2)) {
    operator <- tokens[i]
    operand <- as.numeric(tokens[i + 1])
    if (operator == "+") {
      result <- result + operand
    } else if (operator == "*") {
      result <- result * operand
    }
  }
  return(result)
}

equations <-
  readLines(file) |>
  as.data.frame() |>
  rename(equation = 1) |>
  separate(equation, c("output", "input"), sep = ": ")

answer <- 0

for (i in 1:nrow(equations)) {
  results <- sapply(generate_expressions(equations$input[[i]]), evaluate_left_to_right)

  results_bool <- results == as.numeric(equations$output[[i]])

  if (sum(results_bool) > 0) {
    answer <- answer + as.numeric(equations$output[[i]])
  }
}

generate_expressions_mod <- function(input_string) {
  digits <- strsplit(input_string, " ")[[1]]
  n_operators <- length(digits) - 1
  operators <- expand.grid(rep(list(c("+", "*", "g")), n_operators))
  expressions <- apply(operators, 1, function(op) {
    paste0(paste0(digits[-length(digits)], op, collapse = ""), digits[length(digits)])
  })
  return(expressions)
}

evaluate_left_to_right_mod <- function(expression) {
  tokens <- unlist(strsplit(expression, "(?<=\\d)(?=\\D)|(?<=\\D)(?=\\d)", perl = TRUE))
  result <- as.numeric(tokens[1])

  for (i in seq(2, length(tokens), by = 2)) {
    operator <- tokens[i]
    operand <- tokens[i + 1]

    if (operator == "+") {
      result <- as.numeric(result) + as.numeric(operand)
    } else if (operator == "*") {
      result <- as.numeric(result) * as.numeric(operand)
    } else if (operator == "g") {
      result <- paste0(result, operand)
      result <- as.numeric(result)
    }
  }
  return(result)
}

answer <- 0

for (i in 1:nrow(equations)) {
  results <- sapply(generate_expressions_mod(equations$input[[i]]), evaluate_left_to_right_mod)

  results_bool <- results == as.numeric(equations$output[[i]])

  if (sum(results_bool) > 0) {
    answer <- answer + as.numeric(equations$output[[i]])
  }
  print(i)
  print(answer)
}

evaluate_left_to_right_mod("9+5*7*51g6+2+4+8*4g4g7g965")
