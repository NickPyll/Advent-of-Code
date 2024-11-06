# Day 3 Binary Diagnostic ----

library(tidyverse)

# ## Part 1 ----
# # Find the gamma and epsilon rates
# 
# # binary <- readLines('2021 Challenge/data/input_day3_test.txt') 
# binary <- readLines('2021 Challenge/data/input_day3.txt')
# 
# gamma <- ''
# epsilon <- ''
# 
# for (i in 1:nchar(head(binary, 1))){
#   
#   k <- 0
#   
#   for (j in 1:length(binary)){
#     
#     k <- k + as.numeric(substr(binary[j], i, i))
#     
#   }
#   
#   gamma <- paste0(gamma, as.character(round(k / length(binary))))
#   epsilon <- paste0(epsilon, as.character(round(1 - (k / length(binary)))))
#   
# }
# 
# answer <- strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)

## Part 2 ----
# wtf

# binary <- readLines('2021 Challenge/data/input_day3_test.txt')
binary <- readLines('2021 Challenge/data/input_day3.txt')

zero_list <- c()
one_list <- c()

d <- 1

for (i in 1:length(binary)){

  if(substr(binary[i], d, d) == 0){
    
    zero_list <- c(zero_list, binary[i])
    
  } else {
    
    one_list <- c(one_list, binary[i])
    
  }

}
  
if (length(zero_list) > length(one_list)){
  oxy_list <- zero_list
  co2_list <- one_list
} else {
  oxy_list <- one_list 
  co2_list <- zero_list
}

oxy <- length(oxy_list)
d <- 1

while (oxy > 1){
  
  d <- d + 1
  k <- 0

  for (j in 1:length(oxy_list)){
    k <- k + as.numeric(substr(oxy_list[j], d, d))
  }

  x.oxy_list <- c()

  k <- round((k / length(oxy_list)) + .001)
  
  for (j in 1:length(oxy_list)){
    if (substr(oxy_list[j], d, d) == k){
      x.oxy_list <- c(x.oxy_list, oxy_list[j])
    } 
  }

  oxy_list <- x.oxy_list
  oxy <- length(oxy_list)

}

rm(binary, i, j, k, oxy, one_list, zero_list, x.oxy_list)

co2 <- length(co2_list)
d <- 1

while (co2 > 1){
  
  d <- d + 1
  k <- 0
  
  for (j in 1:length(co2_list)){
    k <- k + as.numeric(substr(co2_list[j], d, d))
  }
  
  x.co2_list <- c()
  
  k <- round((k / length(co2_list)) + .001)
  
  for (j in 1:length(co2_list)){
    if (substr(co2_list[j], d, d) != k){
      x.co2_list <- c(x.co2_list, co2_list[j])
    } 
  }
  
  co2_list <- x.co2_list
  co2 <- length(co2_list)
  
}

answer <- strtoi(oxy_list, base = 2) * strtoi(co2_list, base = 2)
