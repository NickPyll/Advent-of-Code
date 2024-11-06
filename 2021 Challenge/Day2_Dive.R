# Day 2 Dive ----

library(tidyverse)

# ## Part 1 ----
# # Find horizontal and vertical position of submarine
# 
# # instructions <- read.table('2021 Challenge/data/input_day2_test.txt') 
# instructions <- read.table('2021 Challenge/data/input_day2.txt')
# 
# direction <- instructions |> select(V1) |> pull()
# value <- instructions |> select(V2) |> pull()
# 
# # initialize
# horizontal_position <- 0
# depth <- 0
# 
# for (i in 1:length(direction)){
#   if (direction[i] == 'forward'){
#     horizontal_position <- horizontal_position + value[i]
#   } else if (direction[i] == 'down'){
#     depth <- depth + value[i]
#   } else {depth <- depth - value[i]}
# }
# 
# print(horizontal_position*depth)

## Part 2 ----
# aim?

# instructions <- read.table('2021 Challenge/data/input_day2_test.txt')
instructions <- read.table('2021 Challenge/data/input_day2.txt')

direction <- instructions |> select(V1) |> pull()
value <- instructions |> select(V2) |> pull()

# initialize
horizontal_position <- 0
depth <- 0
aim <- 0

for (i in 1:length(direction)){
  if (direction[i] == 'forward'){
    horizontal_position <- horizontal_position + value[i]
    depth <- depth + value[i]*aim
  } else if (direction[i] == 'down'){
    aim <- aim + value[i] 
  } else {aim <- aim - value[i]}
  print(paste('h =', horizontal_position, 'd =', depth, 'a =', aim))
  
}

print(horizontal_position*depth)

