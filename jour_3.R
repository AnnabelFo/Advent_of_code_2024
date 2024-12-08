##Jour 3

#Libraries
library(tidyverse)
library(readxl)

#setwd("~/Travaux_R/")
setwd("~/git_travail/adv_of_code_2024")

#input <- read.csv("Advent_of_code_2024_data/test_j3.txt", header = FALSE, sep = " ")
input <- read.csv("Advent_of_code_2024_data/input_j3.txt", header = FALSE, sep = "\n")

#Etoile 1 ####
#extraire le motif
data_temp <- input %>% 
  str_extract_all("mul\\(\\d+,\\d+\\)") %>% 
  unlist()
#extraire les nombres
nombres <- str_extract_all(data_temp,"\\d+") %>% 
  unlist()

# Restructurer les chiffres en paires
nombres_matrix <- matrix(nombres, ncol = 2, byrow = TRUE)

# Convertir en data frame
data_final <- as.data.frame(nombres_matrix) %>% 
  mutate(multiplication = as.numeric(V1)*as.numeric(V2)) %>% 
  summarise(sum(multiplication))

print(data_final)


#Etoile 2 ####
#extraire le motif
data_motif <- input %>% 
  str_extract_all("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)") %>% 
  unlist() %>% 
  data.frame() 

colnames(data_motif)[1] <- "value"


# Identifier les indices de "do()" et "don't()"
data_temp <- data_motif %>%
  mutate(
    group = case_when(
      value == "do()" ~ "start",
      value == "don't()" ~ "end",
      TRUE ~ NA_character_
    )
  ) %>%
  #Propager les valeurs de "start" et "end" pour identifier les groupes
  fill(group, .direction = "downup") %>%
  mutate(group = ifelse(is.na(group), "other", group)) %>% 
  #filtrer sur les starts
  filter(group == "start" & value != "do()" )

#extraire les nombres
nombres <- str_extract_all(data_temp,"\\d+") %>% 
  unlist()

# Restructurer les chiffres en paires
nombres_matrix <- matrix(nombres, ncol = 2, byrow = TRUE)

# Convertir en data frame
data_final <- as.data.frame(nombres_matrix) %>% 
  mutate(multiplication = as.numeric(V1)*as.numeric(V2)) %>% 
  summarise(sum(multiplication))

print(data_final)




