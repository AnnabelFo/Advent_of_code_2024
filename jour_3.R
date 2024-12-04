##Jour 3

#Libraries
library(tidyverse)
library(readxl)

setwd("~/Travaux_R/")
input <- read.csv("Advent_of_code_2024_data/test_j3.txt", header = FALSE, sep = " ")
input <- read.csv("Advent_of_code_2024_data/input_j3.txt", header = FALSE, sep = "")

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
data_temp <- input %>% 
  str_extract_all("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)") %>% 
  unlist()
  
data_temp

#initialisation
data_a_garder <- data.frame()
i <-1

while (i <= length(data_temp)) {
  
  #récuperer les premieres données avant le don't
    while (data_temp[i] != "don't()") {
      data_a_garder <- bind_rows(data_a_garder, data.frame(value = data_temp[i]))
      i <- i+1
    }
  #PAsser au suivant
  i <- i+1
  
  # Ne pas ajouter les éléments entre "don't()" et "do()"
  while (i <= length(data_temp) && data_temp[i] != "do()") {
      i <- i + 1
  }
  
  # Passer "do()" si on l'a trouvé
  if (i <= length(data_temp) && data_temp[i] == "do()") {
    i <- i + 1
  }
  # Ajouter les éléments après "do()"
  while (i <= length(data_temp) && data_temp[i] != "don't()") {
    data_a_garder <- bind_rows(data_a_garder, data.frame(value = data_temp[i]))
    i <- i + 1
  }
  
  # Passer "don't()" si on l'a trouvé pour la prochaine itération
  if (i <= length(data_temp) && data_temp[i] == "don't()") {
    i <- i + 1
  }
}
  data_a_garder



#extraire les nombres
nombres <- str_extract_all(data_a_garder,"\\d+") %>% 
  unlist()

# Restructurer les chiffres en paires
nombres_matrix <- matrix(nombres, ncol = 2, byrow = TRUE)

# Convertir en data frame
data_final <- as.data.frame(nombres_matrix) %>% 
  mutate(multiplication = as.numeric(V1)*as.numeric(V2)) %>% 
  summarise(sum(multiplication))

print(data_final)




