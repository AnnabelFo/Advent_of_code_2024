##Jour 2

#Libraries
library(tidyverse)

#Etoile 1####

#Import données
setwd("~/Travaux_R/")
input <- read.csv2("Advent_of_code_2024_data/input_J2.csv")
input <- read.csv2("Advent_of_code_2024_data/test_j2.csv", header = FALSE)


  
#Déterminer si une ligne est safe
nb_safe <-0

#boucle pour chaque ligne
for (i in 1:nrow(input)) {
  #transformer la ligne en vecteur sans les NA
  ligne_i <- na.omit(unlist(input[i,]))
  ligne_i
  #Tester croissant/décroissant
  test_ordre <- all(ligne_i == sort(ligne_i, decreasing = FALSE))|
                 all(ligne_i == sort(ligne_i, decreasing = TRUE))
  
  test_ordre
  #tester les écarts sup à 0 et inf à 3
  test_ecarts <- (all(abs(diff(ligne_i)) > 0) &  all(abs(diff(ligne_i))<= 3))
 
  test_ecarts
  # Vérifier si la ligne est "safe" (test_ordre ET test_ecarts sont TRUE)
  ligne_safe <- test_ordre & test_ecarts
  ligne_safe

  
  #Si les tests sont TRUE on ajoute 1 aux nombres de lignes safe
  if (ligne_safe) {
    nb_safe <- nb_safe +1
  }
  
  print(paste("ligne",i,ligne_safe, nb_safe))
  }


