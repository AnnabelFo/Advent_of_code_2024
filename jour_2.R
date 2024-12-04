##Jour 2

#Libraries
library(tidyverse)

#Etoile 1####

#Import données
setwd("~/Travaux_R/")
input <- read.csv2("Advent_of_code_2024_data/input_J2.csv")
#input <- read.csv2("Advent_of_code_2024_data/test_j2.csv", header = FALSE)


  
#Déterminer si une ligne est safe
nb_safe <-0

#boucle pour chaque ligne
for (i in 1:nrow(input)) {
  #transformer la ligne en vecteur sans les NA
  ligne_i <- na.omit(unlist(input[1,]))
  
  #Tester croissant/décroissant
  test_ordre <- all(ligne_i == sort(ligne_i, decreasing = FALSE))|
                 all(ligne_i == sort(ligne_i, decreasing = TRUE))
  
  #tester les écarts sup à 0 et inf à 3
  test_ecarts <- (all(abs(diff(ligne_i)) > 0) &  all(abs(diff(ligne_i))<= 3))
 
  # Vérifier si la ligne est "safe" (test_ordre ET test_ecarts sont TRUE)
  ligne_safe <- test_ordre & test_ecarts
  
  #Si les tests sont TRUE on ajoute 1 aux nombres de lignes safe
  if (ligne_safe) {
    nb_safe <- nb_safe +1
  }
  
  print(paste("ligne",i,ligne_safe, nb_safe))
}


#Etoile 2 ####

#Déterminer si une ligne est safe
nb_safe <-0

#boucle pour chaque ligne
for (i in 1:nrow(input)) {
  
  ligne_safe <- FALSE
  #transformer la ligne en vecteur sans les NA
  ligne_i <- na.omit(unlist(input[i,]))
  ligne_i
  
  for (j in 1:length(ligne_i) ){
    
    #Ligne moins une valeur
    ligne_i_remove <- ligne_i[-j]
   
    #Tester croissant/décroissant
    test_ordre_remove <- all(ligne_i_remove == sort(ligne_i_remove, decreasing = FALSE))|
      all(ligne_i_remove == sort(ligne_i_remove, decreasing = TRUE))
  
    #tester les écarts sup à 0 et inf à 3
    test_ecarts_remove <- (all(abs(diff(ligne_i_remove)) > 0) &  all(abs(diff(ligne_i_remove))<= 3))
    
    # Vérifier si la ligne est "safe" (test_ordre ET test_ecarts sont TRUE)
    ligne_safe_remove <- test_ordre_remove & test_ecarts_remove
    
    #compiler les safe
    ligne_safe <- (ligne_safe | ligne_safe_remove)
     }
  
  #Si les tests sont TRUE on ajoute 1 aux nombres de lignes safe
  if (ligne_safe) {
    nb_safe <- nb_safe +1
  }
  
  print(paste("ligne",i,ligne_safe, nb_safe))
}



