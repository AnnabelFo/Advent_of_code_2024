#Libraries
library(tidyverse)

#Jour 1
#Etoile 1 ####

#Import données
setwd("~/git_travail/adv_of_code_2024/Advent_of_code_2024")
input <- read.csv("data/input_J1.csv", sep = ',')

#Ordonner les listes
liste1o<-input[1] %>% 
  arrange(liste1)
liste2o<-input[2] %>% 
  arrange(liste2)

#fusion et ajout colonne difference
data_sortie <- cbind(liste1o,liste2o)%>% 
mutate(difference = abs(liste1-liste2)) %>% 
  summarise(somme = sum(difference))

print(data_sortie)

#Etoile 2 ####
liste1oc <- input[1] %>% 
  group_by(liste1) %>% 
  summarise(nb_occurences1 = n())

liste2oc <- input[2] %>% 
  group_by(liste2) %>% 
  summarise(nb_occurences2 = n())

data_sortie <- inner_join(liste1oc, liste2oc, by = c("liste1" = "liste2")) %>% 
  mutate(score_sim = liste1 * nb_occurences2) %>% 
  summarise(tot = sum(score_sim))

print(data_sortie)
