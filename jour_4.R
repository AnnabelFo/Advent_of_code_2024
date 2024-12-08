##Jour 4
#Libraries
library(tidyverse)

#setwd("~/Travaux_R/")
setwd("~/git_travail/adv_of_code_2024")

#input <- read.csv("Advent_of_code_2024_data/test_j3.txt", header = FALSE, sep = " ")
input <- read.csv2("Advent_of_code_2024_data/input_j4_test.csv", header = FALSE)
colnames(input)[1] <- "grille_lettres"


#Etoile 1 ####

#Compter le nombre d'eléments horizontaux dans le sens de lecture
#extraire le motif
grille <- input %>% 
  str_extract_all("XMAS") %>% 
  unlist()
#Nombre d'éléments extraits
nb_XMAS_hori <- length(grille)

#Compter le nombre d'eléments horizontaux dans le sens contraire de lecture
#extraire le motif
grille <- input %>% 
  str_extract_all("SAMX") %>% 
  unlist()
#Nombre d'éléments extraits
nb_SAMX_hori <- length(grille)

#Compter le nombre d'eléments verticaux dans le sens de lecture

#Inverser le tableau t()
# Convertir chaque chaîne de caractères en une liste de caractères
grille_split <- input %>%
  mutate(grille_lettres = strsplit(as.character(grille_lettres), ""))

# Transformer en matrice (10 lignes x 10 colonnes)
grille_matrix <- do.call(rbind, grille_split$grille_lettres)
# Transposer la matrice pour obtenir le format souhaité
grille_transposed <- t(grille_matrix)  
# Re-fusionner les lettres pour chaque colonne
grille_fusionnee <- apply(grille_transposed, 1, function(x) paste(x, collapse = ""))
grille_fusionnee

#Les motifs verticaux XMAS
#extraire le motif
grille_t_modifiee <- grille_fusionnee%>% 
  str_extract_all("XMAS") %>% 
  unlist()
#Nombre d'éléments extraits
nb_XMAS_verti <- length(grille_t_modifiee)

#Les motifs verticaux SAMX
#extraire le motif
grille_t_modifiee <- grille_fusionnee %>% 
  str_extract_all("SAMX") %>% 
  unlist()
#Nombre d'éléments extraits
nb_SAMX_verti <- length(grille_t_modifiee)

#Diagonales ####
grille_matrix

get_all_diagonals <- function(mat) {
  
 #mat <-grille_matrix
    # Obtenir les dimensions de la matrice
  nb_lignes <- nrow(mat)
  nb_col <- ncol(mat)
  
  # Matrices des indices
  lignes <- row(mat)
  colonnes <- col(mat)
  
  # Diagonales principales (lignes - colonnes)
  # offsets_principales <- unique(lignes - colonnes)
  
 # Créer un vecteur qui va de -9 à 9
  offsets_principales <- seq(-(nb_lignes-1), (nb_lignes-1), by = 1)
  diagonales_principales <- lapply(offsets_principales, function(offset) {
    diag_indices <- which(lignes - colonnes == offset)
    mat[diag_indices]
  })

  # Diagonales secondaires (lignes + colonnes)
  offsets_secondaires <- seq(-(nb_lignes-1), (nb_lignes-1), by = 1)
  diagonales_secondaires <- lapply(offsets_secondaires, function(offset) {
    diag_indices <- which(lignes + colonnes == offset)
    mat[diag_indices]
  })
  
  #Combiner toutes les diagonales
  toutes_diagonales <- c(diagonales_principales, diagonales_secondaires)
  return(toutes_diagonales)
  }

# Extraire toutes les diagonales
diagonales <- get_all_diagonals(grille_matrix)

# Afficher les diagonales
#diagonales <- diagonales_secondaires

# Re-fusionner les lettres pour chaque ligne
diagonales_fusion <- sapply(diagonales, function(x) paste(x, collapse = ""))
diagonales_fusion

#### Motifs des diagonales ####
#extraire le motif
grille <- diagonales_fusion %>% 
  str_extract_all("XMAS") %>% 
  unlist()
#Nombre d'éléments extraits
nb_XMAS_diag <- length(grille)
nb_XMAS_diag

#extraire le motif
grille <- diagonales_fusion %>% 
  str_extract_all("SAMX") %>% 
  unlist()
#Nombre d'éléments extraits
nb_SAMX_diag <- length(grille)


nb_XMAS_tot <- nb_XMAS_diag +  nb_SAMX_diag + nb_XMAS_hori + nb_XMAS_verti + nb_SAMX_hori + nb_SAMX_verti
print(nb_XMAS_tot)
