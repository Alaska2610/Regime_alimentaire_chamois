library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readxl)
#########################################################
### Description des espèces végétales échantillonnées ###
#########################################################
setwd("~/REGIME_ALIM_CHAM_GIT/Regime_alimentaire_chamois")

#importer le jeu de données trnl 
species_trnl <- read_excel("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100.xlsx", col_names = FALSE)
headerstr(species_trnl)
summary(species_trnl)

#J'extrait les deux premières lignes (occurences, vecteur de rangs taxonomiques) pour les isoler. 
# puis convertir en vecteur simple pour manipuler facilement
species_trnl_occ <- species_trnl[1, 20:ncol(species_trnl)]
ligne_occurence_vect <- unlist(species_trnl_occ)

rangs_trnl <- species_trnl[2, 20:ncol(species_trnl)]
rangs_trnl_vect <- unlist(rangs_trnl)

#Quelle précision pour mes données ? compter par rangs (species, family etc.)



#Proportion des niveaux de précisions par plante détectée 
