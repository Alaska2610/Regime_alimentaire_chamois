library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)

options(tibble.width = Inf)

#########################################################
### Description des espèces végétales échantillonnées ###
### dans le REGIME                                    ###
#########################################################

# Working directory de Louise
setwd("~/REGIME_ALIM_CHAM_GIT/Regime_alimentaire_chamois")
# Working directory de Marjo
setwd("/Volumes/SHARED/Git_Projects/Regime_alimentaire_chamois")

# Importer le jeu de données trnl 
species_trnl_lot1 <- read_excel("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_trnl_lot2 <- read_excel("BDD/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-trnL-Liste_DREAL/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-trnL-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
#species_trnl <- read_excel("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100.xlsx", col_names = FALSE)

# Import de la bdd des espèces animales correspondant aux crottes pour filtrer les chamois
load("Output/Rdata/species_an_occ1.Rdata")

# Nettoyage des noms de colonnes (suppr espaces, changements tirets etc)
colnames(species_trnl_lot1) <- as.character(species_trnl_lot1[3,])
species_trnl_lot1 <- species_trnl_lot1[-3,]

colnames(species_trnl_lot1) <- colnames(species_trnl_lot1) %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

colnames(species_trnl_lot2) <- as.character(species_trnl_lot2[3,])
species_trnl_lot2 <- species_trnl_lot2[-3,]

colnames(species_trnl_lot2) <- colnames(species_trnl_lot2) %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

# J'extrait les deux premières lignes (occurences, vecteur de rangs taxonomiques) pour les isoler. 
# puis convertir en vecteur simple pour manipuler facilement
occurence_trnl_vect_lot1 <- unlist(species_trnl_lot1[1, 20:137]) # 20:151
occurence_trnl_vect_lot1 <- factor(occurence_trnl_vect_lot1)

rangs_trnl_vect_lot1 <- as.vector(unlist(species_trnl_lot1[2, 20:137])) # 20:151
rangs_trnl_vect_lot1 <- factor(rangs_trnl_vect_lot1)
rangs_trnl_vect_lot1

occurence_trnl_vect_lot2 <- unlist(species_trnl_lot2[1, 21:144]) # 20:151
occurence_trnl_vect_lot2 <- factor(occurence_trnl_vect_lot2)

rangs_trnl_vect_lot2 <- as.vector(unlist(species_trnl_lot2[2, 21:144])) # 20:151
rangs_trnl_vect_lot2 <- factor(rangs_trnl_vect_lot2)
rangs_trnl_vect_lot2

rangs_trnl_vect <- c(rangs_trnl_vect_lot1, rangs_trnl_vect_lot2)

# Quelle précision pour mes données ? compter par rangs (species, family etc.)
sort(table(rangs_trnl_vect), decreasing = TRUE) #tableau répartition rangs
distribution_species <- as.data.frame(table(rangs_trnl_vect))
colnames(distribution_species) <- c("rangs", "count")

ggplot(distribution_species, aes(x = rangs, y = count, fill = rangs)) +
  geom_col() +                            
  theme_minimal() +                       
  labs(
    x = "Rangs",                           
    y = "Nombre d'occurrences",            
    title = "Distribution des rangs des espèces"  
  ) +
  theme(
    legend.position = "none"                            
  ) +
  scale_fill_brewer(palette = "Set3")                 

## Transformation de la bdd de long à large
head(species_trnl_lot2)
species_trnl_ok_lot1 <- species_trnl_lot1[-c(1,2),]
species_trnl_ok_lot2 <- species_trnl_lot2[-c(1,2),]

species_trnl_ok <- plyr::rbind.fill(species_trnl_ok_lot1, species_trnl_ok_lot2)

species_trnl_ok_chamois <- species_trnl_ok %>%
  left_join(species_an_occ1[,c("N_Antagene","Taxon1")], by="N_Antagene") %>%
  filter(Taxon1.y == "Rupicapra rupicapra")

species_trnl_long <- species_trnl_ok_chamois %>%
  pivot_longer(cols = c(20:137, 148:198),
               names_to = "species",
               values_to = "occurrences") %>%
  mutate(occurrences = as.numeric(as.character(occurrences)),
         Latitude = as.numeric(as.character(gsub(",", ".", Latitude))),
         Longitude = as.numeric(as.character(gsub(",", ".", Longitude))))

save(species_trnl_long, file="Output/Rdata/species_trnl_long.Rdata")
