library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readODS)

#########################################################
###   Concordance Plantes Régime vs Plantes Terrain   ###
#########################################################

# Working directory de Louise
setwd("~/REGIME_ALIM_CHAM_GIT/Regime_alimentaire_chamois")
# Working directory de Marjo
setwd("/Volumes/SHARED/Git_Projects/Regime_alimentaire_chamois")

#########################
## Plantes Régime
#########################

# Importer le jeu de données trnl 
# import_list pour importer toutes les feuilles du fichier
species_trnl <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100.xlsx", col_names = FALSE)

# On se focalise sur la feuille indiquant la liste des plantes identifiées
taxo_trnl <- species_trnl$`Taxonomie seuil`

# On renomme les colonnes
colnames(taxo_trnl) <- taxo_trnl[1,] %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

taxo_trnl1 <- taxo_trnl %>%
  slice(-1) %>%
  arrange(scientific_name) # ordonner selon les scientific_name

#########################
## Plantes Terrain
#########################

# Importer la base de données de plantes
plantes_terrain <- read_ods("BDD/231219_sp_statuts_bfc_a_diffuser.ods")  %>%
  mutate(nom_scientifique1 = str_extract(
  nom_scientifique,
  "^[A-Z][a-z]+\\s+[a-z-]+"
  ),
  genre = word(nom_scientifique, 1)) # on ne garde que le binôme genre+espèce
  
head(plantes_terrain)


#########################
## Concordances
#########################

# Plantes du régime présentes dans la bdd BFC
common_species <- taxo_trnl1 %>%
  filter(scientific_name %in% plantes_terrain$nom_scientifique1)

# Plantes du régime présentes dans la bdd BFC
non_common_plants <- taxo_trnl1 %>%
  filter(!scientific_name %in% plantes_terrain$nom_scientifique1) 

  # Focus sur la liste des espèces présentes dans le régime mais non présentes dans la bdd BFC
non_common_species <- non_common_plants %>%
  filter(rank == "species") 
# 17 espèces







