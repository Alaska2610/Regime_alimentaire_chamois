library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readODS)
library(tidyr)

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
species_trnl <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_aste <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-Aste-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-Aste-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_cype <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-Cype-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-Cype-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_poac <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-Poac-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-Poac-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)

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

plantes_terrain[is.element(plantes_terrain$nom_scientifique1, 
                           c("Viola lutea", "Campanula baumgartenii", "Lilium martagon", "Anemonastrum narcissiflorum", "Arnica montana")),]
# Pas de Campanul baumgartenii dans la BDD BFC


#########################
## Concordances
#########################

# Plantes de la bdd régime présentes dans la bdd BFC
common_species <- taxo_trnl1 %>%
  filter(scientific_name %in% plantes_terrain$nom_scientifique1)
# 26 espèces présentes dans le régime et dans la bdd BFC
# avec filtre DREAL : 63 espèces présentes dans le régime et dans la bdd BFC

# Plantes de la bdd régime non présentes dans la bdd BFC
non_common_plants <- taxo_trnl1 %>%
  filter(!scientific_name %in% plantes_terrain$nom_scientifique1) 

  # Focus sur la liste des ESPECES présentes dans la bdd régime mais non présentes dans la bdd BFC
non_common_species <- non_common_plants %>%
  filter(rank == "species") 
# 17 espèces présentes dans la bdd régime qui ne sont pas dans la bdd BFC
# avec filtre DREAL : 0 espèces présentes dans la bdd régime qui ne sont pas dans la bdd BFC
non_common_species$scientific_name

# Nombre d'ESPECES dans la bdd régime
diet_species <- taxo_trnl1 %>%
  filter(rank == "species") 
# 43 plantes au niveau ESPECE dans le régime
# avec fitre DREAL : 63 plantes au niveau ESPECE dans le régime

# Nb dans chaque taxon
table(taxo_trnl1$rank)
# avec filtre DREAL : 63 espèces, 41 genres, 10 familles, 3 ordre, 1 classe


### Liste globale des espèces "species_list"
# On récupère la liste des espèces potentielles identifiées dans le régime "species_list"
taxo_trnl2 <- taxo_trnl1 %>%
  mutate(species_list_clean = str_remove_all(species_list, "\\[|\\]|'"))

df_species_list <- taxo_trnl2 %>%
  separate_rows(species_list_clean, sep = ",\\s*") %>% # on attribue une ligne à chaque espèce de species_list
  mutate(rank_number = case_when(
    rank == "species" ~ 1,
    rank == "genus" ~ 2,
    rank == "family" ~ 3,
    rank == "order" ~ 4,
    rank == "class" ~ 5
  )) %>%
  group_by(species_list_clean) %>%
  filter(rank_number == min(rank_number)) %>% # parmi les espèces doublonnées, on conserve celles qui ont le plus petit rang dans le régime
  ungroup()


# Plantes de la bdd régime non présentes dans la bdd BFC
non_common_plants_all <- df_species_list %>%
  filter(!species_list_clean %in% plantes_terrain$nom_scientifique1) 
# avec filtre DREAL : 19 espèces

# Plantes de la bdd régime présentes dans la bdd BFC
common_species_all <- df_species_list %>%
  filter(species_list_clean %in% plantes_terrain$nom_scientifique1)
# avec filtre DREAL : 346 espèces

# On regarde les espèces présentes dans les Rosaceae qui n'auraient pas déjà été identifiées à l'espèce
print(df_species_list[is.element(df_species_list$scientific_name, "Rosaceae"),], n=30)

print(df_species_list[is.element(df_species_list$scientific_name, "Rubus"),], n=30)

print(df_species_list[is.element(df_species_list$scientific_name, "Acer"),], n=30)

unique(df_species_list[df_species_list$rank == "genus",]$scientific_name)
unique(df_species_list[df_species_list$rank == "species",]$scientific_name)

