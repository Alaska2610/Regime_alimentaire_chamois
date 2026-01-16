library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(leaflet)

########################################################
### Description des espèces animales échantillonnées ###
########################################################

# Ici tu peux changer ton "working directory"
setwd("~/REGIME_ALIM_CHAM_GIT/Regime_alimentaire_chamois")
# Working directory de Marjo
setwd("/Volumes/SHARED/Git_Projects/Regime_alimentaire_chamois")

# Importer le jeu de données espèces animales
species_animal <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-12S/ANTAGENE-F1016-FDC70-Regime_Chamois-12S-seuil_100-100-100.xlsx")
str(species_animal)

species_an_occ <- species_animal$`Occurrences seuil`

# Renommage des colonnes
# Nettoyage des noms de colonnes (suppr espaces, changements tirets etc)
colnames(species_an_occ) <- species_an_occ[2,] %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

# Attribution des classes d'objet par colonne
species_an_occ1 <- species_an_occ %>%
  slice(-c(1, 2)) %>%
  mutate(across(c(Latitude, Longitude, Departement,
                  14:28, 30, 32, 34), ~as.numeric(as.character(.)))) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

# Graph de répartition des échantillons par espèce d'ongules 
plot1 <- ggplot(species_an_occ1) +
  geom_bar(aes(x=Taxon1))

# Sauver le plot
ggsave("Output/hist_species.pdf", plot = plot1)

# Plot nb d'échantillons par jour
ggplot(species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]) +
  geom_bar(aes(x=Date))

# Carte de répartition des échantillons
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]$Longitude, 
                   species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]$Latitude, radius=1)
# 2 échantillons avec NA
# Echantillons plus bas que prévu ?
  