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
species_animal_lot1 <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-12S/ANTAGENE-F1016-FDC70-Regime_Chamois-12S-seuil_100-100-100.xlsx")
species_animal_lot2 <- import_list("BDD/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-12S/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-12S-seuil_100-100-100.xlsx")
str(species_animal_lot1)

species_an_occ_lot1 <- species_animal_lot1$`Occurrences seuil`
species_an_occ_lot2 <- species_animal_lot2$`Occurrences seuil`

# Renommage des colonnes
# Nettoyage des noms de colonnes (suppr espaces, changements tirets etc)
colnames(species_an_occ_lot1) <- species_an_occ_lot1[2,] %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

colnames(species_an_occ_lot2) <- species_an_occ_lot2[2,] %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

# Attribution des classes d'objet par colonne
species_an_occ1_lot1 <- species_an_occ_lot1 %>%
  slice(-c(1, 2)) %>%
  mutate(across(c(Latitude, Longitude, Departement,
                  14:28, 30, 32, 34), ~as.numeric(as.character(.)))) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

species_an_occ1_lot2 <- species_an_occ_lot2 %>%
  slice(-c(1, 2)) %>%
  mutate(across(c(Latitude, Longitude, Departement,
                  15:27, 29, 31, 33), ~as.numeric(as.character(.)))) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

# On merge les 2 lots
species_an_occ1 <- plyr::rbind.fill(species_an_occ1_lot1, species_an_occ1_lot2)

# On sauve le .Rdata pour l'utiliser dans le Rmarkdown
save(species_an_occ1, file="Output/Rdata/species_an_occ1.Rdata")

# Graph de répartition des échantillons par espèce d'ongules 
plot1 <- ggplot(species_an_occ1) +
  geom_bar(aes(x=Taxon1)) +
  ylab("Nombre de fecès") +
  xlab("Espèces identifées")
ggsave("Output/hist_species.pdf", plot = plot1) # Sauver le plot

# Plot nb d'échantillons par jour
plot2 <- ggplot(species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]) +
  geom_bar(aes(x=Date)) +
  ylab("Nombre de fecès")
ggsave("Output/hist_pardate.pdf", plot = plot2) # Sauver le plot

# Plot nb d'échantillons par département
plot3 <- ggplot(species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]) +
  geom_bar(aes(x=as.factor(as.character(Departement)))) +
  ylab("Nombre de fecès") +
  xlab("Département")
ggsave("Output/hist_pardepartement.pdf", plot = plot3) # Sauver le plot

# Carte de répartition des échantillons
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]$Longitude, 
                   species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]$Latitude, radius=1)
# 2 échantillons avec NA
# Echantillons plus bas que prévu ?
  