library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)

########################################################
### Description des espèces animales échantillonnées ###
########################################################

# Ici tu peux changer ton "working directory"
setwd("/Volumes/SHARED/Git_Projects/Regime_alimentaire_chamois")

species_animal <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-12S/ANTAGENE-F1016-FDC70-Regime_Chamois-12S-seuil_100-100-100.xlsx")
str(species_animal)

species_an_occ <- species_animal$`Occurrences seuil`

colnames(species_an_occ) <- species_an_occ[2,] %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")

species_an_occ1 <- species_an_occ %>%
  slice(-c(1, 2)) %>%
  mutate(across(c(Latitude, Longitude, Departement,
                  14:28, 30, 32, 34), ~as.numeric(as.character(.)))) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

plot1 <- ggplot(species_an_occ1) +
  geom_bar(aes(x=Taxon1))

ggsave("Output/hist_species.pdf", plot = plot1)

ggplot(species_an_occ1[is.element(species_an_occ1$Taxon1, "Rupicapra rupicapra"),]) +
  geom_bar(aes(x=Date))



  