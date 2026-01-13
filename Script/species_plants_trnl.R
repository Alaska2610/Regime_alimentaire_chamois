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
summary(species_trnl)

#J'extrait les deux premières lignes (occurences, vecteur de rangs taxonomiques) pour les isoler. 
# puis convertir en vecteur simple pour manipuler facilement
occurence_trnl_vect <- unlist(species_trnl[1, 20:151])
occurence_trnl_vect <- factor(occurence_trnl_vect)

rangs_trnl_vect <- as.vector(unlist(species_trnl[2, 20:151]))
rangs_trnl_vect <- factor(rangs_trnl_vect)
rangs_trnl_vect


#vérification et modification des classes 
class(occurence_trnl_vect)
class(rangs_trnl_vect)

#Quelle précision pour mes données ? compter par rangs (species, family etc.)
rangs_trnl_vect
sort(table(rangs_trnl_vect), decreasing = TRUE) #tableau répartition rangs
distribution_species <- as.data.frame(table(rangs_trnl_vect))
colnames(distribution_species) <- c("rangs", "count")


ggplot(distribution_species, aes(x = rangs, y = count, fill = rangs)) +
  geom_col() +                            # barres selon la colonne count
  theme_minimal() +                       # style minimal
  labs(
    x = "Rangs",                           # axe X
    y = "Nombre d'occurrences",            # axe Y
    title = "Distribution des rangs des espèces"  # titre
  ) +
  theme(
    legend.position = "none"                             # enlever la légende si pas utile
  ) +
  scale_fill_brewer(palette = "Set3")                  # palette de couleurs

#Results (du plus représenté au moins représenté : Genus, species, family, order, class)


##### Extraction des donnees avec occurrences par échantillons 
samples_trnl <- species_trnl[, 20:151]

#mettre la ligne des noms de plantes comme nom de colonnes et supprimer la ligne nom (ligne 3).
# + supprimer la ligne somme des occurrences que j'avais extraite plus tôt 
colnames(samples_trnl)<-as.character(samples_trnl[3,])
samples_trnl<-samples_trnl[-3,]
samples_trnl<-samples_trnl[-1,]
##On est pas trop mal 

