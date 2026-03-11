library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readxl)

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
species_trnl <- read_excel("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
#species_trnl <- read_excel("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100.xlsx", col_names = FALSE)

# Nettoyage des noms de colonnes (suppr espaces, changements tirets etc)
colnames(species_trnl) <- as.character(species_trnl[3,])
species_trnl <- species_trnl[-3,]

colnames(species_trnl) <- colnames(species_trnl) %>%
  str_replace_all(" ", "") %>%
  str_replace_all("-", "_") %>%
  stri_trans_general("Latin-ASCII")


# J'extrait les deux premières lignes (occurences, vecteur de rangs taxonomiques) pour les isoler. 
# puis convertir en vecteur simple pour manipuler facilement
occurence_trnl_vect <- unlist(species_trnl[1, 20:146]) # 20:151
occurence_trnl_vect <- factor(occurence_trnl_vect)

rangs_trnl_vect <- as.vector(unlist(species_trnl[2, 20:146])) # 20:151
rangs_trnl_vect <- factor(rangs_trnl_vect)
rangs_trnl_vect

# Vérification et modification des classes 
class(occurence_trnl_vect)
class(rangs_trnl_vect)

# Quelle précision pour mes données ? compter par rangs (species, family etc.)
rangs_trnl_vect
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

##### Extraction des données avec occurrences par échantillons 
## BDD avec uniquement les espèces
samples_trnl_onlyspecies <- species_trnl[, 20:146]

# mettre la ligne des noms de plantes comme nom de colonnes et supprimer la ligne nom (ligne 3).
# + supprimer la ligne somme des occurrences que j'avais extraite plus tôt 
colnames(samples_trnl_onlyspecies) <- as.character(samples_trnl_onlyspecies[3,])
samples_trnl_onlyspecies <- samples_trnl_onlyspecies[-3,]
samples_trnl_onlyspecies <- samples_trnl_onlyspecies[-1,]



#####################
### Visualisation ###
#####################

head(species_trnl)
species_trnl_ok <- species_trnl[-c(1,2),]

# Passer en bdd "long"
species_trnl_long <- species_trnl_ok %>%
  pivot_longer(cols = c(20 : 137),
               names_to = "species",
               values_to = "occurrences") %>%
  mutate(occurrences = as.numeric(as.character(occurrences)))

# Calcul du % de séquences dans chaque crotte
species_trnl_long1 <- species_trnl_long %>%
  group_by(N_Antagene) %>%
  mutate(pourcentage_occ = occurrences/sum(occurrences)*100)

# Calcul de la fréquence de séquences (% moyen de séquences dans le régime)
freq_seq_mean <- species_trnl_long1 %>%
  group_by(species) %>%
  summarise(
    n = n(),
    moyenne = mean(pourcentage_occ, na.rm = TRUE),
    sd = sd(pourcentage_occ, na.rm = TRUE),
    se = sd / sqrt(n),
    t = qt(0.975, df = n - 1),
    IC_inf = moyenne - t * se,
    IC_sup = moyenne + t * se
  ) %>%
  arrange(desc(moyenne))

ggplot(freq_seq_mean[which(freq_seq_mean$moyenne>1),], aes(x = reorder(species, -moyenne), y = moyenne)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +
  labs(
    x = "Espèce",
    y = "Moyenne (en %)",
    title = "Moyenne du pourcentage de séquences par espèce avec IC 95%"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calcul de la fréquence d'occurrence (% de fois où une plante est dans une crotte)
n_feces <- length(unique(species_trnl_long1$N_Antagene))

freq_occ <- species_trnl_long1 %>%
  group_by(species) %>%
  filter(!occurrences == 0) %>%
  summarise(freq_occ = n()/n_feces*100) %>%
  arrange(desc(freq_occ))

ggplot(freq_occ[which(freq_occ$freq_occ>5),], aes(x = reorder(species, -freq_occ), y = freq_occ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Espèce",
    y = "Fréquence d'occurrence (en %)",
    title = "Fréquence d'occurrence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
