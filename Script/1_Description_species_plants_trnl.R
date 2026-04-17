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

fn_nettoyage <- function(data){
  
  data <- data$`Occurrences seuil`
  
  colnames(data) <- as.character(data[3,])
  data <- data[-3,]
  
  colnames(data) <- colnames(data) %>%
    str_replace_all(" ", "") %>%
    str_replace_all("-", "_") %>%
    stri_trans_general("Latin-ASCII")
  
  return(data)
}

fn_rename_taxo <- function(data, family){
  data <- data$`Taxonomie seuil`
  
  colnames(data) <- data[1,]
  
  data <- data[-1,] %>%
    filter(family_name == family) %>%
    mutate(scientific_name1 = gsub(" ", "", scientific_name))
  
  return(data)
}

fn_select_family <- function(data, taxo){
  data %>%
    select(N_Antagene,
           any_of(taxo$scientific_name1)) %>%
    mutate(across(any_of(taxo$scientific_name1), as.numeric)) %>%
    filter(
      rowSums(across(any_of(taxo$scientific_name1)), na.rm = TRUE) > 0
    ) %>%
    mutate(
      total = rowSums(across(-N_Antagene), na.rm = TRUE)
    ) %>%
    mutate(
      across(-c(N_Antagene, total),
             ~ (. / total))
    ) %>%
    select(-total)
}


# trnl --------------------------------------------------------------------

# Importer le jeu de données trnl 
species_trnl_lot1 <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-trnl-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_trnl_lot2 <- import_list("BDD/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-trnL-Liste_DREAL/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-trnL-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)

# Import de la bdd des espèces animales correspondant aux crottes pour filtrer les chamois
load("Output/Rdata/species_an_occ1.Rdata")

# Nettoyage des noms de colonnes (suppr espaces, changements tirets etc)
# pour l'onglet "Occurrences"
species_trnl_lot1_occ <- fn_nettoyage(species_trnl_lot1)
species_trnl_lot2_occ <- fn_nettoyage(species_trnl_lot2)

# J'extrait les deux premières lignes (occurences, vecteur de rangs taxonomiques) pour les isoler. 
# puis convertir en vecteur simple pour manipuler facilement
occurence_trnl_vect_lot1 <- unlist(species_trnl_lot1_occ[1, 20:137]) # 20:151
occurence_trnl_vect_lot1 <- factor(occurence_trnl_vect_lot1)

rangs_trnl_vect_lot1 <- as.vector(unlist(species_trnl_lot1_occ[2, 20:137])) # 20:151
rangs_trnl_vect_lot1 <- factor(rangs_trnl_vect_lot1)
rangs_trnl_vect_lot1

occurence_trnl_vect_lot2 <- unlist(species_trnl_lot2_occ[1, 21:144]) # 20:151
occurence_trnl_vect_lot2 <- factor(occurence_trnl_vect_lot2)

rangs_trnl_vect_lot2 <- as.vector(unlist(species_trnl_lot2_occ[2, 21:144])) # 20:151
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
head(species_trnl_lot2_occ)
species_trnl_ok_lot1_occ <- species_trnl_lot1_occ[-c(1,2),]
species_trnl_ok_lot2_occ <- species_trnl_lot2_occ[-c(1,2),]

species_trnl_ok_occ <- plyr::rbind.fill(species_trnl_ok_lot1_occ, species_trnl_ok_lot2_occ) %>%
  select(-c(PartTaxon1, PartTaxons1a2cumules, PartTaxonsmajoritairestotaux, Taxon1,
            OccurrencesTaxon1, Taxon2, OccurrencesTaxon2, Taxon3, OccurrencesTaxon3,
            Espece, Typeprelevement, Projet, Remarques, Qualite, Abondancemicro_organismes,
            Abondancetaxonsnoncibles, Abondancecibles, Occurrencesmicro_organismes,
            Occurrencestaxonsnoncibles, Occurrencescibles)) %>%
  mutate(across(
    10:last_col(),
    ~ as.numeric(as.character(.))
  ),
  across(
    10:last_col(),
    ~ replace_na(.x, 0)
  ))

species_trnl_ok_chamois_occ <- species_trnl_ok_occ %>%
  left_join(species_an_occ1[,c("N_Antagene","Taxon1")], by="N_Antagene") %>%
  filter(Taxon1 == "Rupicapra rupicapra") %>%
  select(-Taxon1)

species_trnl_long_occ <- species_trnl_ok_chamois_occ %>%
  pivot_longer(cols = c(10:178),
               names_to = "species",
               values_to = "occurrences") %>%
  mutate(occurrences = as.numeric(as.character(occurrences)),
         Latitude = as.numeric(as.character(gsub(",", ".", Latitude))),
         Longitude = as.numeric(as.character(gsub(",", ".", Longitude))))

save(species_trnl_long_occ, file="Output/Rdata/species_trnl_long_occ.Rdata")



# Primers specifiques (cype, aste, poac) ----------------------------------

# Importer les jeux de données des primers spécifiques
species_aste_lot1 <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-Aste-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-Aste-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_aste_lot2 <- import_list("BDD/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-Aste-Liste_DREAL/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-Aste-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)

species_cype_lot1 <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-Cype-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-Cype-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_cype_lot2 <- import_list("BDD/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-Cype-Liste_DREAL/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-Cype-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)

species_poac_lot1 <- import_list("BDD/ANTAGENE-F1016-FDC70-Regime_Chamois-Poac-Liste_DREAL/ANTAGENE-F1016-FDC70-Regime_Chamois-Poac-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)
species_poac_lot2 <- import_list("BDD/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-Poac-Liste_DREAL/ANTAGENE-F1064-FDC70-Regime_Chamois-lot2-Poac-seuil_100-100-100-Liste_DREAL.xlsx", col_names = FALSE)

# Nettoyage des noms de colonnes (suppr espaces, changements tirets etc)
species_aste_lot1_occ <- fn_nettoyage(species_aste_lot1)[-c(1,2),]
species_aste_lot2_occ <- fn_nettoyage(species_aste_lot2)[-c(1,2),]

species_cype_lot1_occ <- fn_nettoyage(species_cype_lot1)[-c(1,2),]
species_cype_lot2_occ <- fn_nettoyage(species_cype_lot2)[-c(1,2),]

species_poac_lot1_occ <- fn_nettoyage(species_poac_lot1)[-c(1,2),]
species_poac_lot2_occ <- fn_nettoyage(species_poac_lot2)[-c(1,2),]

# Taxonomie
taxo_aste_lot1 <- fn_rename_taxo(species_aste_lot1, family="Asteraceae")
taxo_aste_lot2 <- fn_rename_taxo(species_aste_lot2, family="Asteraceae")
taxo_cype_lot1 <- fn_rename_taxo(species_cype_lot1, family="Cyperaceae") # pas de cyperaceae
taxo_cype_lot2 <- fn_rename_taxo(species_cype_lot2, family="Cyperaceae") # pas de cyperaceae
taxo_poac_lot1 <- fn_rename_taxo(species_poac_lot1, family="Poaceae")
taxo_poac_lot2 <- fn_rename_taxo(species_poac_lot2, family="Poaceae")

# On sélectionne les espèces correspondant à chaque famille (aste, cype, poac)
# et on calcule la proportion de chaque espèce 
species_aste_lot1_occ_subset <- fn_select_family(data = species_aste_lot1_occ, taxo = taxo_aste_lot1)
species_aste_lot2_occ_subset <- fn_select_family(data = species_aste_lot2_occ, taxo = taxo_aste_lot2)

species_aste_occ_subset <- plyr::rbind.fill(species_aste_lot1_occ_subset, species_aste_lot2_occ_subset) %>%
  mutate(across(
    2:last_col(),
    ~ replace_na(.x, 0)
  ))

species_poac_lot1_occ_subset <- fn_select_family(data = species_poac_lot1_occ, taxo = taxo_poac_lot1)
species_poac_lot2_occ_subset <- fn_select_family(data = species_poac_lot2_occ, taxo = taxo_poac_lot2)

species_poac_occ_subset <- plyr::rbind.fill(species_poac_lot1_occ_subset, species_poac_lot2_occ_subset) %>%
  mutate(across(
    2:last_col(),
    ~ replace_na(.x, 0)
  ))

# On recalcule ensuite le nombre de séquences en fonction du nombre total 
# présent dans trnl pour chaque famille
fn_newseq <- function(data, trnl_long, family){
  data %>%
    left_join(trnl_long %>%
                filter(N_Antagene %in% data$N_Antagene,
                       species == family) %>%
                select(N_Antagene, occurrences), by="N_Antagene") %>%
    mutate(across(
      -c(N_Antagene, occurrences),
      ~ round(. * occurrences)
    )) %>%
    select(-occurrences) %>%
    filter(!if_all(-N_Antagene, ~ is.na(.x)))
}

species_aste_occ_subset_ok <- fn_newseq(data = species_aste_occ_subset, trnl_long = species_trnl_long_occ, family = "Asteraceae")
species_poac_occ_subset_ok <- fn_newseq(data = species_poac_occ_subset, trnl_long = species_trnl_long_occ, family = "Poaceae")

aste_poac_toadd <- species_aste_occ_subset_ok %>%
  full_join(species_poac_occ_subset_ok, by="N_Antagene") %>%
  mutate(across(
    2:last_col(),
    ~ replace_na(.x, 0)
  ))

# On remplace par 0 les asteracées et poacées qui étaient dans les bdd de primers specifiques
species_trnl_ok_chamois_occ$Asteraceae[species_trnl_ok_chamois_occ$N_Antagene %in% species_aste_occ_subset_ok$N_Antagene] <- 0
species_trnl_ok_chamois_occ$Poaceae[species_trnl_ok_chamois_occ$N_Antagene %in% species_poac_occ_subset_ok$N_Antagene] <- 0

# On merge le tout
species_trnl_ok_chamois_occ_all <- species_trnl_ok_chamois_occ %>%
  left_join(aste_poac_toadd, by="N_Antagene") %>%
  mutate(Poaceae = Poaceae.x + Poaceae.y,
         Avenellaflexuosa = Avenellaflexuosa.x + Avenellaflexuosa.y) %>%
  select(-c(Poaceae.x, Poaceae.y, Avenellaflexuosa.x, Avenellaflexuosa.y)) %>%
  mutate(across(
    10:last_col(),
    ~ replace_na(.x, 0)
  ))

# Vérification qu'il n'y ait pas de colonnes doublées
names(species_trnl_ok_chamois_occ_all)[grepl("\\.x$|\\.y$", names(species_trnl_ok_chamois_occ_all))]



# FINAL : transformation en long
diet_long_chamois <- species_trnl_ok_chamois_occ_all %>%
  pivot_longer(cols = c(10:195),
               names_to = "species",
               values_to = "occurrences") %>%
  mutate(occurrences = as.numeric(as.character(occurrences)),
         Latitude = as.numeric(as.character(gsub(",", ".", Latitude))),
         Longitude = as.numeric(as.character(gsub(",", ".", Longitude))))

save(diet_long_chamois, file="Output/Rdata/diet_long_chamois.Rdata")



















