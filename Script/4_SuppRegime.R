library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(ade4)
library(ape)
library(cluster)
library(FactoMineR)
library(factoextra)
library(spaa)

options(tibble.width = Inf)

##########################################################
### Analyses supplémentaires sur le régime des chamois ###
##########################################################

load("Output/Rdata/diet_long_chamois.Rdata")
head(diet_long_chamois)
load("Output/Rdata/species_an_occ1.Rdata")
dim(species_an_occ1)

## Extraction de la liste des plantes
plants_in_diet <- unique(diet_long_chamois$species)
write.csv(plants_in_diet, file="Output/Rdata/plants_in_diet.csv")
# Réimport de la bdd avec les plantes catégorisées en groupes fonctionnels
plants_in_diet_categorized <- read.csv("BDD/plants_in_diet_categorized.csv", h=T, sep=";") %>%
  select(-X)

# "E00963497" "E00963498" "E00963508" "E00963751" "E00963843" "E00944163" n'ont que des 0
# Validé dans le fichier initial, problème de séquençage

######################################
## Calculs sur les régimes
######################################

### 1. Avec les plantes

# Régime par individu en %
species_trnl_long1 <- diet_long_chamois %>%
  group_by(N_Antagene) %>%
  mutate(pourcentage_occ = occurrences/sum(occurrences, na.rm=T)*100) %>%
  filter(!N_Antagene %in% c("E00963497", "E00963498", "E00963508", "E00963751", "E00963843", "E00944163"),
         !is.na(Departement)) %>%
  group_by(N_Antagene) %>%                         
  filter(sum(occurrences, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  group_by(species) %>%                         
  filter(sum(occurrences, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  select(-Date) %>%
  left_join(species_an_occ1[,c("N_Antagene","Date")], by = "N_Antagene") %>%
  mutate(Month = as.factor(as.character(lubridate::month(Date))))

# Régime moyen par département
freq_seq_mean_pardep <- species_trnl_long1 %>%
  group_by(species, Departement) %>%
  summarise(
    n = n(),
    moyenne = mean(pourcentage_occ, na.rm = TRUE),
    sd = sd(pourcentage_occ, na.rm = TRUE),
    se = sd / sqrt(n),
    t = qt(0.975, df = n - 1),
    IC_inf = moyenne - t * se,
    IC_sup = moyenne + t * se
  ) %>%
  filter(!is.na(Departement)) %>%
  arrange(desc(moyenne))

# Régime moyen tous départements confondus
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

# Merge régime par individu et par département
species_trnl_long_pardep <- species_trnl_long1 %>%
  left_join(freq_seq_mean_pardep, by = c("Departement","species"))

species_trnl_long_tot <- species_trnl_long1 %>%
  left_join(freq_seq_mean, by = c("species"))

# Long to wide
diet_wide_chamois <- species_trnl_long1 %>% 
  pivot_wider(id_cols = c(N_Antagene, Departement, Circuitcomptage, Month), names_from = species, values_from = pourcentage_occ) %>%
  mutate(combi_name = paste(Departement, Circuitcomptage, Month, sep=", "))

colSums(is.na(diet_wide_chamois))
#diet_wide_chamois[is.na(diet_wide_chamois$Buxbaumiaviridis),]$N_Antagene

apply(diet_wide_chamois[,c(5:159)], 1, sum)

save(diet_wide_chamois, file="Output/Rdata/diet_wide_chamois.Rdata")

### 2. Avec les groupes fonctionnels

# Régime par individu en %
species_trnl_long1_pfg <- diet_long_chamois %>%
  left_join(plants_in_diet_categorized, by = c("species" = "Plant")) %>%
  group_by(Functional_group, N_Antagene, Departement, Date) %>%
  summarise(sum_occ = sum(occurrences, na.rm = T)) %>%
  group_by(N_Antagene) %>%
  mutate(pourcentage_occ_pfg = sum_occ/sum(sum_occ, na.rm = T)*100) %>%
  filter(!N_Antagene %in% c("E00963497", "E00963498", "E00963508", "E00963751", "E00963843", "E00944163"),
         !is.na(Departement)) %>%
  group_by(N_Antagene) %>%                         
  filter(sum(sum_occ, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  group_by(Functional_group) %>%                         
  filter(sum(sum_occ, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  select(-Date) %>%
  left_join(species_an_occ1[,c("N_Antagene","Date")], by = "N_Antagene") %>%
  mutate(Month = as.factor(as.character(lubridate::month(Date)))) 

# Régime moyen par département
freq_seq_mean_pardep_pfg <- species_trnl_long1_pfg %>%
  group_by(Functional_group, Departement) %>%
  summarise(
    n = n(),
    moyenne = mean(pourcentage_occ_pfg, na.rm = TRUE),
    sd = sd(pourcentage_occ_pfg, na.rm = TRUE),
    se = sd / sqrt(n),
    t = qt(0.975, df = n - 1),
    IC_inf = moyenne - t * se,
    IC_sup = moyenne + t * se
  ) %>%
  filter(!is.na(Departement)) %>%
  arrange(desc(moyenne))

# Régime moyen tous départements confondus
freq_seq_mean_pfg <- species_trnl_long1_pfg %>%
  group_by(Functional_group) %>%
  summarise(
    n = n(),
    moyenne = mean(pourcentage_occ_pfg, na.rm = TRUE),
    sd = sd(pourcentage_occ_pfg, na.rm = TRUE),
    se = sd / sqrt(n),
    t = qt(0.975, df = n - 1),
    IC_inf = moyenne - t * se,
    IC_sup = moyenne + t * se
  ) %>%
  arrange(desc(moyenne))


######################################
## Variabilité inter-départements
######################################

diet_wide_chamois_pardep <- freq_seq_mean_pardep %>% 
  pivot_wider(id_cols = Departement, names_from = species, values_from = moyenne)

save(diet_wide_chamois_pardep, file="Output/Rdata/diet_wide_chamois_pardep.Rdata")

# 1. PCA
pca_chamois <- dudi.pca(diet_wide_chamois[,c(5:159)], scannf = F, nf = 3)
scatter(pca_chamois, npcs=3)

# 2. BCA
bet_recouv <- bca(pca_chamois, as.factor(diet_wide_chamois$Departement), scan = FALSE, nf = 2)
s.class(bet_recouv$ls, as.factor(diet_wide_chamois$Departement), sub = "Between sites PCA (env)", csub = 1.75)
plot(bet_recouv)

# 3. Indice de Pianka pour mesurer l'overlap entre régimes / diet similarity entre départements (matrice de recouvrement de niche entre tous les départements)
diet_matrix <- as.data.frame(diet_wide_chamois_pardep[,-1])
rownames(diet_matrix) <- as.factor(as.character(diet_wide_chamois_pardep$Departement))
diet_matrix_t <- t(diet_matrix)

pianka_result <- niche.overlap(diet_matrix_t, method = "pianka")
pianka_result

pianka_df <- as.data.frame(as.table(as.matrix(pianka_result)))
colnames(pianka_df) <- c("Dept1", "Dept2", "Overlap")

ggplot(pianka_df, aes(x = Dept1, y = Dept2, fill = Overlap)) +
  geom_tile() +
  geom_text(aes(label = round(Overlap, 2)), color = "black") +
  scale_fill_gradient(low = "white", high = "darkred", limits = c(0,1)) +
  theme_minimal() +
  labs(title = "Indice de recouvrement de niche (Pianka)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Fort recouvrement de régimes moyens entre les départements

save(pianka_df, file="Output/Rdata/pianka_df.Rdata")

# 4. Indice de Pianka pour mesurer l'overlap entre régimes de chaque individu
diet_matrix_allindiv <- as.data.frame(diet_wide_chamois[,c(5:159)])
rownames(diet_matrix_allindiv) <- as.factor(as.character(diet_wide_chamois$N_Antagene))
diet_matrix_allindiv_t <- t(diet_matrix_allindiv)

pianka_result_allindiv <- niche.overlap(diet_matrix_allindiv_t, method = "pianka")
pianka_result_allindiv

pianka_df_allindiv <- as.data.frame(as.table(as.matrix(pianka_result_allindiv)))
colnames(pianka_df_allindiv) <- c("Indiv1", "Indiv2", "Overlap")

ggplot(pianka_df_allindiv, aes(x = Indiv1, y = Indiv2, fill = Overlap)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred", limits = c(0,1)) +
  theme_minimal() +
  labs(title = "Indice de recouvrement de niche (Pianka) entre tous les individus", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save(pianka_df_allindiv, file="Output/Rdata/pianka_df_allindiv.Rdata")


######################################
## Variabilité inter-individuelle
######################################

# 1. Plot des régimes en heatmap
heatmap_regime <- species_trnl_long1 %>%
  group_by(species) %>%                         
  filter(mean(pourcentage_occ, na.rm = TRUE) > 1) %>%
  ungroup() %>%
  ggplot(aes(x=species, y=N_Antagene, fill=pourcentage_occ)) +
  geom_tile() +
  ggtitle("Pourcentage de plantes dans les régimes alimentaires des chamois (>1% dans le régime moyen)") +
  scale_fill_gradientn(colours = c("white","goldenrod1","orange","red","black"),
                       limits = c(0, 100),
                       name = "Pourcentages",
                       na.value = "white") +  
  facet_wrap(~ Departement, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10)) +
  labs(x = "Plantes",
       y = "Fecès") 
heatmap_regime

# Dans le 88, certains chamois avec Acer en forte quantité et d'autres non
# Dépend fortement du circuit
sub_acer <- species_trnl_long1 %>%
  filter(species == "Acer",
         Departement == "88") %>%
  mutate(acer_sup = ifelse(pourcentage_occ > 1, "acer_sup", "acer_inf")) %>%
  left_join(species_an_occ1, by = "N_Antagene") %>%
  mutate(month = as.factor(as.character(lubridate::month(Date.y))))

summary(lm(pourcentage_occ ~ month + Circuitcomptage.x + Milieu.x, data = sub_acer))
table(sub_acer$Circuitcomptage.x)

# 2. PcoA
# Représente la distance euclidienne entre les fecès
pcoa_diet <- pcoa(dist(diet_wide_chamois[,c(5:159)]))
biplot(pcoa_diet)

scores <- as.data.frame(pcoa_diet$vectors[, 1:2])
colnames(scores) <- c("Axis1", "Axis2")
scores$departement <- diet_wide_chamois$Departement  # adapter le nom

# % de variance expliquée pour les labels d'axes
var_exp <- round(pcoa_diet$values$Relative_eig[1:2] * 100, 1)

ggplot(scores, aes(x = Axis1, y = Axis2, color = departement)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.68) +  
  labs(x = paste0("Axis 1 (", var_exp[1], "%)"),
       y = paste0("Axis 2 (", var_exp[2], "%)"),
       color = "Département") +
  theme_minimal()

save(scores, file="Output/Rdata/scores_pcoa.Rdata")
save(var_exp, file="Output/Rdata/var_exp_pcoa.Rdata")

# 3. Dendrogrammes
dist_matrix <- dist(diet_wide_chamois[,c(5:159)], method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, labels = diet_wide_chamois$combi_name, cex=1.2)

# 4. K-means - classification non supervisée
# Axes n'expliquent pas bcp de variance
diet_clust <- diet_wide_chamois[,c(5:159)]

fviz_nbclust(diet_clust, kmeans, method = "wss")
fviz_nbclust(diet_clust, kmeans, method = "silhouette")

k3 <- kmeans(diet_clust, centers = 2, nstart = 100)
fviz_cluster(k3, data = diet_clust)

# 5. Calcul de l'indice de Schoener
species_trnl_long_pardep2 <- species_trnl_long_pardep %>%
  mutate(schoener = 1-0.5*(pourcentage_occ-moyenne))

species_trnl_long_pardep3 <- species_trnl_long_pardep2 %>%
  group_by(N_Antagene) %>%
  summarise(mean_schoener = mean(schoener))

summary(lm(schoener ~ Departement, species_trnl_long_pardep2))

species_trnl_long_tot2 <- species_trnl_long_tot %>%
  mutate(schoener = 1-0.5*(pourcentage_occ-moyenne))
hist(species_trnl_long_tot2$schoener)

species_trnl_long_tot3 <- species_trnl_long_tot2 %>%
  group_by(N_Antagene) %>%
  summarise(mean_schoener = mean(schoener))

