##########################################################
# ANALYSE SPATIALE DES INEGALITES ECONOMIQUES REGIONALES #
##########################################################

#importation des libraries

#install.packages("sf")
#install.packages("spdep", type = "binary")
#install.packages("spatialreg")

library(readxl)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(spdep)
library(tmap)
library(spatialreg)

#importation des bases de données
carte = st_read("C:/Users/33695/Desktop/code/Projets/Econometrie_spatiale/Departements.shp")
chomage = read_excel("C:/Users/33695/Desktop/code/Projets/Econometrie_spatiale/Copie de famille_TAUX-CHOMAGE_21032025.xlsx")
active = read_excel("C:/Users/33695/Desktop/code/Projets/Econometrie_spatiale/TCRD_015 (1).xlsx", sheet = "DEP", skip = 3) 
revenu = read_excel("C:/Users/33695/Desktop/code/Projets/Econometrie_spatiale/Copie de base-cc-filosofi-2021-geo2024.xlsx")

#nettoyage des données

#Active#
colnames(active) = c("code_dep", "nom_dep", "actifs_total", "hommes", "femmes", "tx_activite_15_64", #Changement du nom des colonnes
                          "tx_activite_25_54", "tx_hommes_15_64", "tx_hommes_25_54", 
                          "tx_femmes_15_64", "tx_femmes_25_54")

active = active[-1, ] #suppression de la première ligne de la base

#Chômage#
chomage = chomage %>%
  filter(!grepl("Taux de chômage localisé par région", Libellé)) %>% #ici je filtre les données de la colonne Libellé qui commencent par "Taux de chômage localisé par région", et je supprime cette phrase des données
  select(-idBank, -Période, -`Dernière mise à jour`)                 #je supprime ici ces variables

chomage$Libellé = sub("Taux de chômage localisé par département - ","", chomage$Libellé) #idem je supprime une partie de ce qui est écris dans mes données Libellé

#Revenu#
if (any(is.na(revenu))) {                                         #ici on vérifie s'il y a au moins une valeur manquante NA dans revenu
  print("Des valeurs manquantes ont été trouvées.")
  print(colSums(is.na(revenu)))                                   #ici on affiche la phrase puis le nombre de NA par colonnes
  revenu = revenu %>%
    drop_na()                                                     #on supprime les lignes qui ont des valeurs manquantes
  print("lignes avec NA supprimées.")                             #fin du programme
} else {
  print("Pas de valeurs NA.")
}

#Fusion des bases de données

#Revenu#
Revenu_median = carte %>%
  left_join(revenu, by = c("DDEP_C_COD" = "Code géographique"))%>%                    #ici on fusionne revenu et carte pour avoir les coordonnées des départements
  mutate(`Médiane du niveau vie (€)`= as.numeric(`Médiane du niveau vie (€)`))        #changement du type de la variable
class(Revenu_median)                                                                  #vérification du type de la variable

if (any(is.na(Revenu_median))) {                                                      ##IDEM que précédement 
  print("Des valeurs manquantes ont été trouvées.")
  print(colSums(is.na(Revenu_median)))
  
  Revenu_median <- Revenu_median %>%
    drop_na()
  
  print("Lignes avec NA supprimées.")
} else {
  print("Pas de valeurs NA.")
}

#Active#
Active_rep = carte %>%
  left_join(active, by = c("DDEP_C_COD" = "code_dep"))%>%
  mutate(`actifs_total`= as.numeric(`actifs_total`)) %>%
  mutate(`tx_activite_15_64`= as.numeric(`tx_activite_15_64`)) %>%
  filter(DDEP_C_COD<= 95)                                                            #ici pour une carte assez petite, je supprime les données des outre-mer
class(Active_rep)

#Chomage#
# Nettoyer les noms de départements dans les deux bases
chomage = chomage %>%
  mutate(nom_dep = str_to_upper(str_replace_all(Libellé, "[^[:alnum:] ]", "")))      #ici j'avais un problème lié à l'orthographe du nom des départements (apostrophe/tirets)
                                                                                     #ainsi j'ai remis tout à l'identique pour chomage et activie
active_clean = active %>%
  mutate(nom_dep = str_to_upper(str_replace_all(nom_dep, "[^[:alnum:] ]", "")))

# Jointure pour ajouter seulement le code département à chomage
chomage = chomage %>%
  left_join(active_clean %>% select(nom_dep, code_dep), by = "nom_dep")

Chomage_dep = carte %>%
  left_join(chomage, by = c("DDEP_C_COD" = "code_dep")) %>%
  filter(DDEP_C_COD<= 95)

#Création de carte Choroplèthes
#Revenu#

ggplot(Revenu_median) + 
  geom_sf(aes(fill = `Médiane du niveau vie (€)`)) + #on choisi ici la colonne qui nous donnera les données pour la graphique
  scale_fill_viridis_c(option = "C") +               #c'est juste un filtre pour les couleurs utilisées (c= plasma; A= magma; B=inferno, etc)
  labs( title = "Revenu médian par département", fill = "€")+ #titre du graphique et de la légende
  theme_minimal()                                             #choix du fond et d'autres paramettres par défaut

#Active#
#Nombre d'actifs par département
ggplot(Active_rep) + 
  geom_sf(aes(fill = `actifs_total`)) + 
  scale_fill_viridis_c(option = "C") +
  labs( title = "Nombre d'actifs par département", fill = "Nb actifs")+
  theme_minimal()

#Proportion d'actifs par département
ggplot(Active_rep) + 
  geom_sf(aes(fill = `tx_activite_15_64`)) + 
  scale_fill_viridis_c(option = "C") +
  labs( title = "Part d'actifs par département", fill = "%")+
  theme_minimal()

#Chomage#
ggplot(Chomage_dep) + 
  geom_sf(aes(fill = `2024-T4`)) + 
  scale_fill_viridis_c(option = "C") +
  labs( title = "Chômage par département 2024-T4", fill = "%")+
  theme_minimal()

#Création d'histogrammes
#Histogramme d'activité entre hommes et femmes
#on nettoye notre base de données active pour obetenir les données entre homme et femme
active_long <- active %>%
  select(code_dep, nom_dep,tx_hommes_15_64, tx_femmes_15_64, tx_hommes_25_54, tx_femmes_25_54) %>%
  pivot_longer(cols = c(tx_hommes_15_64, tx_femmes_15_64, tx_hommes_25_54, tx_femmes_25_54),
               names_to = "Variable",
               values_to = "Valeur") %>%
  filter(code_dep == 90)              ##FILTRE du département A changer pour avoir une autre vision

active_long <- active_long %>%       #changement nom de variables
  mutate(Sexe = case_when(
    grepl("homme", Variable) ~ "Homme",
    grepl("femme", Variable) ~ "Femme"
  ))

active_long <- active_long %>%
  mutate(Indicateur = case_when(
    grepl("15_64", Variable) ~ "Taux activité 15-64 ans",
    grepl("25_54", Variable) ~ "Taux activité 25-54 ans"
  ))


ggplot(active_long, aes(x = Indicateur, y = Valeur, fill = Sexe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparaison Hommes-Femmes", x = "Indicateur", y = "Valeur") +
  theme_minimal()

#maintenant on veut faire la même chose mais sur une carte
# en changeant les variables a la ligne 160 modifie la comparaison
Active_rep_carte = Active_rep %>%
  mutate(dominant = ifelse(tx_hommes_25_54 > tx_femmes_25_54, "Hommes", "Femmes"))

ggplot(Active_rep_carte) +
  geom_sf(aes(fill = dominant))+
  scale_fill_manual(values = c("Hommes" = "blue", "Femmes" = "red"))+
  labs(title = "Dominance du taux d'activité 25-54 ans par sexe", fill = "Dominant")+
  theme_minimal()

#regardon le chomage dans les régions
Chomage_reg = Chomage_dep %>%
  group_by(DREG_L_LIB)%>%
  mutate(moy_reg = mean(`2024-T4`, na.rm = TRUE))%>%
  select(DDEP_C_COD, DDEP_L_LIB, DREG_L_LIB, `2024-T4`, moy_reg, geometry)

head(Chomage_reg$moy_reg)

ggplot(Chomage_reg) + 
  geom_sf(aes(fill = moy_reg))+
  scale_fill_viridis_c(option = "C", name = "Chômage moyen (%)") +
  labs(title = "Taux de chômage par région")+
  theme_minimal()

#nous allons maintenant passer au test d'autocorrélation spatiale
#tout d'abord la création de la matrice de contiguité
Chomage_dep <- st_make_valid(Chomage_dep)
voisins = poly2nb(Chomage_dep)

poids = nb2listw(voisins, style = "W", zero.policy = TRUE)

test_moran = moran.test(Chomage_dep$`2024-T4`, poids, zero.policy = TRUE)

print(test_moran)
## d'apres les résultats, on peut rejeter l'hypothese d'absence d'autocorrélation
## donc , le taux de chomage n'est pas distribué aléatoirement dans l'espace


#Analyse LISA:
#voir où sont les clusters
#identifier les points chauds et les points froids
# Calcul du LISA
lisa <- localmoran(Chomage_dep$`2024-T4`, poids, zero.policy = TRUE)

# Ajouter résultats à la base
Chomage_dep$local_moran <- lisa[, 1]
Chomage_dep$p_value <- lisa[, 5]

# Catégoriser en cluster High-High, Low-Low, High-Low, Low-High
Chomage_dep <- Chomage_dep %>%
  mutate(cluster = case_when(
    local_moran > 0 & `2024-T4` > mean(`2024-T4`, na.rm = TRUE) ~ "High-High",
    local_moran > 0 & `2024-T4` < mean(`2024-T4`, na.rm = TRUE) ~ "Low-Low",
    local_moran < 0 & `2024-T4` > mean(`2024-T4`, na.rm = TRUE) ~ "High-Low",
    local_moran < 0 & `2024-T4` < mean(`2024-T4`, na.rm = TRUE) ~ "Low-High",
    TRUE ~ "Non significatif"
  ))

# Basculer tmap en mode interactif
tmap_mode("view") #carte interactive

# Carte LISA
tm_shape(Chomage_dep) +
  tm_polygons("cluster", palette = "Set1", title = "LISA clusters") +
  tm_layout(main.title = "Carte LISA : clusters régionaux", legend.outside = TRUE)

#Construction d'un modéle
modele <- Active_rep_carte %>%
  st_drop_geometry() %>%  # Supprimer la géométrie pour join classique
  left_join(Chomage_reg %>% st_drop_geometry(), by = "DDEP_C_COD") %>%
  left_join(revenu, by = c("DDEP_C_COD" = "Code géographique"))


#nettoyage des variables
modele <- modele %>%
  mutate(
    `Médiane du niveau vie (€)` = as.numeric(gsub(",", ".", `Médiane du niveau vie (€)`)),
    `Taux de pauvreté-Ensemble (%)` = as.numeric(gsub(",", ".", `Taux de pauvreté-Ensemble (%)`)),
    `Part des revenus d'activité (%)` = as.numeric(gsub(",", ".", `Part des revenus d'activité (%)`)),
    `Part des pensions, retraites et rentes (%)` = as.numeric(gsub(",", ".", `Part des pensions, retraites et rentes (%)`)),
    `1ᵉʳ décile du niveau de vie (€)` = as.numeric(gsub(",", ".", `1ᵉʳ décile du niveau de vie (€)`))
  )

#ici on garder uniquement les lignes sans NA pour toutes les variables du modèle
modele_clean <- modele %>%
  filter(
    !is.na(tx_activite_15_64),
    !is.na(`Médiane du niveau vie (€)`),
    !is.na(`Taux de pauvreté-Ensemble (%)`),
    !is.na(`Part des revenus d'activité (%)`),
    !is.na(`Part des pensions, retraites et rentes (%)`),
    !is.na(`1ᵉʳ décile du niveau de vie (€)`),
    !is.na(`2024-T4`)
  )

modele_ols <- lm(`2024-T4` ~ tx_activite_15_64 + `Médiane du niveau vie (€)` + 
                   `Taux de pauvreté-Ensemble (%)` + `Part des revenus d'activité (%)` + 
                   `Part des pensions, retraites et rentes (%)` + `1ᵉʳ décile du niveau de vie (€)`,
                 data = modele_clean)
summary(modele_ols)