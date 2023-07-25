install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gtsummary")
install.packages("sf")
install.packages("ggplot2")
# installation des packages nécessaires 


library(readxl)
library(dplyr)
library(tidyr)
library(gtsummary)
library(sf)
library(gt)
library(ggplot2)
# chargement des packages nécessaires

getwd()
# pour vérifier mon repertoire 
setwd("E:/Projet R")
# et pour changer de répertoire


#-----------------------------------------------
#        IMPORTATION ET MISE EN FORME
#-----------------------------------------------


projet<-read_excel('Base_Partie 1.xlsx')
head(projet)
# data.frame nommé projet
# head : pour lecture du data.frame


selection_variable <- projet[, c("key", "q1", "q2", "q23", "q24",
                                 "q24a_1", "q24a_2", "q24a_3", 
                                 "q24a_4", "q24a_5", "q24a_6", 
                                 "q24a_7", "q24a_9", "q24a_10",
                                 "q25", "q26", "q12", "q14b",
                                 "q16", "q17", "q19", "q20", 
                                 "filiere_1", "filiere_2", 
                                 "filiere_3", "filiere_4", 
                                 "q8", "q81", "gps_menlatitude", 
                                 "gps_menlongitude", 
                                 "submissiondate", "start", "today")]
# création de sous ensemble de data.frame

nombre_valeur_manquante <- colSums(is.na(selection_variable))
# pour avoir le nombre de valeur manquante par variable

tableau_valeur_manquante <- data.frame(variable=names(nombre_valeur_manquante),
                                       nombre_valeur_manquante=nombre_valeur_manquante)

# création de data.frame pour résumer les valeurs manquantes

print(tableau_valeur_manquante)
# on affiche le tableau des valeurs manquantes 

valeur_manquante_key<- is.na(projet$key)
# verification pour l'existence d'une varible manquante pour key

pme_concerne<-projet[valeur_manquante_key,]
# pour avoir les pme concernés

print(pme_concerne)
# affichage des pme concernés


#----------------------------------------------
#            CREATION DE VARIABLES
#----------------------------------------------

projet <- projet %>% rename(region=q1)
# renom de q1 en region

projet <- projet %>% rename(departement=q2)
# renom de q2 en departement

projet <- projet %>% rename(sexe=q23)
#renom de q23 en sexe 

projet$sexe_2 <- ifelse(projet$sexe=="femme",1,0)
# creation de la variable sexé_2 et application du choix binaire

langues <- projet %>% select(key, starts_with("q24a_"))
# creation du data.frame nommé langue

head(langues)
#afficher les premières lignes de langues 

projet$parle<- projet %>% select(key, starts_with("q24a_"))
# creation d'une variable langue et une autre alternative est :
# proje$parle <- rowSums(projet[, starts_with("q24a_")])

langues<- projet %>% select (key, parle)
# creation de langue à partir de key et parle

fusion_projet <- merge(projet, langues, by="key")
# on fussionne les data.frame projet et langues en 
# utilisant key comme la clé de fusion

head(fusion_projet)
# lecture des premières lignes


#------------------------------------------------
#           ANALYSE DESCRIPTIVE
#------------------------------------------------

repartition_sexe <- table(projet$sexe)
# compte le nombre de chaque type de sexe

repartition_niveau_instruction <- table(projet$q25)
# compte l'occurence de chaque niveau d'instruction

repartition_statut_juridique <- table(projet$q12)
# l'occurence de chaque statut juridique

repartition_proprietaire_locataire <- table(projet$q81)
# denombre l'occurence de chaque statut proprietaire_locataire

repartition_statut_sexe <- table(projet$q12, projet$sexe)
# tableau croisé en statut juridique et sexe

repartition_niveau_instruction_sexe <- table(projet$q25, projet$sexe)
# tableau croisé niveu d'instruction et sexe

repartition_proprietaire_sexe <- table(projet$q81, projet$sexe)
# tableau croisé entre statut proprietaire_locataire et sexe

tableau_synthese_1 <- projet %>% select(sexe, q25, q12, q81) %>% tbl_summary() %>% as_gt()
tableau_synthese_1
# tableau synthèse pour les premières répartitions


variables_pour_resume <- c("q26", "q16", "q17", "q19", "q20")

# Rassembler les variables de filière en une seule colonne
fusion_projet_long <- fusion_projet %>%
  pivot_longer(cols = starts_with("filiere_"), names_to = "filiere", values_to = "filiere_value")

# Créer un tableau de résumé pour les variables sélectionnées, groupées par "filiere"
tableau_resume <- fusion_projet_long %>%
  select(filiere, all_of(variables_pour_resume)) %>%
  tbl_summary(by = filiere, missing = "no") %>%
  add_stat_label() %>%
  modify_caption("Statistiques descriptives pour les variables sélectionnées par filière") %>%
  as_gt()

# Afficher le tableau de résumé
tableau_resume

# Convertir le tableau gtsummary en objet gt
tableau_gt <- gt::gt(tableau_resume)

#-----------------------------------------------------------
#                UN PEU DE CARTOGRAPHIE
#-----------------------------------------------------------

library(sf)
library(ggplot2)

projet_map <- st_as_sf(projet, coords = c("gps_menlongitude", "gps_menlatitude"), crs = 4326)
# on crée un objet sf à partir du data.frame projet

head(projet_map)

ggplot(data = projet_map) +
  geom_point(aes(x = "gps_menlongitude", y = "gps_menlatitude", color = sexe), size = 3) +
  labs(title = "Répartition spatiale des PME selon le sexe",
       x = "Longitude", y = "Latitude",
       color = "Sexe") +
  theme_minimal()
#représentation spatiale des PME selon le sexe


ggplot(data = projet_map) +
  geom_point(aes(x = "gps_menlongitude", y = "gps_menlatitude", color = q25), size = 3) +
  labs(title = "Répartition spatiale des PME selon le niveau d'instruction",
       x = "Longitude", y = "Latitude",
       color = "Niveau d'instruction") +
  theme_minimal()
# représentation spatiale des PME selon le niveau d'inscription





#------------------------------------------------------------

#                PARTIE 2
#                                PARTIE 2

#------------------------------------------------------------



#-------------------------------------------------------------
#                  NETTOYAGE ET GESTION DES DONNEES
#-------------------------------------------------------------

install.packages("tidyverse")
# installation des packages nécessaires

library(tidyverse)

# chargement des packages nécessaires


data <- read_excel("Base_Partie 2.xlsx", sheet = 1)
# importer la base

head(data)
# lecture des premières lignes

data <- data %>%
  rename(destination = country_destination) %>%
  mutate(destination = ifelse(destination < 0, NA, destination))
# renom de country_destination en destination
# definition des valeurs negatives en valeur manquante


# Créer une nouvelle variable contenant des tranches d’âge de 5 ans en utilisant la variable “age”
data <- data %>%
  mutate(age_group = cut(age, breaks = seq(0, 100, 5), labels = FALSE, include.lowest = TRUE))
# Création des tranches d'âge de 5ans pour la variable âge

data <- data %>%
  group_by(id) %>%
  mutate(nombre_interviews = n())
# création de variable contenant le nombre d'interview par agent


# Avant l'application de sample(), on vérifie la taille de chaque groupe
group_sizes <- data %>%
  group_by(id) %>%
  summarize(group_size = n())

print(group_sizes)


set.seed(42) 
# Réproduction des mêmes résultats aléatoires
data <- data %>%
  mutate(group_assignment = sample(0:1, n(), replace = TRUE))
# Créer une nouvelle variable qui affecte aléatoirement chaque répondant à un groupe de traitement (1) ou de controle (0).



district_population <- read_excel("Base_Partie 2.xlsx", sheet = 2)
# Chargement des données de la feuille 2
# La base contient la taille de la population de chaque district

head(district_population)
colnames(data)
colnames(district_population)

#data <- left_join(data, district_population, by = c("endline_district" = "district"))
data <- left_join(data, district_population, by = "district")
# Fusion de la feuille 2 avec la feuille 1


data <- data %>%
  mutate(duree = endtime - starttime) %>%
  group_by(id) %>%
  mutate(moy_duree = mean(duree, na.rm = TRUE))
# Calcule de la durée de l’entretien 
# Calcule de la durée moyenne de l’entretien par enquêteur


names(data) <- lapply(names(data), function(x) paste("endline_", x, sep = ""))
# renomçage des variables par une boucle

names(data)
# affichage des données après renom.


#---------------------------------------------------------------
#               ANALYSE ET VISUALISATION DES DONNEES
#---------------------------------------------------------------

install.packages("gt")
install.packages("stargazer")

library(dplyr)
library(ggplot2)
library(knitr)
library(gt)
library(stargazer)
# chargement des packages nécessaires
names(data)

tableau_recap <- data %>%
  group_by(endline_district) %>%
  summarise(age_moyen = mean(endline_age, na.rm = TRUE),
            enfants_moyen = mean(endline_children_num, na.rm = TRUE))
# tableau recapitulatif de l'âge moyen 
# et du nombre d'enfant moyen par district



# nous allons effectuer un test de student 
test_age_sex <- t.test(endline_age ~ endline_sex, data = data)

# Obtenir la valeur p du test
p_value <- test_age_sex$p.value

if (p_value < 0.05) {
  message("La différence d'âge entre les sexes est statistiquement significative.")
} else {
  message("Il n'y a pas de différence d'âge statistiquement significative entre les sexes.")
}


# Filtrer les données pour exclure les âges au-delà de 100 ans
data_filtered <- data %>%
  filter(endline_age <= 100)

# Créer le nuage de points en utilisant les données filtrées
ggplot(data_filtered, aes(x = endline_age, y = endline_children_num)) +
  geom_point(position = position_jitter(width = 0.2), color = "blue", size = 3, alpha = 0.7) +
  labs(x = "Âge", y = "Nombre d'enfants", title = "Nuage de points : Âge vs Nombre d'enfants") +
  theme_minimal()

graphique <- ggplot(data_filtered, aes(x = endline_age, y = endline_children_num)) +
  geom_point(position = position_jitter(width = 0.2), color = "blue", size = 3, alpha = 0.7) +
  labs(x = "Âge", y = "Nombre d'enfants", title = "Nuage de points : Âge vs Nombre d'enfants") +
  theme_minimal()

graphique
#ggplot(data, aes(x = age, y = children_num)) +
#geom_point(position = position_jitter(width = 0.2), color = "blue", size = 3, alpha = 0.7) +
#labs(x = "Âge", y = "Nombre d'enfants", title = "Nuage de points : Âge vs Nombre d'enfants") +
#theme_minimal()

# Création du groupe de traitement et celui du controle

# Calculer la moyenne de la variable "intention"
mean_intention <- mean(data$endline_intention)
print(mean_intention)
# Créer les variables "groupe_traitement" et "groupe_controle"
data$groupe_traitement <- ifelse(data$endline_intention > mean_intention, 1, 0)
data$groupe_controle <- ifelse(data$endline_intention <= mean_intention, 1, 0)


modele_intention <- lm(endline_intention ~ groupe_traitement, data = data)
summary(modele_intention)


# Fonction pour extraire les coefficients et erreurs standard en gérant les cas où le modèle n'a pas de coefficient spécifique
get_coefficients <- function(model) {
  coefficients <- tryCatch(coef(model), error = function(e) rep(NA, length(coef(model))))
  std_errors <- tryCatch(summary(model)$coefficients[, "Std. Error"], error = function(e) rep(NA, length(coef(model))))
  return(list(coefficients, std_errors))
}


# Extraire les coefficients et erreurs standard pour chaque modèle
coefficients_A <- get_coefficients(modele_A)
coefficients_B <- get_coefficients(modele_B)
coefficients_C <- get_coefficients(modele_C)

# Créer un tableau de régression en utilisant la fonction data.frame
tableau_regression <- data.frame(
  Modèle = c("Modèle A", "Modèle B", "Modèle C"),
  Estimateur = c(coefficients_A$Estimateur, coefficients_B$Estimateur, coefficients_C$Estimateur),
  Erreur_standard = c(coefficients_A$Erreur_standard, coefficients_B$Erreur_standard, coefficients_C$Erreur_standard),
  P_value = c(summary(modele_A)$coefficients[, "Pr(>|t|)"],
              summary(modele_B)$coefficients[, "Pr(>|t|)"],
              summary(modele_C)$coefficients[, "Pr(>|t|)"])
)

# Afficher le tableau de régression dans le document Word
knitr::kable(tableau_regression)





modele_A <- lm(endline_intention ~ groupe_traitement, data = data)
modele_B <- lm(endline_intention ~ groupe_traitement + endline_age + endline_sex, data = data)
modele_C <- lm(endline_intention ~ groupe_traitement + endline_age + endline_sex + endline_district, data = data)


# Créer le tableau de régression en utilisant data.frame avec des vecteurs de même longueur
tableau_regression <- data.frame(
  Modèle = c("Modèle A", "Modèle B", "Modèle C"),
  Estimateur = unlist(get_coefficients(modele_A)[1], get_coefficients(modele_B)[1], get_coefficients(modele_C)[1]),
  Erreur_standard = unlist(get_coefficients(modele_A)[2], get_coefficients(modele_B)[2], get_coefficients(modele_C)[2]),
  P_value = c(summary(modele_A)$coefficients[, "Pr(>|t|)"],
              summary(modele_B)$coefficients[1, "Pr(>|t|)"],
              summary(modele_C)$coefficients[1, "Pr(>|t|)"])
)

tableau_regression
















get_coefficients <- function(model) {
  coefficients <- coef(model)
  std_errors <- summary(model)$coefficients[, "Std. Error"]
  return(list(coefficients, std_errors))
}

# Extraire les coefficients et erreurs standard pour chaque modèle
coefficients_A <- get_coefficients(modele_A)
coefficients_B <- get_coefficients(modele_B)
coefficients_C <- get_coefficients(modele_C)

# Créer des vecteurs avec la même longueur pour chaque modèle
max_length <- max(length(coefficients_A[[1]]), length(coefficients_B[[1]]), length(coefficients_C[[1]]))

coefficients_A[[1]] <- c(coefficients_A[[1]], rep(NA, max_length - length(coefficients_A[[1]])))
coefficients_A[[2]] <- c(coefficients_A[[2]], rep(NA, max_length - length(coefficients_A[[2]])))

coefficients_B[[1]] <- c(coefficients_B[[1]], rep(NA, max_length - length(coefficients_B[[1]])))
coefficients_B[[2]] <- c(coefficients_B[[2]], rep(NA, max_length - length(coefficients_B[[2]])))

coefficients_C[[1]] <- c(coefficients_C[[1]], rep(NA, max_length - length(coefficients_C[[1]])))
coefficients_C[[2]] <- c(coefficients_C[[2]], rep(NA, max_length - length(coefficients_C[[2]])))

# Créer le tableau de régression en utilisant data.frame avec des vecteurs de même longueur
tableau_regression <- data.frame(
  Modèle = c("Modèle A", "Modèle B", "Modèle C"),
  Estimateur = c(coefficients_A[[1]], coefficients_B[[1]], coefficients_C[[1]]),
  Erreur_standard = c(coefficients_A[[2]], coefficients_B[[2]], coefficients_C[[2]]),
  P_value = c(summary(modele_A)$coefficients[, "Pr(>|t|)"],
              summary(modele_B)$coefficients[, "Pr(>|t|)"],
              summary(modele_C)$coefficients[, "Pr(>|t|)"])
)

# Afficher le tableau dans la console
print(tableau_regression)





#-------------------------------------------------------------

#                   PARTIE 3
#                                 PARTIE 3

#-------------------------------------------------------------


install.packages("rnaturalearth")
install.packages("leaflet")
install.packages("sp")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("shiny")
install.packages("plotly")
# installation des packages nécessaires

library(sp)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(leaflet)
library(shiny)
library(plotly)

# Chargement des packages necessaires


base_donnee <- read.csv("ACLED-Western_Africa.csv")
# chargement de la base de donnée 

test = subset(base_donnee, pays=="Mali" & type=="Protests" & annee=="2022")
# Création d'un dataframe pour les marqueurs


# Partie création de l'interface utilisateur

ui <- fluidPage(
  
  # titre de l'application
  titlePanel("CARTE SHINY"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="pays",
        label="CHOISIR UN PAYS",
        choices=c(unique(base_donnee$pays)),
        selected = c(unique(base_donnee$pays))[sample(1:length(unique(base_donnee$pays)),1)],
        multiple = TRUE
      ),
      selectInput(
        inputId="evenement",
        label="CHOISIR DES EVENEMENTS",
        choices=c(unique(base_donnee$type)),
        selected = "Protests",
        multiple = TRUE
      ),
      selectInput(
        inputId="annee",
        label="ANNEE DE REALISATION",
        choices=c(unique(base_donnee$annee)),
        selected = "2022",
        multiple = TRUE
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput(outputId="map",
                   width = "100%",
                   height = "720px")
    )
  )
)


# Partie concernant le serveur

server <- function(input, output, session) {
  output$map <- renderPlotly({
    gg <- ggplot() +
      geom_polygon(data = ne_countries(type = "countries",country = c(input$pays)), aes(x = long, y = lat, group = group),
                   fill = "lightblue", color = "gray", alpha = 0.6) +
      #geom_point(data = base, aes(x = longitude, y = latitude),
      #size = 3, alpha = 0.7) +
      theme_void() +
      labs(title = "Carte de l'Afrique de l'Ouest", x = "", y = "") +
      theme(legend.position = "bottom")
    
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)

 





