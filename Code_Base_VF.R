# ==============================================================================
# Analyse du Revenu des Migrants aux États-Unis
# Utilisation de l'objet de données PUMS déjà chargé : pums_migrants
# ==============================================================================

# 1. PRÉPARATION ET CHARGEMENT DES DONNÉES
# ------------------------------------------------------------------------------

# Chargement des packages nécessaires
# library(readr) # Non nécessaire si l'objet est déjà chargé
library(dplyr)
library(ggplot2)
library(scales)

data_migrants <- pums_migrants

# Renommage et Nettoyage
df_clean <- data_migrants %>%
  rename( Revenu_Total = PINCP, Age = AGEP, Sexe = SEX, Niveau_Etude_Code = SCHL, Anglais_Code = ENG
  ) %>%
  # Filtrer les observations non pertinentes (Revenu > 0, Age adulte)
  filter(Revenu_Total > 0, Age >= 16) %>%
  mutate(Sexe_F = factor(Sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
         Anglais_F = factor(Anglais_Code, levels = c(1, 2, 3, 4), labels = c("Très Bien", "Bien", "Pas Bien", "Pas du tout")),
        # Regroupement du Niveau d'Étude (SCHL) en grandes catégories pour l'analyse
    Niveau_Etude_G = case_when(
      Niveau_Etude_Code %in% 1:15 ~ "Moins que BAC",
      Niveau_Etude_Code == 16 ~ "BAC",
      Niveau_Etude_Code %in% 17:19 ~ "Collège/Associate",
      Niveau_Etude_Code %in% 20:24 ~ "Licence ou plus",
      TRUE ~ "Non spécifié"
    ) %>% factor(levels = c("Moins que BAC", "BAC", "Collège/Associate", "Licence ou plus")),

    # Transformation Logarithmique du Revenu Total
    Log_Revenu = log(Revenu_Total))

summary(df_clean)
attach(df_clean)

# ==============================================================================
# 2. STATISTIQUES DESCRIPTIVES (UNIVARIÉES & BIVARIÉES)
# ==============================================================================

cat("\n--- Statistiques Univariées ---\n")

# Statistique Univariée : Sexe
table(Sexe_F) # Effectifs
round(prop.table(table(Sexe_F)), digits = 3) # Fréquences relatives

# Calcul des effectifs
effectifs_sexe <- table(df_clean$Sexe_F)

# Calcul des fréquences relatives (pour les pourcentages)
freq_rel_sexe <- prop.table(effectifs_sexe)

# Préparation des étiquettes incluant les pourcentages
pourcentages <- round(freq_rel_sexe * 100, 1) # Multiplie par 100 et arrondit à 1 décimale
etiquettes <- paste(names(effectifs_sexe), " (", pourcentages, "%)", sep="")

# Création du diagramme circulaire
pie(effectifs_sexe, labels = etiquettes, main = "Répartition des Migrants selon le Sexe",col = c("lightblue", "cornflowerblue") # Choix des couleurs
)

# ==============================================================================
# 2. ANALYSE UNIVARIÉE : ÂGE
# ==============================================================================
summary(df_clean$Age)

# B. Calcul de l'écart-type (mesure de dispersion)
ecart_type_age <- sd(df_clean$Age)
cat("Écart-type de l'Âge :", round(ecart_type_age, 2), "\n")

# C. Histogramme pour la visualisation de la distribution
hist(df_clean$Age,
     main = "Histogramme de la Distribution de l'Âge des Migrants",
     xlab = "Âge (Années)",
     ylab = "Fréquence (Effectifs)",
     col = "skyblue3", # Couleur choisie
     border = "white",
     breaks = 20) # Nombre de classes pour une meilleure visualisation


# --- NIVEAU D'ÉTUDE : Diagramme en Colonnes -----------------------------------------------------------------

# 1. Préparation des données pour ggplot2 (# On crée un dataframe avec les fréquences relatives)
df_educ_freq <- df_clean %>%
  count(Niveau_Etude_G) %>%
  mutate( Frequence_Relative = n / sum(n), Niveau_Etude_G = factor(Niveau_Etude_G, levels = levels(df_clean$Niveau_Etude_G)))

# 2. Définition de la palette de couleurs progressives
nombre_categories <- length(levels(df_clean$Niveau_Etude_G))
palette_ggplot <- hcl.colors(nombre_categories, palette = "DarkMint", rev = TRUE)

# 3. Création du graphique ggplot2 avec ajustement horizontal
plot_niv_educ <- ggplot(df_educ_freq, aes(x = Niveau_Etude_G, y = Frequence_Relative, fill = Niveau_Etude_G)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette_ggplot) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Répartition des Migrants selon le Niveau d'Étude",
    x = "Niveau d'Étude",
    y = "Fréquence relative",
    fill = "Niveau d'Étude"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position = "none")

print(plot_niv_educ)

# --- Statistiques Bivariées : 1. Boxplot : Visualisation de l'effet de l'Anglais sur le Log(Revenu) ----------------------------------------------------------

# Définition de la palette de couleurs progressives pour le boxplot
nombre_categories_anglais <- length(levels(df_clean$Anglais_F))
palette_anglais <- hcl.colors(nombre_categories_anglais, palette = "Mint", rev = TRUE)

plot_boxplot_anglais <- ggplot(df_clean, aes(x = Anglais_F, y = Log_Revenu, fill = Anglais_F)) +
  geom_boxplot() +
  scale_fill_manual(values = palette_anglais) +
  labs(
    title = "Logarithme du Revenu par Niveau de Maîtrise de l'Anglais",
    x = "Niveau d'Anglais",
    y = "Logarithme du Revenu Total"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_anglais)

# 2. Test d'Analyse de la Variance: Mesure de la corrélation

cat("\n--- Test d'Analyse de la Variance (ANOVA) : Effet de Maîtrise de l'Anglais ---\n")

# H0 : Les moyennes de Log_Revenu sont égales pour tous les niveaux d'anglais.
# H1 : Au moins une moyenne de Log_Revenu est différente.
aov_anglais <- aov(Log_Revenu ~ Anglais_F, data = df_clean)
summary(aov_anglais)


# --- STATISTIQUES BIVARIÉES : Log(Revenu) vs. Niveau d'Étude ---
nombre_categories_etude <- length(levels(df_clean$Niveau_Etude_G))
palette_etudes <- hcl.colors(nombre_categories_etude, palette = "Blue-Red", rev = TRUE)

# 2. Création du Boxplot
plot_boxplot_etudes <- ggplot(df_clean, aes(x = Niveau_Etude_G, y = Log_Revenu, fill = Niveau_Etude_G)) +
  geom_boxplot() +
  scale_fill_manual(values = palette_etudes) +
  labs(
    title = "Logarithme du Revenu en fonction du Niveau d'Étude",
    subtitle = "Visualisation du rendement du capital humain",
    x = "Niveau d'Étude (Catégories)",
    y = "Logarithme du Revenu Total"
  ) +
  theme_minimal() +
  # Incliner les labels de l'axe X pour éviter le chevauchement
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") # Retirer la légende redondante

print(plot_boxplot_etudes)

# --- Test d'Analyse de la Variance (ANOVA) pour validation ---

# Ce test valide statistiquement la différence que l'on observe sur le graphique.
aov_etudes <- aov(Log_Revenu ~ Niveau_Etude_G, data = df_clean)
summary(aov_etudes)


# Test de corrélation Chi-2
res_chi2 = chisq.test(tab_sex_eng)
res_chi2
round(res_chi2$residuals^2, 2) # Contributions au Khi-2


# ==============================================================================
# 3. MODÈLE ÉCONOMÉTRIQUE : RÉGRESSION LINÉAIRE
# ==============================================================================

cat("\n--- Modèle de Régression Linéaire : Log(Revenu) ---\n")

# Régression Linéaire Multiple
model_reg <- lm(Log_Revenu ~ Age + Sexe_F + Niveau_Etude_G + Anglais_F, data = df_clean)

# Résultat final
summary(model_reg)
