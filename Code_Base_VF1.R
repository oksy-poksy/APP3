# ==============================================================================
# 1. PRÉPARATION COMPLÈTE ET NETTOYAGE DES DONNÉES
# ==============================================================================

# Chargement des packages nécessaires
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

# Chargement du dataframe original (doit être chargé sous ce nom)
data_migrants = pums_migrants

# Définition des catégories de race (pour la variable RACE_G)
# Nous simplifions les catégories de race pour l'analyse
df_clean = data_migrants %>%
  # --- Renommage des Variables ---
  rename(
    Age = AGEP,                       # Âge de la personne
    Anglais_Code = ENG,               # Maîtrise de l'anglais
    Assurance_Privee = HINS1,         # Statut d'assurance santé privée
    Assurance_Medicaid = HINS2,       # Statut d'assurance santé Medicaid
    Assurance_Medicare = HINS3,       # Statut d'assurance santé Medicare
    Assurance_Militaire = HINS4,      # Statut d'assurance santé Militaire
    Assurance_Indienne = HINS5,       # Statut d'assurance santé Indienne
    Assurance_Etat = HINS6,           # Statut d'assurance santé État/Local
    Assurance_Publique = HINS7,       # Statut d'assurance santé Publique
    Langue_Maison_Code = LANX,        # Parle langue autre qu'anglais à la maison
    Assistance_Publique = PAP,        # Reçoit de l'aide publique
    Scolarisation = SCH,              # Statut de scolarisation (Oui/Non)
    Niveau_Etude_Code = SCHL,         # Code détaillé du plus haut niveau d'étude atteint
    Sexe = SEX,                       # Sexe (1=Homme, 2=Femme)
    Salaire_Annuel = WAGP,            # Salaire annuel et revenus d'emploi
    Heures_Hebdomadaires = WKHP,      # Heures de travail habituelles par semaine
    Statut_Travail = WKL,             # Travail durant l'année (1=Oui, 2=Non)
    Nativite = NATIVITY,              # Nativité (1=Né aux É-U, 2=Étranger/Migrant)
    Revenu_Total = PINCP,             # Revenu total de la personne (Variable Dépendante)
    Lieu_Naissance_Code = POBP,       # Code du lieu de naissance
    Race_Principale_Code = RAC1P,     # Code de la race principale
    # Les autres variables RAC (2P, 3P, Binaires) ne sont pas renommées individuellement
    # mais sont utilisées ci-dessous pour créer une variable de race unique.
  ) %>%

  # --- Filtrage et Création de Variables Dérivées ---
  filter(Revenu_Total > 0, Age >= 16) %>%

  mutate(
    Log_Revenu = log(Revenu_Total),
    Sexe_F = factor(Sexe, levels = c(1, 2), labels = c("Homme", "Femme")), #Vars Sexe factorisée
    Anglais_F = factor(Anglais_Code, levels = c(1, 2, 3, 4), labels = c("Très Bien", "Bien", "Pas Bien", "Pas du tout")), #Variable Anglais factorisée (ENG)
    Niveau_Etude_G = case_when(
      Niveau_Etude_Code %in% 1:15 ~ "Moins que BAC",
      Niveau_Etude_Code == 16 ~ "BAC",
      Niveau_Etude_Code %in% 17:19 ~ "College/Associate",
      Niveau_Etude_Code %in% 20:24 ~ "Licence ou plus",
      TRUE ~ "Non spécifié"
    ) %>% factor(levels = c("Moins que BAC", "BAC", "College/Associate", "Licence ou plus")), # Niveau d'Étude Regroupé (SCHL)
    Statut_Travail_F = factor(Statut_Travail, levels = c(1, 2), labels = c("Travail_Oui", "Travail_Non")), # 5. Statut de Travail (WKL)

    # Race Regroupée (à partir de RAC1P)
    # Les codes 1 à 9 de RAC1P sont regroupés. (Utilisation des codes PUMS)
    Race_G = case_when(
      Race_Principale_Code == 1 ~ "Blanc",
      Race_Principale_Code == 2 ~ "Noir_AfroAmericain",
      Race_Principale_Code == 3 ~ "Amerindien",
      Race_Principale_Code == 4 ~ "Alaska_Natif",
      Race_Principale_Code %in% 5:7 ~ "Asiatique_Pacific", # Regroupe 5, 6, 7
      Race_Principale_Code == 8 ~ "Autre_Race",
      Race_Principale_Code == 9 ~ "Deux_Races_Ou_Plus",
      TRUE ~ "Non_Specifie"
    ) %>% factor(),

    # 7. Autres variables binaires (Oui/Non)
    Assistance_Publique_F = factor(Assistance_Publique, levels = c(0, 1), labels = c("AssistPublic_Non", "AssistPublic_Oui")),
    Scolarisation_F = factor(Scolarisation, levels = c(1, 2), labels = c("Scol_Oui", "Scol_Non"))
  )

summary(df_clean)
attach(df_clean)


# ==============================================================================
# 1. STATS DES, ANALYSE UNIVARIEE - VARIABLES QUANTITATIVES (CONTINUES)
# ==============================================================================

# ==============================================================================
# 1. VARIABLES QUANTITATIVES (CONTINUES)
# ==============================================================================

summary(df_clean$Age)
summary(df_clean$Log_Revenu)
summary(df_clean$Heures_Hebdomadaires)

# --- A. ÂGE (Age) :-----------------------------------------------------------------
plot_age = ggplot(df_clean, aes(x = Age)) +
  geom_histogram(aes(fill = after_stat(count)), binwidth = 5, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "midnightblue") + # Dégradé de bleu
  labs(title = "Distribution de l'Âge des Migrants", x = "Âge (Années)", y = "Fréquence") +
  theme_classic() + # Thème Classique
  theme(legend.position = "none")
print(plot_age)

# --- B. LOG(REVENU) (Log_Revenu) : ----------------------------------------------------
cat("\n--- Graphique : Distribution du Logarithme du Revenu (Dégradé de vert) ---\n")
plot_log_revenu = ggplot(df_clean, aes(x = Log_Revenu)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 50, color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") + # Dégradé de vert
  labs(title = "Distribution du Logarithme du Revenu", x = "Log(Revenu)", y = "Fréquence") +
  theme_bw() + # Thème Noir et Blanc
  theme(legend.position = "none")
print(plot_log_revenu)


# =====================================================================================
# 2. STATS DES, ANALYSE UNIVARIEE : VARIABLES QUALITATIVES (CATÉGORIELLES/FACTEURS)
# =====================================================================================

# Fonction pour créer un graphique en barres avec un thème et une palette personnalisés
make_styled_bar_plot = function(data, variable, titre, palette_name, plot_theme) {
  df_freq = data %>%
    count({{ variable }}) %>%
    mutate(Frequence_Relative = n / sum(n))

  # Définition de la palette en fonction du nombre de catégories
  n_cat = length(unique(df_freq[[deparse(substitute(variable))]]))
  couleurs = hcl.colors(n_cat, palette = palette_name, rev = TRUE)

  plot_bar = ggplot(df_freq, aes(x = {{ variable }}, y = Frequence_Relative, fill = {{ variable }})) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = couleurs) + # Couleurs en dégradé hcl
    scale_y_continuous(labels = percent) +
    labs(title = titre, x = "", y = "Fréquence Relative") +
    plot_theme + # Utilisation du thème passé en argument
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

  print(plot_bar)
  cat("\n--- Fréquences Relatives de", titre, "---\n")
  print(df_freq)
}

# --- A. Sexe (Sexe_F)
make_styled_bar_plot(df_clean, Sexe_F, "Répartition des Migrants selon le Sexe",palette_name = "DarkMint", plot_theme = theme_minimal())

# --- B. Maîtrise de l'Anglais (Anglais_F)
make_styled_bar_plot(df_clean, Anglais_F, "Répartition des Migrants par Maîtrise de l'Anglais", palette_name = "Lajolla", plot_theme = theme_void())

# --- C. Niveau d'Étude Regroupé (Niveau_Etude_G) :
make_styled_bar_plot(df_clean, Niveau_Etude_G, "Répartition par Niveau d'Étude", palette_name = "Sunset", plot_theme = theme_light())

# --- D. Race Regroupée (Race_G) : Palette "Set3"
make_styled_bar_plot(df_clean, Race_G, "Répartition par Race Principale",palette_name = "Set3", plot_theme = ggthemes::theme_tufte()) # Nécessite ggthemes

# --- E. Statut de Travail (Statut_Travail_F) :
make_styled_bar_plot(df_clean, Statut_Travail_F, "Statut de Travail l'Année Dernière", palette_name = "Zissou", plot_theme = theme_dark())

# --- F. Assistance Publique (Assistance_Publique_F) :
make_styled_bar_plot(df_clean, Assistance_Publique_F, "Réception d'Assistance Publique", palette_name = "Inferno", plot_theme = theme_grey())

# --- G. Scolarisation (Scolarisation_F) :
make_styled_bar_plot(df_clean, Scolarisation_F, "Statut de Scolarisation Actuel", palette_name = "Teal", plot_theme = theme_minimal())



# --- H. Fréquences des Variables d'Assurance Santé (HINS1 à HINS7) ---
# Variables et étiquettes pour les assurances
assurance_vars = c("Assurance_Privee", "Assurance_Medicaid", "Assurance_Medicare", "Assurance_Militaire", "Assurance_Indienne", "Assurance_Etat", "Assurance_Publique")
assurance_labels = c("Assurance Santé Privée", "Assurance Santé Medicaid", "Assurance Santé Medicare", "Assurance Santé Militaire", "Assurance Santé Indienne", "Assurance Santé État/Local", "Assurance Santé Publique (VA/TRICARE)")

# Palettes à utiliser (pour la variation visuelle)
palettes = c("Mint", "Plasma", "Teal", "Blue-Red", "Emrld", "DarkMint", "Viridis")

# Fonction pour créer les graphs
make_insurance_bar_plot = function(data, variable_name, titre, palette_name) {
  # Calcule les fréquences et s'assure que les labels sont clairs (1=Oui, 2=Non)
  df_freq = data %>%
    count(!!sym(variable_name)) %>%
    mutate(
      Statut = factor(!!sym(variable_name), levels = c(1, 2), labels = c("Oui", "Non")),
      Frequence_Relative = n / sum(n)
    )

  # Définition de la palette en dégradé pour Oui/Non
  couleurs = hcl.colors(2, palette = palette_name, rev = TRUE)

  plot_bar = ggplot(df_freq, aes(x = Statut, y = Frequence_Relative, fill = Statut)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = couleurs) +
    scale_y_continuous(labels = percent) +
    labs(title = paste("Répartition du Statut d'Assurance :", titre), x = "Statut", y = "Fréquence Relative") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 10))

  print(plot_bar)
  cat("\n--- Fréquences Relatives de", titre, "---\n")
  print(df_freq %>% select(Statut, Frequence_Relative))}

# Génération des graphiques
for (i in seq_along(assurance_vars)) {
  make_insurance_bar_plot(df_clean, assurance_vars[i], assurance_labels[i], palettes[i])}



# ==============================================================================
# ANALYSE BIVARIEE: LOG(REVENU) vs. ÂGE
# ==============================================================================

# Calcul de la limite supérieure pour le Revenu Total brut (pour le titre)
limite_y_brut = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE)

# Échantillonnage des points pour la performance
set.seed(42) # Pour reproductibilité
n_sample = min(nrow(df_clean), 5000)
df_sample = df_clean %>% sample_n(n_sample)

# Utilisation de suppressWarnings pour masquer le message par défaut de geom_smooth
suppressWarnings({
  plot_age_revenu_opt = ggplot(df_clean, aes(x = Age, y = Log_Revenu)) +
    # Points du sous-échantillon
    geom_point(data = df_sample, alpha = 0.3, size = 0.6, color = "gray30") +

    # Courbe de tendance OPTIMISÉE (Régression polynomiale d'ordre 2)
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 2), # Formule quadratique explicite pour la forme en U inversé
                se = TRUE,
                color = "darkred",
                linewidth = 1.5,
                fill = "salmon",
                alpha = 0.3) +

    labs(
      title = "Log(Revenu) en fonction de l'Âge (Modèle Quadratique)",
      subtitle = paste("Tendance calculée par régression polynomiale. N points tracés =", n_sample),
      x = "Âge (Années)",
      y = "Log(Revenu Total)"
    ) +
    theme_bw()

  print(plot_age_revenu_opt)
})


# ==============================================================================
# 2.1. LOG(REVENU) vs. NIVEAU D'ÉTUDE
# ==============================================================================

nombre_categories_etude = length(levels(df_clean$Niveau_Etude_G))
palette_etudes = hcl.colors(nombre_categories_etude, palette = "Sunset", rev = TRUE)

plot_boxplot_etudes = ggplot(df_clean, aes(x = Niveau_Etude_G, y = Log_Revenu, fill = Niveau_Etude_G)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + # outlier.shape=NA masque les valeurs extrêmes pour la clarté
  scale_fill_manual(values = palette_etudes) +
  labs(title = "Rendement de l'Éducation sur le Log(Revenu)", x = "Niveau d'Étude", y = "Logarithme du Revenu Total") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_etudes)

# ==============================================================================
# 2.2. LOG(REVENU) vs. MAÎTRISE DE L'ANGLAIS
# ==============================================================================

nombre_categories_anglais = length(levels(df_clean$Anglais_F))
palette_anglais = hcl.colors(nombre_categories_anglais, palette = "Mint", rev = TRUE)

plot_boxplot_anglais = ggplot(df_clean, aes(x = Anglais_F, y = Log_Revenu, fill = Anglais_F)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_anglais) +
  labs(title = "Rendement Linguistique sur le Log(Revenu)", x = "Maîtrise de l'Anglais", y = "Logarithme du Revenu Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_anglais)

# ==============================================================================
# 2.3. LOG(REVENU) vs. SEXE
# ==============================================================================

palette_sexe = c("skyblue", "pink")

plot_boxplot_sexe = ggplot(df_clean, aes(x = Sexe_F, y = Log_Revenu, fill = Sexe_F)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_sexe) +
  labs(title = "Écart de Log(Revenu) selon le Sexe", x = "Sexe", y = "Logarithme du Revenu Total") +
  theme_bw() +
  theme(legend.position = "none")

print(plot_boxplot_sexe)


# ==============================================================================
# 2.4. LOG(REVENU) vs. RACE (ANOVA & Boxplot)
# ==============================================================================

nombre_categories_race = length(levels(df_clean$Race_G))
palette_race = hcl.colors(nombre_categories_race, palette = "Plasma", rev = TRUE)

plot_boxplot_race = ggplot(df_clean, aes(x = Race_G, y = Log_Revenu, fill = Race_G)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_race) +
  labs(title = "Log(Revenu) selon la Race Principale", x = "Catégorie de Race", y = "Logarithme du Revenu Total") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(plot_boxplot_race)

# ==============================================================================
# 2.5 LOG(REVENU) vs. ASSISTANCE PUBLIQUE (ANOVA & Boxplot) un peu inutile
# ==============================================================================

palette_assistance = c("gray50", "firebrick")

plot_boxplot_ap = ggplot(df_clean, aes(x = Assistance_Publique_F, y = Log_Revenu, fill = Assistance_Publique_F)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_assistance) +
  labs(title = "Log(Revenu) selon la Réception d'Assistance Publique", x = "Reçoit de l'aide publique?", y = "Logarithme du Revenu Total") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_boxplot_ap)

# ==============================================================================
# 2.6. LOG(REVENU) vs. HEURES HEBDOMADAIRES (Nuage de points)
# ==============================================================================

# Échantillonnage des points pour la performance
set.seed(42)
n_sample_heures = min(nrow(df_clean), 5000)
df_sample_heures = df_clean %>% sample_n(n_sample_heures)

plot_heures_revenu = ggplot(df_clean, aes(x = Heures_Hebdomadaires, y = Log_Revenu)) +
  # Points du sous-échantillon
  geom_point(data = df_sample_heures, alpha = 0.3, size = 0.6, color = "darkblue") +

  # Courbe de tendance (Régression Linéaire simple)
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "darkgreen", linewidth = 1.5, fill = "lightgreen", alpha = 0.3) +

  labs(
    title = "Log(Revenu) en fonction des Heures Hebdomadaires",
    subtitle = paste("Tendance linéaire. N points tracés =", n_sample_heures),
    x = "Heures de Travail Hebdomadaires",
    y = "Log(Revenu Total)"
  ) +
  theme_bw()

print(plot_heures_revenu)
# Afficher la corrélation de Pearson
cor_heures_revenu = cor(df_clean$Heures_Hebdomadaires, df_clean$Log_Revenu, use = "complete.obs")
cat(paste0("\nCoefficient de corrélation (Pearson) : ", round(cor_heures_revenu, 3), "\n"))

# ==============================================================================
# 2.7. LOG(REVENU) vs. ASSURANCE
# ==============================================================================

# --- Création d'une variable binaire agrégée pour l'assurance ---
df_clean = df_clean %>%
  mutate(
    A_Assurance_Sante = if_else(
      # HINS1 ou HINS2 ou HINS3 ou HINS4 ou HINS5 ou HINS6 ou HINS7 == 1 (Oui)
      Assurance_Privee == 1 | Assurance_Medicaid == 1 | Assurance_Medicare == 1 |
        Assurance_Militaire == 1 | Assurance_Indienne == 1 | Assurance_Etat == 1 |
        Assurance_Publique == 1,
      "Assure_Oui",
      "Assure_Non"
    ) %>% factor()
  )

palette_assurance_totale = c("thistle", "cornflowerblue")

plot_boxplot_assurance = ggplot(df_clean, aes(x = A_Assurance_Sante, y = Log_Revenu, fill = A_Assurance_Sante)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_assurance_totale) +
  labs(title = "Log(Revenu) selon le Statut d'Assurance Santé", x = "Possède une Assurance Santé?", y = "Log(Revenu Total)") +
  theme_bw() +
  theme(legend.position = "none")

print(plot_boxplot_assurance)



# ==============================================================================
# 3. QUALITATIF vs. QUALITATIF : SEXE vs. NIVEAU D'ÉTUDE (TEST DU CHI-2)
# ==============================================================================

# 1. Tableau de Contingence
tab_sex_etude = table(Sexe_F, Niveau_Etude_G)
addmargins(tab_sex_etude)

# 2. Profils-Lignes (Fréquence du niveau d'étude conditionnel au sexe)
tab_PL_etude = prop.table(tab_sex_etude, 1)
round(tab_PL_etude, digits = 3)

# 3. Graphique des Profils-Lignes
barplot(t(tab_PL_etude), beside = TRUE,
        main = "Niveau d'Étude selon le Sexe (Profils-Lignes)",
        xlab = "Sexe", ylab = "Fréquence relative", ylim = c(0, 1.0),
        col = hcl.colors(ncol(tab_PL_etude), palette = "Blue-Red"), # Palette en dégradé
        legend.text = TRUE)

# 4. Test de Corrélation Chi-2
# H0 : Les variables Sexe et Niveau d'Étude sont indépendantes.
res_chi2_etude = chisq.test(tab_sex_etude)
res_chi2_etude


# ==============================================================================
# 2. MODÈLE ÉCONOMÉTRIQUE COMPLET
# ==============================================================================

cat("\n--- Modèle de Régression Linéaire : Log(Revenu) Complet ---\n")

# Régression Linéaire Multiple avec toutes les variables factorielles créées
# L'inclusion de 'Statut_Travail_F' pourrait être endogène, mais est incluse pour utiliser la variable.
# 'Heures_Hebdomadaires' est incluse comme variable continue.

model_reg_complet = lm(
  Log_Revenu ~
    Age +
    Sexe_F +
    Niveau_Etude_G +
    Anglais_F +
    Race_G +
    Statut_Travail_F +
    Heures_Hebdomadaires +
    Assistance_Publique_F +
    Scolarisation_F,
  data = df_clean
)

# Résultat final du modèle
summary(model_reg_complet)

