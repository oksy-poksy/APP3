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
      Race_Principale_Code == 8 ~ "Autre_Ethnie",
      Race_Principale_Code == 9 ~ "Deux_Ethnies_Ou_Plus",
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

# --- A. ÂGE (Age) :-----------------------------------------------------------------
summary(df_clean$Age)

# --- 1. Calcul des Stats Clés ---
stats_age = summary(df_clean$Age)
moyenne = stats_age["Mean"]
mediane = stats_age["Median"]
q1 = stats_age["1st Qu."]
q3 = stats_age["3rd Qu."]

# Création du texte d'annotation pour la légende
# Utilise paste0 pour regrouper les statistiques importantes
stats_text = paste0(
  "Moyenne: ", round(moyenne, 1), " ans\n",
  "Médiane: ", round(mediane, 1), " ans\n",
  "Q1: ", round(q1, 1), " ans\n",
  "Q3: ", round(q3, 1), " ans"
)


# --- 2. Génération du Graphique avec Annotations ---
plot_age_annotated = ggplot(df_clean, aes(x = Age)) +
  geom_histogram(aes(fill = after_stat(count)), binwidth = 5, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "midnightblue") +

  # Ligne verticale pour la Moyenne (Rouge)
  geom_vline(xintercept = moyenne, linetype = "dashed", color = "red", linewidth = 1) +
  # Ligne verticale pour la Médiane (Verte)
  geom_vline(xintercept = mediane, linetype = "solid", color = "yellow", linewidth = 1) +

  # Annotation Textuelle des Statistiques
  # Positionné dans le coin supérieur droit (ou adapté selon la distribution)
  annotate("text",
           x = max(df_clean$Age) * 0.85, # Position X (ex: 85% de l'âge max)
           y = max(ggplot_build(ggplot(df_clean, aes(x=Age)) +
                                  geom_histogram(binwidth = 5))$data[[1]]$count) * 0.9, # Position Y (ex: 90% de la fréquence max)
           label = stats_text,
           hjust = 0, vjust = 1,
           size = 4,
           color = "gray10",
           fontface = "bold") +

  labs(title = "Distribution de l'Âge des Migrants",
       subtitle = "Médiane (jaune) et Moyenne (rouge pointillé)",
       x = "Âge (Années)",
       y = "Fréquence") +
  theme_classic() +
  theme(legend.position = "none")

print(plot_age_annotated)

# --- B REVENUu ------------------------------------------------------------------------------

# Calcul de la limite à 99% pour la troncature visuelle
# Le 99ème percentile est un bon compromis pour visualiser l'essentiel des données.
limite_x = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE)

cat(paste0("NOTE: L'axe des X est tronqué à ", format(limite_x, big.mark = ","), " $ (99ème percentile) pour la lisibilité.\n"))

plot_revenu_brut = ggplot(df_clean, aes(x = Revenu_Total)) +
  # Histogramme en rouge pour le contraste
  geom_histogram(aes(fill = after_stat(count)), bins = 100, color = "white") +
  scale_fill_gradient(low = "lightcoral", high = "darkred") + # Dégradé de rouge

  # Utilisation du coord_cartesian pour tronquer l'affichage sans filtrer les données
  coord_cartesian(xlim = c(0, limite_x)) +

  # Formatage de l'axe X en milliers de dollars
  scale_x_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  labs(
    title = "Distribution du Revenu Total (Brut)",
    subtitle = paste("Concentration extrême des données à gauche (asymétrie positive). N =", nrow(df_clean)),
    x = "Revenu Total Annuel (en milliers de $)",
    y = "Fréquence"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_revenu_brut)



# --- C. LOG(REVENU) (Log_Revenu) : ----------------------------------------------------
summary(df_clean$Log_Revenu)

plot_log_revenu = ggplot(df_clean, aes(x = Log_Revenu)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 50, color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") + # Dégradé de vert
  labs(title = "Distribution du Logarithme du Revenu", x = "Log(Revenu)", y = "Fréquence") +
  theme_bw() + # Thème Noir et Blanc
  theme(legend.position = "none")
print(plot_log_revenu)


# B. SALAIRE ANNUEL (WAGP) : Distribution asymétrique---------------------------------------

# --- 1. Filtrage et Statistiques (À exécuter une seule fois) ---

# Filtre pour les salaires positifs (pour l'histogramme et les stats positives)
df_salaire_positif = df_clean %>% filter(Salaire_Annuel > 0)

# Statistiques sur l'ensemble des données (pour voir l'impact des zéros)
cat("\n--- Statistiques pour le Salaire Annuel (Tous les N) ---\n")
print(summary(df_clean$Salaire_Annuel))

# Statistiques sur les salaires positifs uniquement
cat("\n--- Statistiques pour les Salaires > 0 ---\n")
print(summary(df_salaire_positif$Salaire_Annuel))

# Calcul de la proportion de salaires nuls
n_zeros = sum(df_clean$Salaire_Annuel == 0, na.rm = TRUE)
n_total = nrow(df_clean)
prop_zeros = n_zeros / n_total

cat(paste0("\nNombre d'observations avec un Salaire Annuel de 0 : ", n_zeros, " (", round(prop_zeros * 100, 2), "% du total)\n"))


# Calcul de la limite à 99% pour la troncature visuelle
limite_x = quantile(df_salaire_positif$Salaire_Annuel, 0.99, na.rm = TRUE)

cat(paste0("NOTE: L'axe des X est tronqué à ", format(limite_x, big.mark = ","), " $ (99ème percentile) pour la lisibilité.\n"))

plot_salaire_brut = ggplot(df_salaire_positif, aes(x = Salaire_Annuel)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 100, color = "white") +
  scale_fill_gradient(low = "#CCCCFF", high = "#5500AA") +

  # Troncature de l'axe X (limite visuelle)
  coord_cartesian(xlim = c(0, limite_x)) +
  # Formatage de l'axe X en milliers de dollars
  scale_x_continuous(labels = scales::dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  labs(
    title = "Distribution du Salaire Annuel (WAGP) (Salaires > 0)",
    subtitle = paste("Forte asymétrie positive. L'axe X est tronqué au 99e percentile (", format(limite_x, big.mark = ","), "$)"),
    x = "Salaire Annuel (en milliers de $)",
    y = "Fréquence"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_salaire_brut)


limite_x = quantile(df_salaire_positif$Salaire_Annuel, 0.95, na.rm = TRUE)

plot_hist_salaire = ggplot(df_salaire_positif, aes(x = Salaire_Annuel)) +
  # Utilisation de la densité pour comparer différentes tailles d'échantillons si besoin
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "darkblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) + # Ajout de la courbe de densité
  # Limite l'axe X pour la clarté
  coord_cartesian(xlim = c(0, limite_x)) +
  scale_x_continuous(labels = comma) + # Formatage des nombres
  labs(
    title = "Distribution du Salaire Annuel (WAGP) (Salaires > 0)",
    subtitle = paste("L'axe des X est tronqué au 95ème percentile pour la lisibilité"),
    x = "Salaire Annuel (USD)",
    y = "Densité"
  ) +
  theme_minimal()

print(plot_hist_salaire)


# ==============================================================================
# ANALYSE BIVARIEE: LOG(REVENU) vs. ÂGE
# ==============================================================================

modele_quadratique_age = lm(Log_Revenu ~ poly(Age, 2, raw = TRUE), data = df_clean)
summary_modele = summary(modele_quadratique_age)

# Extraire les valeurs clés
r_carre_ajuste = summary_modele$adj.r.squared
coeff_age = modele_quadratique_age$coefficients["poly(Age, 2, raw = TRUE)1"]
coeff_age_carre = modele_quadratique_age$coefficients["poly(Age, 2, raw = TRUE)2"]

# Calcul de l'Âge Optimal (Sommet de la courbe)
age_optimal = - (coeff_age / (2 * coeff_age_carre))

# Création du texte d'annotation
stats_text_modele = paste0(
  "Âge optimal: ", round(age_optimal, 1), " ans\n",
  "R² Ajusté: ", round(r_carre_ajuste, 3)
)

# ______ graphique _____
set.seed(42)
n_sample = min(nrow(df_clean), 5000)
df_sample = df_clean %>% sample_n(n_sample)

# Calculer les coordonnées maximales pour le positionnement en bas à droite
x_max = max(df_clean$Age, na.rm = TRUE)
y_min = min(df_clean$Log_Revenu, na.rm = TRUE)


suppressWarnings({
  plot_age_revenu_final = ggplot(df_clean, aes(x = Age, y = Log_Revenu)) +
    # Points du sous-échantillon
    geom_point(data = df_sample, alpha = 0.3, size = 0.6, color = "gray30") +

    # Courbe de tendance quadratique
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 2),
                se = TRUE,
                color = "darkred",
                linewidth = 1.5,
                fill = "salmon",
                alpha = 0.3) +

    # Ligne verticale pour l'Âge Optimal
    geom_vline(xintercept = age_optimal, linetype = "dotted", color = "red", linewidth = 1.2) +

    # Annotation des résultats statistiques (Positionnée en BAS A DROITE)
    annotate("text",
             x = x_max * 0.98,        # Ancrage près de l'extrême droite (98%)
             y = y_min * 1.05,        # Ancrage près de l'extrême bas (légèrement au-dessus du minimum)
             label = stats_text_modele,
             hjust = 1,                 # Alignement horizontal à droite
             vjust = 0,                 # Alignement vertical en bas
             size = 4,
             color = "black") +

    labs(
      title = "Log(Revenu) en fonction de l'Âge (Courbe de Mincer)",
      subtitle = "Le revenu atteint son maximum à l'âge optimal (ligne pointillée)",
      x = "Âge (Années)",
      y = "Log(Revenu Total)"
    ) +
    theme_bw()

  print(plot_age_revenu_final)
})


# ==============================================================================
# 2.1. LOG(REVENU) vs. NIVEAU D'ÉTUDE
# ==============================================================================

# Test de Kruskal-Wallis
"Le test de Kruskal-Wallis est un test statistique non
paramétrique utilisé pour déterminer s'il existe une différence
significative dans la tendance centrale (distribution ou médiane)
d'une variable quantitative (continue ou ordinale)
entre trois groupes indépendants ou plus.
"
kruskal_test_etude = kruskal.test(Log_Revenu ~ Niveau_Etude_G, data = df_clean)

# Extraction des valeurs clés
chi_square = kruskal_test_etude$statistic
p_value = kruskal_test_etude$p.value

# Formater la p-value
p_value_text = paste0("p-value: ", format.pval(p_value, digits = 3, eps = 0.001))
if (p_value < 0.001) {
  p_value_text = "p-value < 0.001 (extra faible)"
}

# Création du texte d'annotation
stats_text_kruskal = paste0(
  "Test de Kruskal-Wallis :\n",
  "\u03C7\u00B2 = ", round(chi_square, 2), " (ddl = ", kruskal_test_etude$parameter, ")\n",
  p_value_text
)

y_min = min(df_clean$Log_Revenu, na.rm = TRUE)
x_max = length(levels(df_clean$Niveau_Etude_G)) # Nombre de catégories (pour l'axe X)


# ==============================================================================

nombre_categories_etude = length(levels(df_clean$Niveau_Etude_G))
palette_etudes = hcl.colors(nombre_categories_etude, palette = "Sunset", rev = TRUE)

plot_boxplot_etudes_annotated = ggplot(df_clean, aes(x = Niveau_Etude_G, y = Log_Revenu, fill = Niveau_Etude_G)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_etudes) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max, # Position X (sur la dernière catégorie)
           y = y_min + 1, # Position Y (juste au-dessus du minimum)
           label = stats_text_kruskal,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Rendement de l'Éducation sur le Log(Revenu)",
    subtitle = "La tendance centrale du Log(Revenu) est significativement différente selon le niveau d'étude.",
    x = "Niveau d'Étude",
    y = "Log(Revenu Total)"
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_etudes_annotated)




# ==============================================================================
# 2.2. LOG(REVENU) vs. MAÎTRISE DE L'ANGLAIS
# ==============================================================================

# Test de Kruskal-Wallis
kruskal_test_anglais = kruskal.test(Log_Revenu ~ Anglais_F, data = df_clean)

# Extraction des valeurs clés
chi_square_anglais = kruskal_test_anglais$statistic
p_value_anglais = kruskal_test_anglais$p.value
ddl_anglais = kruskal_test_anglais$parameter

# Formater la p-value
p_value_text_anglais = paste0("p-value: ", format.pval(p_value_anglais, digits = 3, eps = 0.001))
if (p_value_anglais < 0.001) {
  p_value_text_anglais = "p-value < 0.001 (extra faible)"
}

# Création du texte d'annotation
stats_text_kruskal_anglais = paste0(
  "Test de Kruskal-Wallis :\n",
  "\u03C7\u00B2 = ", round(chi_square_anglais, 2), " (ddl = ", ddl_anglais, ")\n",
  p_value_text_anglais
)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Log_Revenu, na.rm = TRUE)
x_max = length(levels(df_clean$Anglais_F)) # Nombre de catégories (pour l'axe X)


# ==============================================================================

nombre_categories_anglais = length(levels(df_clean$Anglais_F))
palette_anglais = hcl.colors(nombre_categories_anglais, palette = "Mint", rev = TRUE)

plot_boxplot_anglais_annotated = ggplot(df_clean, aes(x = Anglais_F, y = Log_Revenu, fill = Anglais_F)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_anglais) +


  annotate("text",
           x = x_max,         # Position X (sur la dernière catégorie)
           y = y_min + 1,     # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_kruskal_anglais,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Rendement Linguistique sur le Log(Revenu)",
    subtitle = "La tendance centrale du Log(Revenu) est significativement différente selon la maîtrise de l'Anglais.",
    x = "Maîtrise de l'Anglais",
    y = "Logarithme du Revenu Total"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_anglais_annotated)



# ==============================================================================
# 2.3. LOG(REVENU) vs. SEXE
# ==============================================================================

# Test t de Student
t_test_sexe = t.test(Log_Revenu ~ Sexe_F, data = df_clean)

# Extraction des valeurs clés
t_statistic = t_test_sexe$statistic
p_value_sexe = t_test_sexe$p.value
ddl_sexe = t_test_sexe$parameter

# Formater la p-value
p_value_text_sexe = paste0("p-value: ", format.pval(p_value_sexe, digits = 3, eps = 0.001))
if (p_value_sexe < 0.001) {
  p_value_text_sexe = "p-value < 0.001 (***)"
}

# Création du texte d'annotation
stats_text_t_test = paste0(
  "Test t de Student :\n",
  "t = ", round(t_statistic, 2), " (ddl = ", round(ddl_sexe, 0), ")\n",
  p_value_text_sexe
)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Log_Revenu, na.rm = TRUE)
x_max = length(levels(df_clean$Sexe_F)) # Nombre de catégories (2 pour Hommes/Femmes)

# ==============================================================================

palette_sexe = c("skyblue", "pink")

plot_boxplot_sexe_annotated = ggplot(df_clean, aes(x = Sexe_F, y = Log_Revenu, fill = Sexe_F)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_sexe) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max,         # Position X (sur la dernière catégorie : "Femme")
           y = y_min + 1,     # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_t_test,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Écart de Log(Revenu) selon le Sexe",
    subtitle = "La différence de moyenne de Log(Revenu) entre les sexes est hautement significative.",
    x = "Sexe",
    y = "Logarithme du Revenu Total"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_boxplot_sexe_annotated)


# ==============================================================================
# 2.4. LOG(REVENU) vs. Ethnie
# ==============================================================================

# H0: La distribution de Log_Revenu est la même dans toutes les catégories de Race.
kruskal_test_race = kruskal.test(Log_Revenu ~ Race_G, data = df_clean)

# Extraction des valeurs clés
chi_square_race = kruskal_test_race$statistic
p_value_race = kruskal_test_race$p.value
ddl_race = kruskal_test_race$parameter

# Formater la p-value
p_value_text_race = paste0("p-value: ", format.pval(p_value_race, digits = 3, eps = 0.001))
if (p_value_race < 0.001) {
  p_value_text_race = "p-value < 0.001 (extra faible)"
}


# Création du texte d'annotation
stats_text_kruskal_race = paste0(
  "Test de Kruskal-Wallis :\n",
  "\u03C7\u00B2 = ", round(chi_square_race, 2), " (ddl = ", ddl_race, ")\n",
  p_value_text_race
)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Log_Revenu, na.rm = TRUE)
x_max = length(levels(df_clean$Race_G))

# ==============================================================================

nombre_categories_race = length(levels(df_clean$Race_G))
palette_race = hcl.colors(nombre_categories_race, palette = "Plasma", rev = TRUE)

plot_boxplot_race_annotated = ggplot(df_clean, aes(x = Race_G, y = Log_Revenu, fill = Race_G)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_race) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max,         # Position X (sur la dernière catégorie)
           y = y_min + 1,     # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_kruskal_race,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Log(Revenu) selon l'Ethnie Principale",
    subtitle = "La différence de distribution de Log(Revenu) selon la Race est hautement significative.",
    x = "Catégorie d'Ethnie",
    y = "Log(Revenu Total)"
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(plot_boxplot_race_annotated)



# ==============================================================================
# 2.5 LOG(REVENU) vs. ASSISTANCE PUBLIQUE
# ==============================================================================

palette_assistance = c("gray50", "firebrick")

plot_boxplot_ap = ggplot(df_clean, aes(x = Assistance_Publique_F, y = Log_Revenu, fill = Assistance_Publique_F)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_assistance) +
  labs(title = "Log(Revenu) selon la Réception d'Assistance Publique", x = "Reçoit de l'aide publique?", y = "Logarithme du Revenu Total") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_boxplot_ap)

# Test de Kruskal-Wallis : Log(Revenu) vs. Race ___________________________________

# H0: La distribution de Log_Revenu est la même dans tous les groupes de race.
kruskal_test_race = kruskal.test(Log_Revenu ~ Race_G, data = df_clean)
print(kruskal_test_race)


# ==============================================================================
# 2.6. LOG(REVENU) vs. HEURES HEBDOMADAIRES (Nuage de points)
# ==============================================================================

# Test de Corrélation de Pearson
cor_test_heures = cor.test(df_clean$Heures_Hebdomadaires, df_clean$Log_Revenu, use = "complete.obs")

# Extraction des valeurs clés
r_coefficient = cor_test_heures$estimate
p_value_cor = cor_test_heures$p.value
t_stat = cor_test_heures$statistic
ddl_cor = cor_test_heures$parameter

# Formater la p-value
p_value_text_cor = paste0("p-value: ", format.pval(p_value_cor, digits = 3, eps = 0.001))
if (p_value_cor < 0.001) {
  p_value_text_cor = "p-value < 0.001 (***)"
}

# Création du texte d'annotation
stats_text_pearson = paste0(
  "Test de Corrélation de Pearson :\n",
  "r = ", round(r_coefficient, 3), " (Force de l'association)\n",
  p_value_text_cor
)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Log_Revenu, na.rm = TRUE)
x_max = max(df_clean$Heures_Hebdomadaires, na.rm = TRUE)


# ==============================================================================
# 2. GÉNÉRATION DU GRAPHIQUE ANNOTÉ (Bas Droit)
# ==============================================================================

# Échantillonnage des points pour la performance
set.seed(42)
n_sample_heures = min(nrow(df_clean), 5000)
df_sample_heures = df_clean %>% sample_n(n_sample_heures)

suppressWarnings({
  plot_heures_revenu_annotated = ggplot(df_clean, aes(x = Heures_Hebdomadaires, y = Log_Revenu)) +

    # Points du sous-échantillon
    geom_point(data = df_sample_heures, alpha = 0.3, size = 0.6, color = "darkblue") +

    # Courbe de tendance (Régression Linéaire simple)
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "darkgreen", linewidth = 1.5, fill = "lightgreen", alpha = 0.3) +

    # Ajout des résultats du test statistique en BAS A DROITE
    annotate("text",
             x = x_max * 0.98,         # Ancrage près de l'extrême droite
             y = y_min + 1,            # Position Y (juste au-dessus du minimum de l'axe)
             label = stats_text_pearson,
             hjust = 1, vjust = 0,     # Alignement à droite et en bas
             size = 4,
             color = "gray10") +

    labs(
      title = "Log(Revenu) en fonction des Heures Hebdomadaires",
      subtitle = paste("Corrélation positive modérée et hautement significative. N points tracés =", n_sample_heures),
      x = "Heures de Travail Hebdomadaires",
      y = "Log(Revenu Total)"
    ) +
    theme_bw()

  print(plot_heures_revenu_annotated)
})

# Test de Corrélation de Pearson : Log(Revenu) vs. Heures Hebdomadaires ________________________
"Définition du Test de Corrélation de Pearson
Le Test de Corrélation de Pearson est une méthode statistique utilisée
pour évaluer la force et la direction d'une relation linéaire
entre deux variables quantitatives (ou continues).

Il permet de répondre à la question :
Dans quelle mesure les variations d'une variable sont-elles associées
aux variations de l'autre variable,
et cette association est-elle significative ?"





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

# Test t de Student
t_test_assurance = t.test(Log_Revenu ~ A_Assurance_Sante, data = df_clean)

# Extraction des valeurs clés
t_statistic = t_test_assurance$statistic
p_value_assurance = t_test_assurance$p.value
ddl_assurance = t_test_assurance$parameter

# Formater la p-value
p_value_text_assurance = paste0("p-value: ", format.pval(p_value_assurance, digits = 3, eps = 0.001))
if (p_value_assurance < 0.001) {
  p_value_text_assurance = "p-value < 0.001 (***)"
}

# Création du texte d'annotation
stats_text_t_test_assurance = paste0(
  "Test t de Student :\n",
  "t = ", round(t_statistic, 2), " (ddl = ", round(ddl_assurance, 0), ")\n",
  p_value_text_assurance
)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Log_Revenu, na.rm = TRUE)
x_max = length(levels(df_clean$A_Assurance_Sante)) # Nombre de catégories (2)

# ==============================================================================

palette_assurance_totale = c("thistle", "cornflowerblue")

plot_boxplot_assurance_annotated = ggplot(df_clean, aes(x = A_Assurance_Sante, y = Log_Revenu, fill = A_Assurance_Sante)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_assurance_totale) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max,         # Position X (sur la dernière catégorie)
           y = y_min + 1,     # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_t_test_assurance,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Log(Revenu) selon le Statut d'Assurance Santé",
    subtitle = "La différence de moyenne de Log(Revenu) entre les groupes est hautement significative.",
    x = "Possède une Assurance Santé?",
    y = "Log(Revenu Total)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_boxplot_assurance_annotated)
