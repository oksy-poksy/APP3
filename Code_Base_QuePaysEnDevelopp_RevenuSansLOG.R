# ==============================================================================
# 1. PRÉPARATION COMPLÈTE ET NETTOYAGE DES DONNÉES
# ==============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

data_migrants = pums_migrants

# ----------------------------------------------------------------------
# DEFINITION ÉTENDUE DES CODES PAYS EN DEVELOPPEMENT (POBP)
# ----------------------------------------------------------------------

codes_pays_developpement = c(
  # Amérique Centrale et du Sud (Exemples étendus)
  "100", "101", "102", "103", "104", "105", "106", "107", "108", "109",
  "110", "111", "112", "113", "114", "115", "116", "117", "118", "119",
  "120", "121", "122", "123", "124", "125", "126", "127", "128", "129",
  "130", "131", "132", "133", "134", "135", "136", "137", "138", "139",
  "140", "141", "142", "143", "144", "145", "146", "147", "148",

  # Asie et Moyen-Orient (Exemples étendus)
  "301", "302", "303", "304", "305", "306", "307", "308", "309",
  "310", "311", "312", "313", "314", "315", "316", "317", "318", "319",
  "320", "321", "322", "323", "324", "325", "326", "327", "328", "329",
  "330", "331", "332", "333", "334", "335", "336", "337", "338", "339",
  "340", "341", "342", "343", "344", "345", "346", "347", "348", "349",
  "350", "351", "352", "353", "354", "355", "356", "357", "358", "359",
  "360", "361", "362", "363", "364", "365", "366", "367", "368", "369",
  "370", "371", "372", "373", "374", "375", "376", "377", "378", "379",

  # Afrique (Exemples étendus)
  "400", "401", "402", "403", "404", "405", "406", "407", "408", "409",
  "410", "411", "412", "413", "414", "415", "416", "417", "418", "419",
  "420", "421", "422", "423", "424", "425", "426", "427", "428", "429",
  "430", "431", "432", "433", "434", "435", "436", "437", "438", "439",
  "440", "441", "442", "443", "444", "445", "446", "447", "448", "449",
  "450", "451", "452", "453", "454", "455", "456", "457", "458",

  # Exemples d'autres régions non développées (codes divers)
  "900", "901", "902", "903", "904", "905", "906", "907", "908", "909",
  "910", "911", "912", "913", "914", "915", "916", "917", "918", "919",
  "920", "921", "922", "923"
)


df_clean = data_migrants %>%
  # Renommage des Variables
  rename(
    Age = AGEP, # Âge de la personne
    Anglais_Code = ENG,# Maîtrise de l'anglais
    Assurance_Privee = HINS1,# Statut d'assurance santé privée
    Assurance_Medicaid = HINS2,# Statut d'assurance santé Medicaid
    Assurance_Medicare = HINS3,# Statut d'assurance santé Medicare
    Assurance_Militaire = HINS4, # Statut d'assurance santé Militaire
    Assurance_Indienne = HINS5,# Statut d'assurance santé Indienne
    Assurance_Etat = HINS6,# Statut d'assurance santé État/Local
    Assurance_Publique = HINS7,# Statut d'assurance santé Publique
    Langue_Maison_Code = LANX, # Parle langue autre qu'anglais à la maison
    Assistance_Publique = PAP, # Reçoit de l'aide publique
    Scolarisation = SCH,# Statut de scolarisation (Oui/Non)
    Niveau_Etude_Code = SCHL,# Code détaillé du plus haut niveau d'étude atteint
    Sexe = SEX,# Sexe (1=Homme, 2=Femme)
    Salaire_Annuel = WAGP, # Salaire annuel et revenus d'emploi
    Heures_Hebdomadaires = WKHP, # Heures de travail habituelles par semaine
    Statut_Travail = WKL,# Travail durant l'année (1=Oui, 2=Non)
    Nativite = NATIVITY, # Nativité (1=Né aux É-U, 2=Étranger/Migrant)
    Revenu_Total = PINCP,# Revenu total de la personne (Variable Dépendante)
    Lieu_Naissance_Code = POBP,# Code du lieu de naissance
    Race_Principale_Code = RAC1P,# Code de la race principale

  ) %>%

  # --- Filtrage CRUCIAL : Pays en Développement ---
  filter(Revenu_Total > 0, Age >= 16) %>%
  filter(Lieu_Naissance_Code %in% codes_pays_developpement) %>% # <<< FILTRAGE PAR PAYS EN DÉVELOPPEMENT

  mutate(
    # Log_Revenu = log(Revenu_Total), # Supprimé ou commenté
    Sexe_F = factor(Sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
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
    Assistance_Publique_F = factor(Assistance_Publique, levels = c(1, 2), labels = c("AssistPublic_Non", "AssistPublic_Oui")),
    Scolarisation_F = factor(Scolarisation, levels = c(0, 1), labels = c("Scol_Oui", "Scol_Non"))
  )

# IMPORTANT : Re-attach pour que les fonctions qui suivent utilisent le df_clean filtré
attach(df_clean)

cat(paste0("Taille de l'échantillon filtré (Migrants Pays en Développement) : N = ", nrow(df_clean), "\n"))


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

# texte d'annotation pour la légende
stats_text = paste0(
  "Moyenne: ", round(moyenne, 1), " ans\n",
  "Médiane: ", round(mediane, 1), " ans\n",
  "Q1: ", round(q1, 1), " ans\n",
  "Q3: ", round(q3, 1), " ans"
)


# --- 2. Graphique avec Annotations ---
plot_age_annotated = ggplot(df_clean, aes(x = Age)) +
  geom_histogram(aes(fill = after_stat(count)), binwidth = 5, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "midnightblue") +

  # Ligne verticale pour la Moyenne (Rouge)
  geom_vline(xintercept = moyenne, linetype = "dashed", color = "red", linewidth = 1) +
  # Ligne verticale pour la Médiane (Jaune)
  geom_vline(xintercept = mediane, linetype = "solid", color = "yellow", linewidth = 1) +

  # Annotation Textuelle des Statistiques
  annotate("text",
           x = max(df_clean$Age) * 0.85, # Position X (ex: 85% de l'âge max)
           y = max(ggplot_build(ggplot(df_clean, aes(x=Age)) +
                                  geom_histogram(binwidth = 5))$data[[1]]$count) * 0.9, # Position Y (ex: 90% de la fréquence max)
           label = stats_text,
           hjust = 0, vjust = 1,
           size = 4,
           color = "gray10") +

  labs(title = "Distribution de l'Âge des Migrants (Pays en Développement)",
       subtitle = "Médiane (jaune) et Moyenne (rouge pointillé)",
       x = "Âge (Années)",
       y = "Fréquence") +
  theme_classic() +
  theme(legend.position = "none")

print(plot_age_annotated)

# --- B REVENUu ------------------------------------------------------------------------------

# Calcul de la limite à 99%
limite_x = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE)

cat(paste0("NOTE: L'axe des X est tronqué à ", format(limite_x, big.mark = ","), " $ (99ème percentile) pour la lisibilité.\n"))

plot_revenu_brut = ggplot(df_clean, aes(x = Revenu_Total)) +
  # Histogramme
  geom_histogram(aes(fill = after_stat(count)), bins = 100, color = "white") +
  scale_fill_gradient(low = "lightcoral", high = "darkred") + # Dégradé de rouge

  # Utilisation du coord_cartesian
  coord_cartesian(xlim = c(0, limite_x)) +

  # Formatage de l'axe X en milliers de dollars
  scale_x_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  labs(
    title = "Distribution du Revenu Total (Brut) (Migrants PD)",
    subtitle = paste("Concentration extrême des données à gauche (asymétrie positive). N =", nrow(df_clean)),
    x = "Revenu Total Annuel (en milliers de $)",
    y = "Fréquence"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_revenu_brut)


# --- C. LOG(REVENU) (Log_Revenu) : ----------------------------------------------------
# Le log revenu n'est plus utilisé comme variable principale mais le graphique est conservé pour la comparaison.
# summary(df_clean$Log_Revenu) # Commenté

# plot_log_revenu = ggplot(df_clean, aes(x = Log_Revenu)) +
# geom_histogram(aes(fill = after_stat(count)), bins = 50, color = "white") +
# scale_fill_gradient(low = "lightgreen", high = "darkgreen") + # Dégradé de vert
# labs(title = "Distribution du Logarithme du Revenu (Migrants PD)", x = "Log(Revenu)", y = "Fréquence") +
# theme_bw() + # Thème Noir et Blanc
# theme(legend.position = "none")
# print(plot_log_revenu)


# B. SALAIRE ANNUEL (WAGP) : ------------------------------------------------------------------

# --- 1. Filtrage et Statistiques

df_salaire_positif = df_clean %>% filter(Salaire_Annuel > 0) # Filtre pour les salaires positifs
print(summary(df_clean$Salaire_Annuel)) # Statistiques sur l'ensemble des données
print(summary(df_salaire_positif$Salaire_Annuel))

n_zeros = sum(df_clean$Salaire_Annuel == 0, na.rm = TRUE) # Calcul de la proportion de salaires nuls
n_total = nrow(df_clean)
prop_zeros = n_zeros / n_total

cat(paste0("\nNombre d'observations avec un Salaire Annuel de 0 : ", n_zeros, " (", round(prop_zeros * 100, 2), "% du total)\n"))

limite_x = quantile(df_salaire_positif$Salaire_Annuel, 0.99, na.rm = TRUE) # Calcul de la limite à 99% pour la troncature visuelle

cat(paste0("NOTE: L'axe des X est tronqué à ", format(limite_x, big.mark = ","), " $ (99ème percentile) pour la lisibilité.\n"))

plot_salaire_brut = ggplot(df_salaire_positif, aes(x = Salaire_Annuel)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 100, color = "white") +
  scale_fill_gradient(low = "#CCCCFF", high = "#5500AA") +

  # Troncature de l'axe X (limite visuelle)
  coord_cartesian(xlim = c(0, limite_x)) +
  # Formatage de l'axe X en milliers de dollars
  scale_x_continuous(labels = scales::dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  labs(
    title = "Distribution du Salaire Annuel (WAGP) (Salaires > 0) (Migrants PD)",
    subtitle = paste("Forte asymétrie positive. L'axe X est tronqué au 99e percentile (", format(limite_x, big.mark = ","), "$)"),
    x = "Salaire Annuel (en milliers de $)",
    y = "Fréquence"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_salaire_brut)


limite_x = quantile(df_salaire_positif$Salaire_Annuel, 0.95, na.rm = TRUE)

plot_hist_salaire = ggplot(df_salaire_positif, aes(x = Salaire_Annuel)) +
  # Utilisation de la densité pour comparer différentes tailles d'échantillons
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "darkblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) + # Ajout de la courbe de densité
  # Limite l'axe X pour la clarté
  coord_cartesian(xlim = c(0, limite_x)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution du Salaire Annuel (WAGP) (Salaires > 0) (Migrants PD)",
    subtitle = paste("L'axe des X est tronqué au 95ème percentile pour la lisibilité"),
    x = "Salaire Annuel (USD)",
    y = "Densité"
  ) +
  theme_minimal()

print(plot_hist_salaire)


# ==============================================================================
# ANALYSE BIVARIEE: REVENU TOTAL vs. ÂGE
# ==============================================================================

# NOTE IMPORTANTE : Utiliser un modèle polynomial sur le revenu brut est délicat
# en raison de la forte asymétrie. Nous conservons la méthode mais l'interprétation
# du R² et de l'âge optimal est moins fiable que sur le log-revenu.

modele_quadratique_age = lm(Revenu_Total ~ poly(Age, 2, raw = TRUE), data = df_clean) # Changement ici
summary_modele = summary(modele_quadratique_age)

# Extraire les valeurs clés
r_carre_ajuste = summary_modele$adj.r.squared
coeff_age = modele_quadratique_age$coefficients["poly(Age, 2, raw = TRUE)1"]
coeff_age_carre = modele_quadratique_age$coefficients["poly(Age, 2, raw = TRUE)2"]

age_optimal = - (coeff_age / (2 * coeff_age_carre)) # Calcul de l'Âge Optimal (Sommet de la courbe)

# Création du texte d'annotation
stats_text_modele = paste0(
  "Âge optimal: ", round(age_optimal, 1), " ans\n",
  "R² Ajusté: ", round(r_carre_ajuste, 3))

# Définir la limite Y pour le graphique (troncature visuelle)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE)

# ______ graphique _____
set.seed(42)
n_sample = min(nrow(df_clean), 5000)
df_sample = df_clean %>% sample_n(n_sample)

x_max = max(df_clean$Age, na.rm = TRUE) # Calculer les coordonnées maximales pour le positionnement en bas à droite
y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici


suppressWarnings({
  plot_age_revenu_final = ggplot(df_clean, aes(x = Age, y = Revenu_Total)) + # Changement ici
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
             x = x_max * 0.98, # Ancrage près de l'extrême droite (98%)
             y = y_min + 1000, # Ancrage près de l'extrême bas (ajusté pour l'échelle du revenu brut)
             label = stats_text_modele,
             hjust = 1,# Alignement horizontal à droite
             vjust = 0,# Alignement vertical en bas
             size = 4,
             color = "black") +
    # Troncature de l'axe Y pour la lisibilité
    coord_cartesian(ylim = c(0, limite_y_revenu)) +
    # Formatage de l'axe Y en milliers de dollars
    scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +
    labs(
      title = "Revenu Total vs. Âge (Courbe de Mincer) (Migrants PD)",
      subtitle = "Le revenu atteint son maximum à l'âge optimal (ligne pointillée). Axe Y tronqué au 99e percentile.",
      x = "Âge (Années)",
      y = "Revenu Total Annuel (en milliers de $)" # Changement ici
    ) +
    theme_bw()

  print(plot_age_revenu_final)
})


# ==============================================================================
# 2.1. REVENU TOTAL vs. NIVEAU D'ÉTUDE
# ==============================================================================


# Test de Kruskal-Wallis (test non-paramétrique)
# H0: pas de différence significative dans la distribution du Revenu Total entre les différents niveaux d'étude (Niveau_Etude_G) des migrants.
kruskal_test_etude = kruskal.test(Revenu_Total ~ Niveau_Etude_G, data = df_clean) # Changement ici

chi_square = kruskal_test_etude$statistic # Extraction des valeurs clés
p_value = kruskal_test_etude$p.value

p_value_text = paste0("p-value: ", format.pval(p_value, digits = 3, eps = 0.001)) # Formater la p-value
if (p_value < 0.001) {p_value_text = "p-value < 0.001 (***)"}

# Création du texte d'annotation
stats_text_kruskal = paste0("Test de Kruskal-Wallis :\n","\u03C7\u00B2 = ", round(chi_square, 2), " (ddl = ", kruskal_test_etude$parameter, ")\n", p_value_text)

y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = length(levels(df_clean$Niveau_Etude_G)) # Nombre de catégories (pour l'axe X)

# Définir la limite Y pour le graphique (troncature visuelle)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE)


# ==============================================================================

nombre_categories_etude = length(levels(df_clean$Niveau_Etude_G))
palette_etudes = hcl.colors(nombre_categories_etude, palette = "Sunset", rev = TRUE)

plot_boxplot_etudes_annotated = ggplot(df_clean, aes(x = Niveau_Etude_G, y = Revenu_Total, fill = Niveau_Etude_G)) + # Changement ici
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_etudes) +

  # Troncature de l'axe Y pour la lisibilité
  coord_cartesian(ylim = c(0, limite_y_revenu)) +
  # Formatage de l'axe Y en milliers de dollars
  scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max, # Position X (sur la dernière catégorie)
           y = y_min + 1, # Position Y (juste au-dessus du minimum, ajusté pour l'échelle)
           label = stats_text_kruskal,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +
  labs(
    title = "Rendement de l'Éducation sur le Revenu Total (Migrants PD)", # Changement ici
    subtitle = "La tendance centrale du Revenu Total est significativement différente selon le niveau d'étude. Axe Y tronqué au 99e percentile.",
    x = "Niveau d'Étude",
    y = "Revenu Total Annuel (en milliers de $)" # Changement ici
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_etudes_annotated)




# ==============================================================================
# 2.2. REVENU TOTAL vs. MAÎTRISE DE L'ANGLAIS
# ==============================================================================

# Test de Kruskal-Wallis
# H0: pas de différence significative dans la distribution du Revenu Total entre les différentes catégories de maîtrise de l'anglais des migrants.
kruskal_test_anglais = kruskal.test(Revenu_Total ~ Anglais_F, data = df_clean) # Changement ici

chi_square_anglais = kruskal_test_anglais$statistic
p_value_anglais = kruskal_test_anglais$p.value
ddl_anglais = kruskal_test_anglais$parameter

p_value_text_anglais = paste0("p-value: ", format.pval(p_value_anglais, digits = 3, eps = 0.001))
if (p_value_anglais < 0.001) {p_value_text_anglais = "p-value < 0.001 (***)"}

stats_text_kruskal_anglais = paste0("Test de Kruskal-Wallis :\n", "\u03C7\u00B2 = ", round(chi_square_anglais, 2), " (ddl = ", ddl_anglais, ")\n", p_value_text_anglais)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = length(levels(df_clean$Anglais_F)) # Nombre de catégories (pour l'axe X)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE) # Définir la limite Y pour le graphique (troncature visuelle)


# ==============================================================================

nombre_categories_anglais = length(levels(df_clean$Anglais_F))
palette_anglais = hcl.colors(nombre_categories_anglais, palette = "Mint", rev = TRUE)

plot_boxplot_anglais_annotated = ggplot(df_clean, aes(x = Anglais_F, y = Revenu_Total, fill = Anglais_F)) + # Changement ici
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_anglais) +
  # Troncature de l'axe Y pour la lisibilité
  coord_cartesian(ylim = c(0, limite_y_revenu)) +
  # Formatage de l'axe Y en milliers de dollars
  scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +
  annotate("text",
           x = x_max,# Position X (sur la dernière catégorie)
           y = y_min + 1, # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_kruskal_anglais,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +
  labs(
    title = "Rendement Linguistique sur le Revenu Total (Migrants PD)", # Changement ici
    subtitle = "La tendance centrale du Revenu Total est significativement différente selon la maîtrise de l'Anglais. Axe Y tronqué au 99e percentile.",
    x = "Maîtrise de l'Anglais",
    y = "Revenu Total Annuel (en milliers de $)" # Changement ici
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")

print(plot_boxplot_anglais_annotated)



# ==============================================================================
# 2.3. REVENU TOTAL vs. SEXE
# ==============================================================================

# Test t de Student
# H0: La moyenne du Revenu Total des hommes est égale à la moyenne du Revenu Total des femmes parmi les migrants.
t_test_sexe = t.test(Revenu_Total ~ Sexe_F, data = df_clean) # Changement ici

t_statistic = t_test_sexe$statistic
p_value_sexe = t_test_sexe$p.value
ddl_sexe = t_test_sexe$parameter

p_value_text_sexe = paste0("p-value: ", format.pval(p_value_sexe, digits = 3, eps = 0.001))
if (p_value_sexe < 0.001) {p_value_text_sexe = "p-value < 0.001 (***)"}

# Création du texte d'annotation
stats_text_t_test = paste0("Test t de Student :\n", "t = ", round(t_statistic, 2), " (ddl = ", round(ddl_sexe, 0), ")\n",p_value_text_sexe)

y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = length(levels(df_clean$Sexe_F)) # Nombre de catégories (2 pour Hommes/Femmes)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE) # Définir la limite Y pour le graphique (troncature visuelle)

# ==============================================================================

palette_sexe = c("skyblue", "pink")

plot_boxplot_sexe_annotated = ggplot(df_clean, aes(x = Sexe_F, y = Revenu_Total, fill = Sexe_F)) + # Changement ici
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_sexe) +
  # Troncature de l'axe Y pour la lisibilité
  coord_cartesian(ylim = c(0, limite_y_revenu)) +
  # Formatage de l'axe Y en milliers de dollars
  scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +
  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max,# Position X (sur la dernière catégorie : "Femme")
           y = y_min + 1, # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_t_test,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +
  labs(
    title = "Écart de Revenu Total selon le Sexe (Migrants PD)", # Changement ici
    subtitle = "La différence de moyenne de Revenu Total entre les sexes est hautement significative. Axe Y tronqué au 99e percentile.",
    x = "Sexe",
    y = "Revenu Total Annuel (en milliers de $)" # Changement ici
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_boxplot_sexe_annotated)


# ==============================================================================
# 2.4. REVENU TOTAL vs. Ethnie
# ==============================================================================

# H0: La distribution de Revenu_Total est la même dans toutes les catégories de Race.
kruskal_test_race = kruskal.test(Revenu_Total ~ Race_G, data = df_clean) # Changement ici

chi_square_race = kruskal_test_race$statistic
p_value_race = kruskal_test_race$p.value
ddl_race = kruskal_test_race$parameter

# Formater la p-value
p_value_text_race = paste0("p-value: ", format.pval(p_value_race, digits = 3, eps = 0.001))
if (p_value_race < 0.001) {p_value_text_race = "p-value < 0.001 (extra faible)"}

stats_text_kruskal_race = paste0("Test de Kruskal-Wallis :\n","\u03C7\u00B2 = ", round(chi_square_race, 2), " (ddl = ", ddl_race, ")\n",p_value_text_race)

y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = length(levels(df_clean$Race_G))
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE) # Définir la limite Y pour le graphique (troncature visuelle)

# ==============================================================================

nombre_categories_race = length(levels(df_clean$Race_G))
palette_race = hcl.colors(nombre_categories_race, palette = "Plasma", rev = TRUE)

plot_boxplot_race_annotated = ggplot(df_clean, aes(x = Race_G, y = Revenu_Total, fill = Race_G)) + # Changement ici
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_race) +

  # Troncature de l'axe Y pour la lisibilité
  coord_cartesian(ylim = c(0, limite_y_revenu)) +
  # Formatage de l'axe Y en milliers de dollars
  scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max,# Position X (sur la dernière catégorie)
           y = y_min + 1, # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_kruskal_race,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Revenu Total selon l'Ethnie Principale (Migrants PD)", # Changement ici
    subtitle = "La différence de distribution de Revenu Total selon la Race est hautement significative. Axe Y tronqué au 99e percentile.",
    x = "Catégorie d'Ethnie",
    y = "Revenu Total Annuel (en milliers de $)" # Changement ici
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(plot_boxplot_race_annotated)


# ==============================================================================
# 2.5. REVENU TOTAL vs. ASSISTANCE PUBLIQUE (Code Corrigé)
# ==============================================================================

# --- ASSURER 2 NIVEAUX DANS LE FACTEUR (Code de la réponse précédente) ---
df_clean_ap_valid = df_clean %>%
  filter(Assistance_Publique %in% c(1, 2)) %>%
  mutate(Assistance_Publique_F = factor(Assistance_Publique_F, levels = c("AssistPublic_Non", "AssistPublic_Oui")))

# Test t de Student
t_test_assistance_pub = t.test(Revenu_Total ~ Assistance_Publique_F, data = df_clean_ap_valid) # Changement ici

# Extraction des valeurs clés
t_statistic_ap = t_test_assistance_pub$statistic
p_value_ap = t_test_assistance_pub$p.value
ddl_ap = t_test_assistance_pub$parameter

# Formater la p-value
p_value_text_ap = paste0("p-value: ", format.pval(p_value_ap, digits = 3, eps = 0.001))
if (p_value_ap < 0.001) {
  p_value_text_ap = "p-value < 0.001 (***)"
}

# Création du texte d'annotation
stats_text_t_test_ap = paste0(
  "Test t de Student :\n",
  "t = ", round(t_statistic_ap, 2), " (ddl = ", round(ddl_ap, 0), ")\n",
  p_value_text_ap
)

# Calcul des coordonnées pour le coin inférieur droit
y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = length(levels(df_clean$Assistance_Publique_F)) # Nombre de catégories (2)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE) # Définir la limite Y pour le graphique (troncature visuelle)

# ==============================================================================

palette_assistance = c("gray50", "firebrick")

plot_boxplot_ap_annotated = ggplot(df_clean_ap_valid, aes(x = Assistance_Publique_F, y = Revenu_Total, fill = Assistance_Publique_F)) + # Changement ici
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_assistance) +

  # Troncature de l'axe Y pour la lisibilité
  coord_cartesian(ylim = c(0, limite_y_revenu)) +
  # Formatage de l'axe Y en milliers de dollars
  scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max,# Position X (sur la dernière catégorie)
           y = y_min + 1, # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_t_test_ap,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(title = "Revenu Total vs. Assistance Publique (Migrants PD)", # Changement ici
       subtitle = "La différence de moyenne de Revenu Total entre les groupes est hautement significative. Axe Y tronqué au 99e percentile.",
       x = "Reçoit de l'aide publique?",
       y = "Revenu Total Annuel (en milliers de $)") # Changement ici
theme_minimal() +
  theme(legend.position = "none")

print(plot_boxplot_ap_annotated)


# ==============================================================================
# 2.6. REVENU TOTAL vs. HEURES HEBDOMADAIRES (Nuage de points)
# ==============================================================================

# Test de Corrélation de Pearson
cor_test_heures = cor.test(df_clean$Heures_Hebdomadaires, df_clean$Revenu_Total, use = "complete.obs") # Changement ici

r_coefficient = cor_test_heures$estimate
p_value_cor = cor_test_heures$p.value
t_stat = cor_test_heures$statistic
ddl_cor = cor_test_heures$parameter


p_value_text_cor = paste0("p-value: ", format.pval(p_value_cor, digits = 3, eps = 0.001))
if (p_value_cor < 0.001) {p_value_text_cor = "p-value < 0.001 (***)"}

stats_text_pearson = paste0("Test de Corrélation de Pearson :\n","r = ", round(r_coefficient, 3), " (Force de l'association)\n",p_value_text_cor)

y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = max(df_clean$Heures_Hebdomadaires, na.rm = TRUE)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE) # Définir la limite Y pour le graphique (troncature visuelle)


# ==============================================================================

set.seed(42)
n_sample_heures = min(nrow(df_clean), 5000) # Échantillonnage des points pour la performance
df_sample_heures = df_clean %>% sample_n(n_sample_heures)

suppressWarnings({
  plot_heures_revenu_annotated = ggplot(df_clean, aes(x = Heures_Hebdomadaires, y = Revenu_Total)) + # Changement ici
    # Points du sous-échantillon
    geom_point(data = df_sample_heures, alpha = 0.3, size = 0.6, color = "darkblue") +
    # Courbe de tendance (Régression Linéaire simple)
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "darkgreen", linewidth = 1.5, fill = "lightgreen", alpha = 0.3) +
    # Troncature de l'axe Y pour la lisibilité
    coord_cartesian(ylim = c(0, limite_y_revenu)) +
    # Formatage de l'axe Y en milliers de dollars
    scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +
    annotate("text",
             x = x_max * 0.98,# Ancrage près de l'extrême droite
             y = y_min + 1, # Position Y (juste au-dessus du minimum de l'axe)
             label = stats_text_pearson,
             hjust = 1, vjust = 0,# Alignement à droite et en bas
             size = 4,
             color = "gray10") +
    labs(
      title = "Revenu Total vs. Heures Hebdomadaires (Migrants PD)", # Changement ici
      subtitle = paste("Corrélation positive modérée et hautement significative. Axe Y tronqué au 99e percentile. N points tracés =", n_sample_heures),
      x = "Heures de Travail Hebdomadaires",
      y = "Revenu Total Annuel (en milliers de $)" # Changement ici
    ) +
    theme_bw()

  print(plot_heures_revenu_annotated)
})



# ==============================================================================
# 2.7. REVENU TOTAL vs. ASSURANCE
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
t_test_assurance = t.test(Revenu_Total ~ A_Assurance_Sante, data = df_clean) # Changement ici

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
y_min = min(df_clean$Revenu_Total, na.rm = TRUE) # Changement ici
x_max = length(levels(df_clean$A_Assurance_Sante)) # Nombre de catégories (2)
limite_y_revenu = quantile(df_clean$Revenu_Total, 0.99, na.rm = TRUE) # Définir la limite Y pour le graphique (troncature visuelle)

# ==============================================================================

palette_assurance_totale = c("lightblue4", "lightblue")

plot_boxplot_assurance_annotated = ggplot(df_clean, aes(x = A_Assurance_Sante, y = Revenu_Total, fill = A_Assurance_Sante)) + # Changement ici
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = palette_assurance_totale) +

  # Troncature de l'axe Y pour la lisibilité
  coord_cartesian(ylim = c(0, limite_y_revenu)) +
  # Formatage de l'axe Y en milliers de dollars
  scale_y_continuous(labels = dollar_format(prefix="$", scale=1e-3, suffix="K")) +

  # Ajout des résultats du test statistique en BAS A DROITE
  annotate("text",
           x = x_max, # Position X (sur la dernière catégorie)
           y = y_min + 1, # Position Y (juste au-dessus du minimum de l'axe)
           label = stats_text_t_test_assurance,
           hjust = 1, vjust = 0, # Alignement à droite et en bas
           size = 4,
           color = "gray10") +

  labs(
    title = "Revenu Total vs. Statut d'Assurance Santé (Migrants PD)", # Changement ici
    subtitle = "La différence de moyenne de Revenu Total entre les groupes est hautement significative. Axe Y tronqué au 99e percentile.",
    x = "Possède une Assurance Santé?",
    y = "Revenu Total Annuel (en milliers de $)" # Changement ici
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(plot_boxplot_assurance_annotated)

