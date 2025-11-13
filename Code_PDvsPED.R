# ==============================================================================
# 1. PRÉPARATION DU CLASSEMENT D/PD
# ==============================================================================

library(dplyr)
library(ggplot2)

# --- Base de Données Initiale ---
# data_migrants = pums_migrants (Assumant que cette variable contient votre jeu de données brut)
# Renommons POBP pour plus de clarté
data_migrants_initial = pums_migrants %>%
  rename(Lieu_Naissance_Code = POBP)

# --- Codes des Pays en Développement (POBP) ---
# J'utilise la liste étendue que vous avez validée
codes_pays_developpement = c(
  # Amérique Centrale et du Sud
  "100", "101", "102", "103", "104", "105", "106", "107", "108", "109",
  "110", "111", "112", "113", "114", "115", "116", "117", "118", "119",
  "120", "121", "122", "123", "124", "125", "126", "127", "128", "129",
  "130", "131", "132", "133", "134", "135", "136", "137", "138", "139",
  "140", "141", "142", "143", "144", "145", "146", "147", "148",

  # Asie et Moyen-Orient
  "301", "302", "303", "304", "305", "306", "307", "308", "309",
  "310", "311", "312", "313", "314", "315", "316", "317", "318", "319",
  "320", "321", "322", "323", "324", "325", "326", "327", "328", "329",
  "330", "331", "332", "333", "334", "335", "336", "337", "338", "339",
  "340", "341", "342", "343", "344", "345", "346", "347", "348", "349",
  "350", "351", "352", "353", "354", "355", "356", "357", "358", "359",
  "360", "361", "362", "363", "364", "365", "366", "367", "368", "369",
  "370", "371", "372", "373", "374", "375", "376", "377", "378", "379",

  # Afrique
  "400", "401", "402", "403", "404", "405", "406", "407", "408", "409",
  "410", "411", "412", "413", "414", "415", "416", "417", "418", "419",
  "420", "421", "422", "423", "424", "425", "426", "427", "428", "429",
  "430", "431", "432", "433", "434", "435", "436", "437", "438", "439",
  "440", "441", "442", "443", "444", "445", "446", "447", "448", "449",
  "450", "451", "452", "453", "454", "455", "456", "457", "458",

  # Autres régions non développées
  "900", "901", "902", "903", "904", "905", "906", "907", "908", "909",
  "910", "911", "912", "913", "914", "915", "916", "917", "918", "919",
  "920", "921", "922", "923"
)

# ==============================================================================
# 2. CLASSIFICATION ET CALCUL DES EFFECTIFS
# ==============================================================================

df_classification = data_migrants_initial %>%
  # 1. Créer une variable binaire pour la classification
  mutate(
    Type_Pays_Naissance = if_else(
      Lieu_Naissance_Code %in% codes_pays_developpement,
      "Pays_Developpement (PD)",
      "Pays_Developpe (D)")
  ) %>%  # 2. Compter les effectifs pour chaque catégorie
  group_by(Type_Pays_Naissance) %>%
  summarise( Effectif = n(), .groups = 'drop'
  ) %>%
  # 3. Calculer les pourcentages
  mutate(
    Pourcentage = Effectif / sum(Effectif),
    Label_Pourcentage = scales::percent(Pourcentage, accuracy = 0.1)
  )

print(df_classification)


# ==============================================================================
# 3. VISUALISATION DES EFFECTIFS
# ==============================================================================

plot_effectifs = ggplot(df_classification, aes(x = Type_Pays_Naissance, y = Effectif, fill = Type_Pays_Naissance)) +
  geom_bar(stat = "identity", color = "white", width = 0.7) +
  geom_text(aes(label = paste0(scales::comma(Effectif), "\n(", Label_Pourcentage, ")")),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Pays_Developpe (D)" = "#F28E2B", "Pays_Developpement (PD)" = "#4E79A7")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Répartition des Migrants dans la Base Initiale",
    subtitle = "Classification selon le lieu de naissance (POBP)",
    x = "Type de Pays de Naissance",
    y = "Effectif Total (Observations)",
    fill = "Type de Pays"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_blank() # Enlever le titre de l'axe X pour plus de clarté
  )

print(plot_effectifs)
