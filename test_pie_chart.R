# =============================================================================
# SCRIPT TEST : Vérification des pie charts corrigés
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(readxl)
library(ggrepel)
library(here)

# Chargement des données nettoyées
DOSSIER_SORTIES <- here("D:/Avotra/asa/kobo/mpox/projet_mpox/sorties")
DOSSIER_FIGURES <- file.path(DOSSIER_SORTIES, "figures")
if (!dir.exists(DOSSIER_FIGURES)) dir.create(DOSSIER_FIGURES, recursive = TRUE)

df <- read_excel(file.path(DOSSIER_SORTIES, "mpox_nettoye.xlsx"))
message(sprintf("Données chargées : %d observations", nrow(df)))

# Voir les colonnes disponibles
cat("Colonnes disponibles:\n")
print(colnames(df))

# =============================================================================
# FONCTION PIE CORRIGÉE
# =============================================================================

graphique_pie <- function(data, var, titre,
                          couleurs = c("#D62B2B","#27AE60","#2980B9","#E67E22","#95A5A6"),
                          subtitle = NULL) {
  # Nettoyer le titre : enlever "Q# — " au début
  titre_clean <- str_trim(str_remove(titre, "^Q\\d+\\s*[-–—]\\s*"))

  df_pie <- data %>%
    count(.data[[var]]) %>%
    rename(modalite = 1) %>%
    mutate(
      pct       = round(n / sum(n) * 100, 1),
      # Position de l'étiquette au milieu de chaque portion
      pos_label = cumsum(pct) - pct / 2,
      # Étiquette avec modalité, pourcentage et n
      etiquette = paste0(modalite, " : ", pct, "%\n(n=", n, ")")
    ) %>%
    arrange(is.na(modalite)) # NA en dernier pour le remplissage circulaire correct

  # Calculer les positions des étiquettes en coordonnées polaires
  # Dans coord_polar(theta="y"), y représente l'angle (0-100 pour le pct cumulé)
  # et x représente le rayon
  df_labels <- df_pie %>%
    mutate(
      # Rayon externe pour les étiquettes (au-delà du camembert)
      x_label = 1.4,
      # Angle au centre de chaque portion
      y_label = pos_label
    )

  ggplot(df_pie, aes(x = "", y = pct, fill = as.character(modalite))) +
    geom_col(width = 1, colour = "white", linewidth = 0.8) +
    # Ligne de référence (flèche) du camembert vers l'étiquette
    geom_segment(data = df_labels,
                 aes(x = 1, xend = 1.3, y = y_label, yend = y_label),
                 colour = "grey35", linewidth = 0.3, inherit.aes = FALSE) +
    # Étiquettes positionnées à l'extérieur avec ggrepel
    ggrepel::geom_label_repel(
      data = df_labels,
      aes(x = x_label, y = y_label, label = etiquette),
      size = 3, fontface = "bold", colour = "#2C3E50",
      fill = "white", alpha = 0.95,
      box.padding = unit(0.3, "lines"),
      point.padding = unit(0.3, "lines"),
      force = 1,
      inherit.aes = FALSE
    ) +
    # Début du camembert à droite vers gauche (start = -90)
    coord_polar(theta = "y", start = -90) +
    xlim(-0.5, 2) +
    # Couleurs avec transparence pour NA
    scale_fill_manual(
      values = setNames(
        c(couleurs[seq_len(sum(!is.na(df_pie$modalite)))], NA),
        as.character(df_pie$modalite)
      ),
      na.value = NA
    ) +
    labs(title = str_wrap(titre_clean, 60), subtitle = subtitle) +
    theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", colour = "#2C3E50",
                                       size = 13, hjust = 0.5),
          plot.subtitle = element_text(colour = "#666666", hjust = 0.5, size = 9),
          legend.position = "none")
}

# =============================================================================
# EXEMPLES DE TESTS
# =============================================================================

# Trouver une colonne avec des valeurs binaires/catégoriques pour tester
cat("\n✓ Génération de graphiques tests...\n\n")

# Test 1 : Sensibilisation Mpox (si la colonne existe)
if ("mpox_nandre" %in% colnames(df)) {
  p1 <- graphique_pie(
    df %>% mutate(mpox_nandre = recode(as.character(mpox_nandre),
      "Oui" = "A entendu parler du Mpox",
      "Non" = "N'a pas entendu parler",
      "Ne sait pas" = "Ne sait pas")),
    var = "mpox_nandre",
    titre = "Q1 — Sensibilisation au Mpox",
    couleurs = c("#27AE60", "#D62B2B", "#95A5A6")
  )
  print(p1)
  ggsave(file.path(DOSSIER_FIGURES, "test_pie_1_sensibilisation.png"), p1, w = 8, h = 8, dpi = 300)
  cat("✓ Graphique sauvegardé : test_pie_1_sensibilisation.png\n")
}

# Test 2 : Chercher une autre colonne catégorique
categorical_cols <- df %>%
  summarise(across(everything(), ~n_distinct(., na.rm = TRUE))) %>%
  pivot_longer(everything()) %>%
  filter(value > 1 & value <= 6) %>%
  pull(name)

if (length(categorical_cols) > 0) {
  test_col <- categorical_cols[1]
  cat(sprintf("\nTest 2 : Colonne '%s'\n", test_col))

  p2 <- graphique_pie(
    df,
    var = test_col,
    titre = sprintf("Q2 — Test avec %s", test_col),
    couleurs = c("#D62B2B","#27AE60","#2980B9","#E67E22","#95A5A6")
  )
  print(p2)
  ggsave(file.path(DOSSIER_FIGURES, "test_pie_2_autre.png"), p2, w = 8, h = 8, dpi = 300)
  cat(sprintf("✓ Graphique sauvegardé : test_pie_2_autre.png\n"))
}

cat("\n✅ Tests complétés ! Les graphiques ont été sauvegardés dans :\n")
cat(sprintf("   %s\n", DOSSIER_FIGURES))
cat("\nVérifiez les fichiers PNG générés pour voir si :\n")
cat("   ✓ Les étiquettes sont à l'EXTÉRIEUR du camembert\n")
cat("   ✓ Les flèches relient le camembert aux étiquettes\n")
cat("   ✓ Le camembert COMMENCE À DROITE et tourne vers la GAUCHE\n")
cat("   ✓ Les titres N'INCLUENT PAS 'Q1', 'Q2', etc.\n")
cat("   ✓ Les valeurs vides sont TRANSPARENTES (si applicable)\n")
