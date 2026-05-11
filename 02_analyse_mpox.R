# =============================================================================
# SCRIPT 02 : ANALYSE ET RAPPORT - ENQUÊTE KAP MPOX
# =============================================================================
# Description : Analyses et visualisations selon le plan d'analyse KAP Mpox
#               (CRM Madagascar, 2026). Sections : Connaissance, Perception,
#               Pratiques, Accès aux soins, Communication, Vaccination,
#               Satisfaction, Stigmatisation, Engagement communautaire.
# Auteur      : Analyste de données - CRM Madagascar
# Prérequis   : Exécuter d'abord le script 01_nettoyage_mpox.R
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES (tous chargés au début)
# -----------------------------------------------------------------------------
{
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(stringr)
  library(janitor)
  library(writexl)
  library(readxl)     # Lecture fichiers Excel
  library(here)
  library(patchwork)
  library(ggtext)     # Texte enrichi dans les titres ggplot2
  library(ggrepel)
}
    # Étiquettes positionnées de façon optimale
# install.packages(c("patchwork","purrr","ggtext","ggrepel","readxl")) si nécessaire

# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES DONNÉES NETTOYÉES
# -----------------------------------------------------------------------------
DOSSIER_SORTIES <- here("D:/Avotra/asa/kobo/mpox/projet_mpox/sorties")
DOSSIER_FIGURES <- file.path(DOSSIER_SORTIES, "figures")
if (!dir.exists(DOSSIER_FIGURES)) dir.create(DOSSIER_FIGURES, recursive = TRUE)

df <- read_excel(file.path(DOSSIER_SORTIES, "mpox_nettoye.xlsx"))
message(sprintf("Données chargées : %d observations", nrow(df)))

# -----------------------------------------------------------------------------
# 2. FONCTIONS UTILITAIRES
# -----------------------------------------------------------------------------

#' Tableau de fréquences (choix unique)
tableau_freq <- function(data, var, label = var) {
  data %>%
    count(.data[[var]], name = "n") %>%
    filter(!is.na(.data[[var]])) %>%
    mutate(pct = round(n / sum(n) * 100, 1), variable = label) %>%
    rename(modalite = 1) %>%
    select(variable, modalite, n, pct)
}

#' Pourcentage d'une variable binaire (0/1)
pct_bin <- function(data, var) round(mean(data[[var]], na.rm = TRUE) * 100, 1)

#' Graphique en barres horizontal standard (questions à choix multiples)
graphique_barres <- function(df_freq, titre, couleur = "#D62B2B", subtitle = NULL) {
  ggplot(df_freq,
    aes(x = pct, y = fct_reorder(str_wrap(as.character(modalite), 38), pct))) +
    geom_col(fill = couleur, alpha = 0.85, width = 0.65) +
    geom_text(aes(label = paste0(pct, "%")), hjust = -0.15, size = 3.5, fontface = "bold") +
    scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
    labs(title = str_wrap(titre, 65), subtitle = subtitle, x = "Pourcentage (%)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", colour = "#2C3E50", size = 12),
          plot.subtitle = element_text(colour = "#666666", size = 9),
          axis.text.y = element_text(colour = "#2C3E50"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank())
}

#' Diagramme camembert — bordures épaisses, légende avec % et bordure rectangle
graphique_pie <- function(data, var, titre,
                          couleurs = c("#D62B2B","#27AE60","#2980B9","#E67E22","#95A5A6"),
                          subtitle = NULL) {
  titre_clean <- str_trim(str_remove(titre, "^Q\\d+\\s*[-–—]\\s*"))

  df_pie <- data %>%
    count(.data[[var]]) %>%
    rename(modalite = 1) %>%
    mutate(
      pct       = round(n / sum(n) * 100, 1),
      pos_label = cumsum(pct) - pct / 2,
      legende   = paste0(modalite, "  —  ", pct, "%")
    ) %>%
    arrange(is.na(modalite))

  n_cat <- sum(!is.na(df_pie$modalite))
  couleurs_map <- setNames(c(couleurs[seq_len(n_cat)], NA),
                           as.character(df_pie$modalite))

  ggplot(df_pie, aes(x = "", y = pct,
                     fill = as.character(modalite))) +
    geom_col(width = 1, colour = "#333333", linewidth = 1.4) +
    coord_polar(theta = "y", start = -90) +
    scale_fill_manual(
      values   = couleurs_map,
      na.value = NA,
      labels   = setNames(df_pie$legende, as.character(df_pie$modalite)),
      name     = NULL
    ) +
    labs(title = str_wrap(titre_clean, 60), subtitle = subtitle) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", colour = "#2C3E50",
                                   size = 13, hjust = 0.5),
      plot.subtitle = element_text(colour = "#666666", hjust = 0.5, size = 9),
      legend.position  = "bottom",
      legend.text      = element_text(size = 11, colour = "#2C3E50", face = "bold"),
      legend.key.size  = unit(0.8, "cm"),
      legend.spacing.x = unit(0.5, "cm"),
      legend.box.just  = "center",
      legend.background = element_rect(fill = "white", colour = "#333333", linewidth = 1.2),
      legend.title = element_blank(),
      legend.margin = margin(t = 12, r = 18, b = 12, l = 18),
      legend.spacing.y = unit(3, "pt")
    )
}

#' Sauvegarder un graphique en PNG
sauvegarder <- function(plot, nom, w = 10, h = 6) {
  chemin <- file.path(DOSSIER_FIGURES, paste0(nom, ".png"))
  ggsave(chemin, plot = plot, width = w, height = h, dpi = 180, bg = "white")
  message(sprintf("  Sauvegardé : %s", basename(chemin)))
  invisible(chemin)
}

#' Donut chart (variante élégante du pie, avec chiffre central et légende)
#' Idéal pour une seule valeur clé à mettre en avant (ex: % Oui)
#'
#' @param pct_oui   Pourcentage de la modalité principale (0-100)
#' @param label_oui Libellé de la modalité principale
#' @param titre     Titre du graphique
#' @param couleur   Couleur de la tranche principale
graphique_donut <- function(pct_oui, label_oui, titre,
                            couleur = "#D62B2B", subtitle = NULL) {
  pct_reste <- 100 - pct_oui
  df_d <- tibble(
    cat = c(label_oui, "Reste"),
    pct = c(pct_oui, pct_reste),
    legende = c(paste0(label_oui, "  —  ", pct_oui, "%"),
                paste0("Reste  —  ", pct_reste, "%"))
  )

  ggplot(df_d, aes(x = 2, y = pct, fill = cat)) +
    geom_col(width = 1, colour = "white", linewidth = 0.8) +
    coord_polar(theta = "y", start = 0) +
    scale_fill_manual(values = c(setNames(couleur, label_oui),
                                 "Reste" = "#E8E8E8"),
                      labels = setNames(df_d$legende, df_d$cat),
                      name = NULL) +
    # Texte central
    annotate("text", x = 0.5, y = 0,
             label = paste0(pct_oui, "%"),
             size = 11, fontface = "bold", colour = couleur) +
    annotate("text", x = 0.5, y = 0,
             label = paste0("\n\n\n", str_wrap(label_oui, 15)),
             size = 3.8, colour = "#5C5C5C") +
    expand_limits(x = c(-1.5, 3.5), y = c(-1.5, 3.5)) +
    labs(title = str_wrap(titre, 55), subtitle = subtitle) +
    theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", colour = "#2C3E50",
                                       size = 12, hjust = 0.5),
          plot.subtitle = element_text(colour = "#666666", hjust = 0.5, size = 9),
          legend.position = "bottom",
          legend.text = element_text(size = 10, colour = "#2C3E50", face = "bold"),
          legend.key.size = unit(0.7, "cm"),
          legend.spacing.x = unit(0.5, "cm"),
          legend.box.just = "center",
          legend.background = element_rect(fill = "white", colour = "#333333", linewidth = 1.2),
          legend.margin = margin(t = 12, r = 18, b = 12, l = 18))
}

#' Lollipop chart (barres fines + point terminal, plus aéré que les barres)
#' Idéal pour les classements à nombreuses modalités
#'
#' @param df_freq  data.frame avec colonnes `modalite` et `pct`
#' @param titre    Titre du graphique
#' @param couleur  Couleur des points et tiges
graphique_lollipop <- function(df_freq, titre, couleur = "#2980B9", subtitle = NULL) {
  ggplot(df_freq,
    aes(x = pct, y = fct_reorder(str_wrap(as.character(modalite), 38), pct))) +
    geom_segment(aes(xend = 0, yend = fct_reorder(str_wrap(as.character(modalite), 38), pct)),
                 colour = "#CCCCCC", linewidth = 0.8) +
    geom_point(colour = couleur, size = 5, alpha = 0.9) +
    geom_text(aes(label = paste0(pct, "%")), hjust = -0.5, size = 3.4,
              fontface = "bold", colour = "#2C3E50") +
    scale_x_continuous(limits = c(0, max(df_freq$pct) * 1.3),
                       labels = label_percent(scale = 1)) +
    labs(title = str_wrap(titre, 65), subtitle = subtitle,
         x = "Pourcentage (%)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 12),
          plot.subtitle = element_text(colour = "#666666", size = 9),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.y = element_text(colour = "#2C3E50"))
}

#' Dumbbell chart (deux points reliés — compare deux mesures pour chaque catégorie)
#' Idéal pour : canaux actuels vs préférés, avant/après, groupe A vs groupe B
#'
#' @param data      data.frame avec colonnes : `modalite`, `valeur`, `groupe`
#' @param grp1      Nom du groupe 1 (point gauche)
#' @param grp2      Nom du groupe 2 (point droit)
#' @param titre     Titre
#' @param col1      Couleur groupe 1
#' @param col2      Couleur groupe 2
graphique_dumbbell <- function(data, grp1, grp2, titre,
                                col1 = "#95A5A6", col2 = "#2980B9",
                                subtitle = NULL) {
  df_wide <- data %>%
    pivot_wider(names_from = groupe, values_from = valeur) %>%
    mutate(modalite_w = fct_reorder(str_wrap(modalite, 35),
                                    .data[[grp2]]))

  ggplot(df_wide) +
    geom_segment(aes(x = .data[[grp1]], xend = .data[[grp2]],
                     y = modalite_w, yend = modalite_w),
                 colour = "#DDDDDD", linewidth = 1.2) +
    geom_point(aes(x = .data[[grp1]], y = modalite_w),
               colour = col1, size = 4.5, alpha = 0.9) +
    geom_point(aes(x = .data[[grp2]], y = modalite_w),
               colour = col2, size = 4.5, alpha = 0.9) +
    geom_text(aes(x = .data[[grp1]], y = modalite_w,
                  label = paste0(.data[[grp1]], "%")),
              hjust = 1.4, size = 3, colour = col1, fontface = "bold") +
    geom_text(aes(x = .data[[grp2]], y = modalite_w,
                  label = paste0(.data[[grp2]], "%")),
              hjust = -0.4, size = 3, colour = col2, fontface = "bold") +
    scale_x_continuous(labels = label_percent(scale = 1),
                       limits = c(
                         min(c(data$valeur[data$groupe == grp1],
                               data$valeur[data$groupe == grp2]), na.rm = TRUE) - 10,
                         max(c(data$valeur[data$groupe == grp1],
                               data$valeur[data$groupe == grp2]), na.rm = TRUE) + 15
                       )) +
    labs(title = str_wrap(titre, 65), subtitle = subtitle,
         x = "% répondants", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 12),
          plot.subtitle = element_text(colour = "#666666", size = 9),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(colour = "#2C3E50")) +
    # Légende manuelle dans le subtitle via annotation
    annotate("point", x = -Inf, y = -Inf, colour = col1, size = 4) +
    annotate("point", x = -Inf, y = -Inf, colour = col2, size = 4)
}

#' Diverging bar chart (barres partant d'un centre — échelles Likert / satisfaction)
#' Idéal pour : satisfaction (Très satisfait → Pas satisfait)
#'
#' @param data        data.frame avec colonnes `modalite`, `n`, `pct`
#' @param ordre_neg   Vecteur des modalités "négatives" (à gauche du centre)
#' @param ordre_pos   Vecteur des modalités "positives" (à droite du centre)
#' @param palette     Vecteur nommé de couleurs
#' @param titre       Titre
graphique_divergent <- function(data, ordre_neg, ordre_pos, palette, titre,
                                subtitle = NULL) {
  # Construire la position divergente
  df_div <- data %>%
    mutate(modalite = factor(modalite, levels = c(ordre_neg, ordre_pos))) %>%
    arrange(modalite) %>%
    mutate(
      pct_plot = ifelse(modalite %in% ordre_neg, -pct, pct),
      hjust    = ifelse(modalite %in% ordre_neg, 1.15, -0.15)
    )

  ggplot(df_div, aes(x = pct_plot, y = "Répondants", fill = modalite)) +
    geom_col(position = "stack", width = 0.55, alpha = 0.9) +
    geom_vline(xintercept = 0, colour = "white", linewidth = 1) +
    geom_text(aes(label = ifelse(abs(pct_plot) >= 5, paste0(abs(pct), "%"), "")),
              position = position_stack(vjust = 0.5),
              colour = "white", fontface = "bold", size = 4) +
    scale_fill_manual(values = palette, name = "") +
    scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(title = str_wrap(titre, 65), subtitle = subtitle,
         x = "% répondants", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 12),
          plot.subtitle = element_text(colour = "#666666", size = 9),
          legend.position = "bottom",
          legend.text     = element_text(size = 9),
          axis.text.y     = element_blank(),
          panel.grid.major.y = element_blank())
}

#' Slope chart (deux colonnes reliées par des lignes — évolution A → B)
#' Idéal pour : GAP connaissance → pratique
#'
#' @param data   data.frame avec colonnes `label`, `x` (axe), `y` (valeur), `groupe`
#' @param titre  Titre
graphique_slope <- function(data, titre, subtitle = NULL) {
  # data doit avoir : label (libellé), etape (ex: "Connaissance","Pratique"),
  #                   valeur (%), couleur (hex par ligne)
  ggplot(data, aes(x = etape, y = valeur, group = label, colour = label)) +
    geom_line(linewidth = 1.4, alpha = 0.85) +
    geom_point(size = 5, alpha = 0.9) +
    geom_text(data = data %>% filter(etape == min(etape)),
              aes(label = paste0(str_wrap(label, 25), " ", valeur, "%")),
              hjust = 1.08, size = 3.3, fontface = "bold") +
    geom_text(data = data %>% filter(etape == max(etape)),
              aes(label = paste0(valeur, "%")),
              hjust = -0.08, size = 3.5, fontface = "bold") +
    scale_y_continuous(limits = c(0, max(data$valeur) * 1.2),
                       labels = label_percent(scale = 1)) +
    scale_x_discrete(expand = expansion(add = c(3.5, 1.5))) +
    labs(title = str_wrap(titre, 65), subtitle = subtitle,
         x = NULL, y = "% répondants") +
    theme_minimal(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 12),
          plot.subtitle = element_text(colour = "#666666", size = 9),
          legend.position = "none",
          panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.8),
          panel.grid.minor   = element_blank())
}

# Palette institutionnelle CRM
pal <- c(rouge = "#f81c1c", vert = "#3fe58b", bleu = "#1c21f8",
         pink = "#DB6A8F", violet = "#8E44AD", pastel = "#FBBFB8",
         jaune = "#FCFE19", bleu_ciel = "#B6D8F2")

# =============================================================================
# SECTION A — CONNAISSANCE (Q1 à Q5)
# =============================================================================
message("\n=== SECTION A : CONNAISSANCE ===")

# --- A1. Sensibilisation — Q1 (choix unique → pie) ---
fig_A1 <- graphique_pie(
  df %>% mutate(mpox_nandre = recode(as.character(mpox_nandre),
                                     "Oui" = "A entendu parler du Mpox",
                                     "Non" = "N'a pas entendu parler",
                                     "Ne sait pas" = "Ne sait pas")),
  var = "mpox_nandre",
  titre = "Sensibilisation au Mpox",
  couleurs = c(pal[["vert"]], pal[["rouge"]], pal[["pastel"]])
)
sauvegarder(fig_A1, "A1_sensibilisation", w = 9, h = 7.5)

# --- A2. Modes de transmission — Q3 (choix multiples → barres) ---
trans_cols <- c(
  "Eau contaminée"        = "miparitaka/rano_maloto",
  "Contact pers. infectée"= "miparitaka/olona_voa",
  "Sang"                  = "miparitaka/ra",
  "Piqûre insecte"        = "miparitaka/kaikitra_moka",
  "Sécrétions"            = "miparitaka/diky",
  "Aliments contaminés"   = "miparitaka/sakafo_voampoizina",
  "Objets contaminés"     = "miparitaka/zavatra_toerana_voapoizina",
  "Vaisselle commune"     = "miparitaka/lovia",
  "Mains sales"           = "miparitaka/tanana_maloto",
  "Air / Aérosol"         = "miparitaka/rivotra",
  "Autre"                 = "miparitaka/hafa",
  "Ne sait pas"           = "miparitaka/tsy_fantatro"
)
cols_trans_correctes <- c("miparitaka/olona_voa","miparitaka/ra",
                           "miparitaka/diky","miparitaka/tanana_maloto")

df_trans <- df %>%
  summarise(across(any_of(unname(trans_cols)), ~ round(mean(. == 1, na.rm = TRUE)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(trans_cols)[match(col, unname(trans_cols))],
         correct  = col %in% cols_trans_correctes) %>%
  filter(!is.na(modalite))

fig_A2 <- ggplot(df_trans,
    aes(x = pct, y = fct_reorder(modalite, pct), fill = correct)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.3) +
  scale_fill_manual(values = c("TRUE" = pal[["vert"]], "FALSE" = pal[["rouge"]]),
                    labels = c("TRUE" = "Correct ✓", "FALSE" = "Incorrect / NSP"),
                    name = "") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title = "Modes de transmission du Mpox cités",
       subtitle = "Vert = réponse scientifiquement correcte  |  Question à choix multiples",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
sauvegarder(fig_A2, "A2_transmission")

# --- A3. Symptômes — Q4 (choix multiples → barres) ---
symp_cols <- c(
  "Vomissements"         = "fiseho/mandoa",
  "Éruptions cutanées"   = "fiseho/atody_tarimo",
  "Diarrhée"             = "fiseho/fivalanana",
  "Diarrhée sanglante"   = "fiseho/fivalanana_ra",
  "Lésions cutanées"     = "fiseho/marary_hoditra",
  "Fièvre"               = "fiseho/manavy",
  "Fatigue"              = "fiseho/reraka",
  "Douleurs musculaires" = "fiseho/manaintaina",
  "Autre"                = "fiseho/hafa",
  "Ne sait pas"          = "fiseho/tsy_fantatro"
)
cols_symp_correctes <- c("fiseho/atody_tarimo","fiseho/manavy","fiseho/reraka",
                          "fiseho/mandoa","fiseho/marary_hoditra")

df_symp <- df %>%
  summarise(across(any_of(unname(symp_cols)), ~ round(mean(. == 1, na.rm = TRUE)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(symp_cols)[match(col, unname(symp_cols))],
         correct  = col %in% cols_symp_correctes) %>%
  filter(!is.na(modalite))

fig_A3 <- ggplot(df_symp,
    aes(x = pct, y = fct_reorder(modalite, pct), fill = correct)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.3) +
  scale_fill_manual(values = c("TRUE" = pal[["vert"]], "FALSE" = pal[["rouge"]]),
                    labels = c("TRUE" = "Correct ✓", "FALSE" = "Incorrect / NSP"),
                    name = "") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title = "Symptômes du Mpox cités",
       subtitle = "Vert = réponse scientifiquement correcte  |  Question à choix multiples",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
sauvegarder(fig_A3, "A3_symptomes")

# --- A4. Mesures de prévention — Q5 (choix multiples → barres) ---
prev_cols <- c(
  "Lavage des mains"    = "fisorohana/manasa_tanana",
  "Eau propre"          = "fisorohana/misotro_rano",
  "Cuisson aliments"    = "fisorohana/Atao_masaka_tsara_ny_sakafo",
  "Couvrir aliments"    = "fisorohana/manarona_sakafo",
  "Éviter contact"      = "fisorohana/tsy_mifampikasoka",
  "Prière"              = "fisorohana/mivavaka",
  "Miel / plantes"      = "fisorohana/mampiasa_tantely",
  "Vaccination"         = "fisorohana/manao_vaksiny",
  "Gestion des déchets" = "fisorohana/manary_fako",
  "Ne sait pas"         = "fisorohana/tsy_fantatro"
)
cols_prev_correctes <- c("fisorohana/manasa_tanana","fisorohana/tsy_mifampikasoka",
                          "fisorohana/manao_vaksiny","fisorohana/Atao_masaka_tsara_ny_sakafo")

df_prev <- df %>%
  summarise(across(any_of(unname(prev_cols)), ~ round(mean(. == 1, na.rm = TRUE)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(prev_cols)[match(col, unname(prev_cols))],
         correct  = col %in% cols_prev_correctes) %>%
  filter(!is.na(modalite))

fig_A4 <- ggplot(df_prev,
    aes(x = pct, y = fct_reorder(modalite, pct), fill = correct)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.3) +
  scale_fill_manual(values = c("TRUE" = pal[["vert"]], "FALSE" = pal[["rouge"]]),
                    labels = c("TRUE" = "Correct ✓", "FALSE" = "Incorrect / NSP"),
                    name = "") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title = "Mesures de prévention du Mpox citées",
       subtitle = "Vert = réponse scientifiquement correcte  |  Question à choix multiples",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
sauvegarder(fig_A4, "A4_prevention")

# --- A5. Score global de connaissance (pie) ---
df_niveau <- df %>%
  count(niveau_connaissance) %>%
  filter(!is.na(niveau_connaissance)) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         legende = paste0(niveau_connaissance, "  —  ", pct, "%"))

fig_A5 <- ggplot(df_niveau, aes(x = "", y = pct, fill = niveau_connaissance)) +
  geom_col(width = 1, colour = "#333333", linewidth = 1.4) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("Faible (0-1/3)" = pal[["rouge"]],
                                "Moyen (2/3)"    = pal[["pink"]],
                                "Élevé (3/3)"    = pal[["vert"]]),
                    labels = setNames(df_niveau$legende, df_niveau$niveau_connaissance)) +
  labs(title = "Score global de connaissance sur le Mpox",
       subtitle = "Basé sur transmission, symptômes et prévention (≥2 bonnes réponses / domaine)") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 13, hjust = 0.5),
        plot.subtitle = element_text(colour = "#666666", hjust = 0.5, size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 11, colour = "#2C3E50", face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.box.just = "center",
        legend.background = element_rect(fill = "white", colour = "#333333", linewidth = 1.2),
        legend.title = element_blank(),
        legend.margin = margin(t = 8, r = 12, b = 8, l = 12))
sauvegarder(fig_A5, "A5_score_connaissance", w = 7, h = 7)

# --- A6. Dashboard : indicateur défaillant + fausses croyances ---
# ============= DASHBOARD CONNAISSANCE =============
# Tableau de bord 2 panneaux : indicateurs de connaissance + fausses croyances
# Sortie : fig_dashboard → figures/A_dashboard_connaissance.png
# ==================================================

# Calcul des 5 indicateurs depuis df
ind_connaissance <- tibble::tibble(
  indicateur = c(
    "A entendu parler du Mpox",
    "≥ 2 modes de transmission corrects",
    "≥ 2 symptômes corrects",
    "≥ 2 mesures de prévention correctes",
    "Score élevé (transmission + symptômes + prévention)"
  ),
  pct = c(
    round(mean(df$mpox_nandre         == "Oui",          na.rm = TRUE) * 100, 1),
    round(mean(df$ind_trans_2plus      == 1,              na.rm = TRUE) * 100, 1),
    round(mean(df$ind_symp_2plus       == 1,              na.rm = TRUE) * 100, 1),
    round(mean(df$ind_prev_2plus       == 1,              na.rm = TRUE) * 100, 1),
    round(mean(df$niveau_connaissance  == "Élevé (3/3)",  na.rm = TRUE) * 100, 1)
  )
) %>%
  mutate(
    est_plus_bas = pct == min(pct),
    couleur_bar  = if_else(est_plus_bas, pal[["rouge"]], pal[["bleu_ciel"]]),
    indicateur   = fct_reorder(indicateur, pct)
  )

annot_p1 <- ind_connaissance %>% filter(est_plus_bas) %>% slice(1)

p1_dash <- ggplot(ind_connaissance, aes(x = pct, y = indicateur)) +
  geom_col(aes(fill = couleur_bar), width = 0.62, alpha = 0.92, show.legend = FALSE) +
  scale_fill_identity() +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.18, size = 3.8, fontface = "bold", colour = "#2C3E50") +
  annotate("text",
           x = annot_p1$pct + 4,
           y = as.numeric(annot_p1$indicateur) - 0.55,
           label = "↑ Indicateur le plus défaillant",
           colour = pal[["rouge"]], size = 3.3, fontface = "bold.italic", hjust = 0) +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(title    = "Indicateurs de connaissance sur le Mpox",
       subtitle = "Proportion de répondants maîtrisant chaque dimension",
       x = "Pourcentage (%)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", colour = "#2C3E50", size = 12),
        plot.subtitle      = element_text(colour = "#666666", size = 9, margin = margin(b = 6)),
        axis.text.y        = element_text(colour = "#2C3E50", size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
        plot.margin        = margin(t = 8, r = 20, b = 8, l = 8))

fausses_croyances_dict <- tibble::tibble(
  colonne = c(
    "miparitaka/rano_maloto", "miparitaka/kaikitra_moka",
    "miparitaka/sakafo_voampoizina", "miparitaka/zavatra_toerana_voapoizina",
    "miparitaka/lovia", "miparitaka/rivotra",
    "fiseho/fivalanana", "fiseho/fivalanana_ra",
    "fisorohana/mivavaka", "fisorohana/mampiasa_tantely"
  ),
  libelle = c(
    "Eau contaminée (transmission)", "Piqûre d'insecte (transmission)",
    "Aliments contaminés (transmission)", "Objets contaminés (transmission)",
    "Vaisselle commune (transmission)", "Air / Aérosol (transmission)",
    "Diarrhée (symptôme)", "Diarrhée sanglante (symptôme)",
    "Prière comme prévention", "Miel/plantes comme prévention"
  ),
  domaine = c(rep("Transmission", 6), rep("Symptômes", 2), rep("Prévention", 2))
)

cols_presentes <- fausses_croyances_dict %>% filter(colonne %in% names(df))

df_fc <- cols_presentes %>%
  mutate(pct = map_dbl(colonne, ~ round(mean(as.numeric(df[[.x]]), na.rm = TRUE) * 100, 1))) %>%
  arrange(desc(pct)) %>%
  mutate(
    libelle       = fct_reorder(libelle, pct),
    est_plus_haut = pct == max(pct)
  )

couleurs_domaine <- c(
  "Transmission" = pal[["rouge"]],
  "Symptômes"    = pal[["pink"]],
  "Prévention"   = pal[["violet"]]
)

top_fc <- df_fc %>% filter(est_plus_haut) %>% slice(1)

p2_dash <- ggplot(df_fc, aes(x = pct, y = libelle, colour = domaine)) +
  geom_segment(aes(xend = 0, yend = libelle, colour = domaine),
               linewidth = 1.0, alpha = 0.65, show.legend = FALSE) +
  geom_point(size = 5, alpha = 0.95) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.45, size = 3.5, fontface = "bold",
            colour = "#2C3E50", show.legend = FALSE) +
  annotate("text",
           x = top_fc$pct + 2.5,
           y = as.numeric(top_fc$libelle) + 0.58,
           label = "★ Croyance la plus répandue",
           colour = pal[["rouge"]], size = 3.2, fontface = "bold.italic", hjust = 0) +
  scale_colour_manual(values = couleurs_domaine, name = "Domaine") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(title    = "Fausses croyances les plus répandues",
       subtitle = "Items incorrects cités par les répondants, classés par fréquence",
       x = "Pourcentage (%)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", colour = "#2C3E50", size = 12),
        plot.subtitle      = element_text(colour = "#666666", size = 9, margin = margin(b = 6)),
        axis.text.y        = element_text(colour = "#2C3E50", size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
        legend.position    = "bottom",
        legend.title       = element_text(face = "bold", size = 9, colour = "#2C3E50"),
        legend.text        = element_text(size = 9, colour = "#2C3E50"),
        legend.key.size    = unit(0.45, "cm"),
        plot.margin        = margin(t = 8, r = 20, b = 8, l = 8)) +
  guides(colour = guide_legend(override.aes = list(size = 4)))

fig_dashboard <- p1_dash / p2_dash +
  plot_annotation(
    title    = "**Tableau de bord des connaissances — Enquête KAP Mpox**\nCRM Madagascar, 2026",
    subtitle = paste0("n = ", nrow(df), " répondants"),
    tag_levels = "A",
    theme = theme(
      plot.title    = ggtext::element_markdown(face = "bold", size = 15, colour = "#2C3E50",
                                               hjust = 0, lineheight = 1.25, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = "#666666", size = 10, hjust = 0, margin = margin(b = 10)),
      plot.tag      = element_text(face = "bold", size = 13, colour = "#2C3E50"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin   = margin(t = 14, r = 14, b = 10, l = 14)
    )
  ) +
  plot_layout(heights = c(1, 1.6))

sauvegarder(fig_dashboard, "A_dashboard_connaissance", w = 12, h = 14)

# =============================================================================
# SECTION B — PERCEPTION DU RISQUE (Q6 à Q13)
# =============================================================================
message("\n=== SECTION B : PERCEPTION DU RISQUE ===")

# --- B1. Croyance que le Mpox est réel — Q6 (pie) ---
fig_B1 <- graphique_pie(
  df %>% mutate(avy_aiza_lab = recode(as.character(avy_aiza),
    "Oui" = "Croit que c'est réel",
    "Non" = "Ne croit pas",
    "Ne sait pas" = "Ne sait pas")),
  var = "avy_aiza_lab",
  titre = "Croyance que le Mpox est une maladie réelle",
  couleurs = c(pal[["vert"]], pal[["rouge"]], pal[["pastel"]])
)
sauvegarder(fig_B1, "B1_realite_mpox", w = 7, h = 7)

# --- B2. Maladie présente dans la communauté — Q8 (pie) ---
fig_B2 <- graphique_pie(
  df %>% mutate(misy_mpox_lab = recode(as.character(misy_mpox),
    "oui"     = "Oui, présente",
    "non"     = "Non absente",
    "tsy_azoko" = "Ne sait pas")),
  var = "misy_mpox_lab",
  titre = "Le Mpox est-il présent dans votre communauté ?",
  couleurs = c(pal[["rouge"]], pal[["vert"]], pal[["pastel"]])
)
sauvegarder(fig_B2, "B2_mpox_communaute", w = 7, h = 7)

# --- B3. Origines perçues / rumeurs — Q7 (barres, choix multiples) ---
orig_cols <- c(
  "Virus (correct)"        = "hevitra/virus",
  "Malédiction divine"     = "hevitra/ozona",
  "Mensonge autorités"     = "hevitra/lainga",
  "Importée par étrangers" = "hevitra/vazaha",
  "Liée aux prostituées"   = "hevitra/mpivarotena",
  "Nature (air, eau)"      = "hevitra/natiora",
  "Ne sait pas"            = "hevitra/tsy_fantatro"
)
df_orig <- df %>%
  filter(!is.na(hevitra)) %>%
  summarise(across(any_of(unname(orig_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(orig_cols)[match(col, unname(orig_cols))],
         rumeur   = col != "hevitra/virus")

fig_B3 <- ggplot(df_orig,
    aes(x = pct, y = fct_reorder(modalite, pct), fill = rumeur)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("FALSE" = pal[["vert"]], "TRUE" = pal[["rouge"]]),
                    labels = c("FALSE" = "Correct", "TRUE" = "Rumeur"), name = "") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title = "Origines perçues du Mpox",
       subtitle = "Parmi ceux qui croient à sa réalité  |  Rouge = croyance erronée / rumeur",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
sauvegarder(fig_B3, "B3_origines_rumeurs")

# --- B4. Niveau de peur — Q11 (barres ordonnées + couleur dégradée) ---
df_peur <- df %>%
  filter(fanahiana %in% c("tsy_manahy", "manahy_kely", "manahy_mafy", "manahy_be_dia_be")) %>%
  count(fanahiana) %>%
  mutate(
    pct   = round(n / sum(n) * 100, 1),
    label = case_when(
      fanahiana == "tsy_manahy"       ~ "Pas du tout",
      fanahiana == "manahy_kely"      ~ "Un peu",
      fanahiana == "manahy_mafy"      ~ "Assez",
      fanahiana == "manahy_be_dia_be" ~ "Très inquiet"
    ),
    label = factor(label, levels = c("Pas du tout","Un peu","Assez","Très inquiet"))
  )

fig_B4 <- ggplot(df_peur, aes(x = label, y = pct, fill = label)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%\n(n=", n, ")")),
            vjust = -0.3, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("Pas du tout"  = pal[["vert"]],
                                "Un peu"       = pal[["jaune"]],
                                "Assez"        = pal[["pink"]],
                                "Très inquiet" = pal[["rouge"]])) +
  scale_y_continuous(limits = c(0, max(df_peur$pct) * 1.5),
                     labels = label_percent(scale = 1)) +
  labs(title = "Q11 — Niveau d'inquiétude face au Mpox",
       x = NULL, y = "% répondants") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
sauvegarder(fig_B4, "B4_niveau_peur")

# --- B5. Groupes perçus comme transmettant davantage — Q12 (barres) ---
risque_cols <- c(
  "Immunodéprimés"        = "mamindra_bebe/manana_hery",
  "Enfants"               = "mamindra_bebe/ankizy",
  "Femmes enceintes"      = "mamindra_bebe/vehivavy_bevohoka",
  "Travailleuses du sexe" = "mamindra_bebe/mpivarotena",
  "Non-vaccinés"          = "mamindra_bebe/tsy_manantona_toby_pahasalamana",
  "Guéris du Mpox"        = "mamindra_bebe/efa_sitran_mpox",
  "homosexuel"            = "mamindra_bebe/miaraka_amin_ny_lahy_samy_lahy_na_vavy_vavy",
  "Ne sait pas"           = "mamindra_bebe/tsy_fantatro"
)
df_risque <- df %>%
  summarise(across(any_of(unname(risque_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(risque_cols)[match(col, unname(risque_cols))])

fig_B5 <- graphique_barres(df_risque,
  "Groupes perçus comme transmettant davantage le Mpox",
  couleur = pal[["violet"]],
  subtitle = "Question à choix multiples — la somme peut dépasser 100%")
sauvegarder(fig_B5, "B5_groupes_risque")

# --- B6. Groupes les plus à risque d'être atteints — Q13 (barres) ---
voa_cols <- c(
  "Immunodéprimés"        = "voa/manana_hery_fiarovana_ambany",
  "Enfants"               = "voa/ankizy",
  "Femmes enceintes"      = "voa/vehivavy_bevohoka",
  "Travailleuses du sexe" = "voa/mpivaro_tena",
  "homosexuel / lesbienne"= "voa/miaraka_amin_ny_lahy_samy_lahy_na_vavy_samy_vavy",
  "Ne sait pas"           = "voa/tsy_fantatro",
  "Autre"                 = "voa/hafa"
)
df_voa <- df %>%
  summarise(across(any_of(unname(voa_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(voa_cols)[match(col, unname(voa_cols))])

fig_B6 <- graphique_barres(df_voa,
  "Q13 — Groupes les plus vulnérables au Mpox selon les répondants",
  couleur = pal[["bleu"]],
  subtitle = "Question à choix multiples — la somme peut dépasser 100%")
sauvegarder(fig_B6, "B6_groupes_vulnerables")

# --- B7. % global adhérant à au moins une rumeur sur l'origine (donut) ------
pct_rumeur_orig <- round(mean(df$croit_rumeur_origine == 1, na.rm = TRUE) * 100, 1)
fig_B7 <- graphique_donut(
  pct_rumeur_orig,
  label_oui = "Adhère\nà une rumeur",
  titre     = "% adhérant à au moins une rumeur sur l'origine du Mpox",
  couleur   = pal[["rouge"]],
  subtitle  = "Rumeurs : malédiction, mensonge autorités, importée par étrangers, liée aux prostituées, nature"
)
sauvegarder(fig_B7, "B7_adherence_rumeurs", w = 7, h = 7)

# --- B7b. % adhérant à la rumeur sur la raison de la communication — misy_mpox (donut) ---
pct_rumeur_comm <- round(mean(df$misy_mpox == "Non", na.rm = TRUE) * 100, 1)
fig_B7b <- graphique_donut(
  pct_rumeur_comm,
  label_oui = "Nie la présence\ndu Mpox",
  titre     = "% adhérant à la rumeur sur la raison de la communication du Mpox",
  couleur   = pal[["rouge"]],
  subtitle  = "Ceux qui nient la présence du Mpox dans leur communauté "
)
sauvegarder(fig_B7b, "B7b_rumeur_communication", w = 7, h = 7)

# --- B8. Comparaison perception vs réalité scientifique ---------------------
# Chaque item : valeur observée (perception) vs référence idéale (100 %)
df_comp <- tibble::tibble(
  dimension = c(
    "Croit que le Mpox est réel",
    "Identifie correctement la source (virus)",
    "N'adhère à AUCUNE rumeur sur l'origine",
    "Croit que le Mpox est présent dans sa communauté",
    "Ne désigne pas les travailleurs(ses) de sexe comme groupe transmettant +",
    "Ne désigne pas les homosexuels comme groupe transmettant +"
  ),
  pct_obs = c(
    round(mean(df$avy_aiza == "Oui",                                         na.rm = TRUE) * 100, 1),
    round(mean(df[["hevitra/virus"]] == 1,                                   na.rm = TRUE) * 100, 1),
    round(mean(df$croit_rumeur_origine == 0,                                 na.rm = TRUE) * 100, 1),
    round(mean(df$misy_mpox == "Oui",                                        na.rm = TRUE) * 100, 1),
    round(mean(df[["mamindra_bebe/mpivarotena"]] == 0,                       na.rm = TRUE) * 100, 1),
    round(mean(df[["mamindra_bebe/miaraka_amin_ny_lahy_samy_lahy_na_vavy_vavy"]] == 0,
               na.rm = TRUE) * 100, 1)
  ),
  correct = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
) %>%
  mutate(
    ecart      = 100 - pct_obs,
    dimension  = fct_reorder(str_wrap(dimension, 42), pct_obs),
    couleur    = case_when(pct_obs >= 75 ~ pal[["vert"]],
                           pct_obs >= 50 ~ pal[["jaune"]],
                           TRUE          ~ pal[["rouge"]])
  )

fig_B8 <- ggplot(df_comp, aes(y = dimension)) +
  # Barre de fond (réalité idéale = 100 %)
  geom_col(aes(x = 100), fill = "#F0F0F0", width = 0.65) +
  # Barre observée (perception)
  geom_col(aes(x = pct_obs, fill = couleur), width = 0.65, alpha = 0.9) +
  scale_fill_identity() +
  # Étiquette % observé
  geom_text(aes(x = pct_obs, label = paste0(pct_obs, "%")),
            hjust = -0.18, size = 3.8, fontface = "bold", colour = "#2C3E50") +
  # Ligne de référence à 100 %
  geom_vline(xintercept = 100, linetype = "dashed", colour = "#27AE60", linewidth = 0.8) +
  annotate("text", x = 101, y = 0.4, label = "Idéal\n(100%)",
           hjust = 0, size = 3, colour = "#27AE60", fontface = "italic") +
  scale_x_continuous(limits = c(0, 120), labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(
    title    = "Perception vs réalité scientifique — indicateurs clés",
    subtitle = "Vert ≥ 75 % · Jaune 50-74 % · Rouge < 50 %  |  Ligne verte = valeur idéale (100 %)",
    x = "% répondants", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", colour = "#2C3E50", size = 12),
    plot.subtitle      = element_text(colour = "#666666", size = 9),
    axis.text.y        = element_text(colour = "#2C3E50", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
    plot.margin        = margin(t = 8, r = 25, b = 8, l = 8)
  )
sauvegarder(fig_B8, "B8_perception_vs_realite", w = 11, h = 6)

# --- B9. Sources d'info (Q27) croisées avec l'adhésion aux rumeurs ----------
# Montre quels canaux sont associés à une perception correcte ou erronée
# (proportion des citants de chaque canal qui adhèrent à au moins une rumeur)
canaux_rumeur_cols <- c(
  "Télévision"            = "vaovao/fahitalavitra",
  "Bouche-à-oreille"      = "vaovao/resaka_mivantana",
  "Radio"                 = "vaovao/radio",
  "Réseaux sociaux"       = "vaovao/tambazotra",
  "SMS"                   = "vaovao/message_finday",
  "Agents de santé"       = "vaovao/mpiasan_fahasalamana",
  "Agents communautaires" = "vaovao/mpanentana_fiarahamonina",
  "Centre de santé"       = "vaovao/tobim_pahasalamana"
)

cols_cr_present <- canaux_rumeur_cols[unname(canaux_rumeur_cols) %in% names(df)]

if (length(cols_cr_present) > 0) {
  df_cr <- purrr::map_dfr(names(cols_cr_present), function(label) {
    col <- cols_cr_present[[label]]
    utilisateurs <- df %>% filter(.data[[col]] == 1)
    tibble::tibble(
      canal   = label,
      n       = nrow(utilisateurs),
      pct_rumeur = if (nrow(utilisateurs) > 0)
        round(mean(utilisateurs$croit_rumeur_origine == 1, na.rm = TRUE) * 100, 1)
      else NA_real_
    )
  }) %>%
    filter(!is.na(pct_rumeur), n >= 5) %>%
    mutate(
      canal   = fct_reorder(canal, pct_rumeur),
      couleur = case_when(pct_rumeur < 30  ~ pal[["vert"]],
                          pct_rumeur < 55  ~ pal[["jaune"]],
                          TRUE             ~ pal[["rouge"]])
    )

  fig_B9 <- ggplot(df_cr, aes(x = pct_rumeur, y = canal)) +
    geom_segment(aes(xend = 0, yend = canal), colour = "#DDDDDD", linewidth = 0.9) +
    geom_point(aes(colour = couleur), size = 6, alpha = 0.9) +
    scale_colour_identity() +
    geom_text(aes(label = paste0(pct_rumeur, "%")),
              hjust = -0.5, size = 3.5, fontface = "bold", colour = "#2C3E50") +
    geom_vline(xintercept = pct_rumeur_orig, linetype = "dashed",
               colour = "#95A5A6", linewidth = 0.7) +
    annotate("text", x = pct_rumeur_orig + 1, y = 0.5,
             label = paste0("Moy. globale\n", pct_rumeur_orig, "%"),
             hjust = 0, size = 2.8, colour = "#95A5A6", fontface = "italic") +
    scale_x_continuous(limits = c(0, 110), labels = label_percent(scale = 1), expand = c(0, 0)) +
    labs(
      title    = "Adhésion aux rumeurs selon la source d'information principale (Q27)",
      subtitle = "% de citants de chaque canal qui adhèrent à au moins une rumeur sur l'origine du Mpox",
      x = "% adhérant à une rumeur", y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", colour = "#2C3E50", size = 12),
      plot.subtitle      = element_text(colour = "#666666", size = 9),
      axis.text.y        = element_text(colour = "#2C3E50", size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.4),
      plot.margin        = margin(t = 8, r = 25, b = 8, l = 8)
    )
  sauvegarder(fig_B9, "B9_rumeurs_par_canal_info", w = 11, h = 6)
}

# =============================================================================
# SECTION C — PRATIQUES ET ACCÈS AUX SOINS (Q14 à Q26)
# =============================================================================
message("\n=== SECTION C : PRATIQUES ===")

# --- C1. Moments de lavage des mains — Q14 (barres) ---
moments_cols <- c(
  "Après toilettes"              = "fahazarana_manasa_tanana/avy_wc",
  "Avant préparer repas"         = "fahazarana_manasa_tanana/alohan_mikarakara_sakafo",
  "Avant manger"                 = "fahazarana_manasa_tanana/alohan_sakafo",
  "Après manger"                 = "fahazarana_manasa_tanana/rehefa_avy_misakafo",
  "Après contact animaux"        = "fahazarana_manasa_tanana/rehefa_avy_nikasika_biby",
  "Après contact affaires malades"= "fahazarana_manasa_tanana/rehefa_avy_nikasika_entan_olona",
  "Après poignée de main"        = "fahazarana_manasa_tanana/rehefa_avy_nandray_tanana",
  "Après contact malade"         = "fahazarana_manasa_tanana/nifandray_marary",
  "Autre"                        = "fahazarana_manasa_tanana/hafa"
)
cols_moments_critiques_c1 <- c(
  "fahazarana_manasa_tanana/avy_wc",
  "fahazarana_manasa_tanana/alohan_mikarakara_sakafo",
  "fahazarana_manasa_tanana/alohan_sakafo",
  "fahazarana_manasa_tanana/rehefa_avy_nikasika_biby",
  "fahazarana_manasa_tanana/nifandray_marary"
)
df_moments <- df %>%
  summarise(across(any_of(unname(moments_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(moments_cols)[match(col, unname(moments_cols))],
         critique = col %in% cols_moments_critiques_c1)

fig_C1 <- ggplot(df_moments,
    aes(x = pct, y = fct_reorder(modalite, pct), fill = critique)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.4) +
  scale_fill_manual(values = c("TRUE" = pal[["vert"]], "FALSE" = pal[["pastel"]]),
                    labels = c("TRUE" = "Moment critique ✓", "FALSE" = "Autre"), name = "") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title = "Q14 — Moments habituels de lavage des mains",
       subtitle = "Vert = moment critique pour la prévention du Mpox  |  Choix multiples",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
sauvegarder(fig_C1, "C1_moments_lavage")

# --- C2. Produits utilisés pour le lavage — Q15 (barres) ---
produits_cols <- c(
  "Eau + savon"   = "manasa_tanana/rano_savony",
  "Eau seule"     = "manasa_tanana/rano",
  "Eau de javel"  = "manasa_tanana/rano_chlore",
  "Cendre"        = "manasa_tanana/lavenoka",
  "Sable"         = "manasa_tanana/fasika",
  "Autre"         = "manasa_tanana/hafa",
  "Ne sait pas"   = "manasa_tanana/tsy_manana_havaly"
)
df_produits <- df %>%
  summarise(across(all_of(intersect(unname(produits_cols), names(df))),
                   ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(produits_cols)[match(col, unname(produits_cols))])

fig_C2 <- graphique_barres(df_produits,
  "Produits utilisés pour le lavage des mains",
  couleur = pal[["bleu"]],
  subtitle = "Question à choix multiples")
sauvegarder(fig_C2, "C2_produits_lavage")

# --- C2b. Barrières au lavage des mains (antony_tsy_manasa_tanana) ---
if ("antony_tsy_manasa_tanana" %in% names(df)) {
  df_c2b <- df %>%
    filter(!is.na(antony_tsy_manasa_tanana)) %>%
    mutate(barriere_lab = recode(as.character(antony_tsy_manasa_tanana),
      "ho_an_lamba"     = "Pour laver les vêtements",
      "tsy_misy_rano"   = "Pas de l'eau",
      "tsy_misy_savony" = "Pas de savon"))

  if (nrow(df_c2b) > 0) {
    fig_C2b <- graphique_pie(
      df_c2b,
      var      = "barriere_lab",
      titre    = "Principale barrière au lavage des mains",
      couleurs = c("#FCFE19", "#51da4a", "#4a8dda")
    )
    sauvegarder(fig_C2b, "C2b_barrieres_lavage_mains", w = 8, h = 7)
  }
}

# --- C3. Faisabilité isolement — Q17 (pie) ---
df_c3 <- df %>%
  filter(!is.na(manana_efitra)) %>%
  mutate(efitra_lab = recode(as.character(manana_efitra),
    "difficile"         = "Difficile",
    "tsy_fantatro"      = "Ne sait pas",
    "Ne sait pas"       = "Ne sait pas",
    "mora"              = "Facile",
    "tsi_dia_mora"      = "Peu facile",
    "tsy_azo_atao"      = "Pas du tout faisable"))

fig_C3 <- graphique_pie(df_c3,
  var = "efitra_lab",
  titre = "Faisabilité de l'isolement d'un cas suspect à domicile",
  couleurs = c("#f81c1c", "white", "#faf861", pal[["pink"]], pal[["pastel"]])
)
sauvegarder(fig_C3, "C3_isolement_faisabilite", w = 7, h = 7)

# --- C4. Faisabilité matériel dédié — Q18 (pie) ---
df_c4 <- df %>%
  filter(!is.na(manokana_fitaovana)) %>%
  mutate(fitaovana_lab = recode(as.character(manokana_fitaovana),
    "difficile"         = "Difficile",
    "tsy_fantatro"      = "Ne sait pas",
    "Ne sait pas"       = "Ne sait pas",
    "mora"              = "Facile",
    "tsi_dia_mora"      = "Peu facile",
    "tsy_azo_atao"      = "Pas du tout faisable"))

fig_C4 <- graphique_pie(df_c4,
  var = "fitaovana_lab",
  titre = "Faisabilité de réserver le matériel pour un cas suspect",
  couleurs = c("#f81c1c", "white", "#faf861", pal[["pink"]], pal[["pastel"]])
)
sauvegarder(fig_C4, "C4_materiel_faisabilite", w = 7, h = 7)

# --- C5. Décideur dans la famille — Q19 (barres) ---
fianakaviana_cols <- c(
  "Mère"       = "fianakaviana/reny",
  "Père"       = "fianakaviana/ray",
  "Mari"       = "fianakaviana/vady_lehilahy",
  "Femme"      = "fianakaviana/vady_vehivavy",
  "Tante"      = "fianakaviana/nenitoa",
  "Oncle"      = "fianakaviana/dadatoa",
  "Grand-mère" = "fianakaviana/bebe",
  "Grand-père" = "fianakaviana/dadabe",
  "Frère"      = "fianakaviana/rahalahy",
  "Sœur"       = "fianakaviana/anabavy",
  "Autre"      = "fianakaviana/hafa"
)
df_fianakaviana <- df %>%
  summarise(across(any_of(unname(fianakaviana_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(fianakaviana_cols)[match(col, unname(fianakaviana_cols))]) %>%
  filter(pct > 0)

fig_C5 <- graphique_barres(df_fianakaviana,
  "Qui décide de la prise en charge en cas de symptômes Mpox ?",
  couleur = pal[["violet"]],
  subtitle = "Question à choix multiples")
sauvegarder(fig_C5, "C5_decisionnaire_famille")

# --- C6. Recours aux soins — Q20 (barres bicolores) ---
soins_cols <- c(
  "Médecine traditionnelle" = "raha_marary/nentim_paharazana",
  "Médicaments modernes"    = "raha_marary/fanafody_maoderina",
  "Centre de santé"         = "raha_marary/tobim_pahasalamana",
  "Ne rien faire"           = "raha_marary/tsy_mitsabo",
  "Guérisseur trad."        = "raha_marary/manantona_nentim_paharazana",
  "Prière"                  = "raha_marary/ara_pivavahana",
  "Pharmacie"               = "raha_marary/fivarotam_panafody",
  "Traitement à domicile"   = "raha_marary/ao_antrano",
  "Autre"                   = "raha_marary/hafa"
)
df_soins <- df %>%
  summarise(across(any_of(unname(soins_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(soins_cols)[match(col, unname(soins_cols))],
         formel   = col %in% c("raha_marary/tobim_pahasalamana",
                                "raha_marary/fanafody_maoderina",
                                "raha_marary/fivarotam_panafody"))

fig_C6 <- ggplot(df_soins,
    aes(x = pct, y = fct_reorder(modalite, pct), fill = formel)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.4) +
  scale_fill_manual(values = c("TRUE" = pal[["bleu"]], "FALSE" = pal[["rouge"]]),
                    labels = c("TRUE" = "Système formel", "FALSE" = "Informel"), name = "") +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title = "Recours aux soins en cas de symptômes Mpox",
       subtitle = "Bleu = système de santé formel  |  Question à choix multiples",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
sauvegarder(fig_C6, "C6_recours_soins")

# --- C7. Ira au centre de santé — Q25 (pie) ---
fig_C7 <- graphique_pie(
  df %>% mutate(manatona_lab = recode(as.character(manatona_toby),
    "Oui" = "Oui, irait",
    "Non" = "Non, n'irait pas",
    "Ne sait pas" = "Ne sait pas")),
  var = "manatona_lab",
  titre = "Irait-on au centre de santé en cas de symptômes Mpox ?",
  couleurs = c(pal[["vert"]], pal[["rouge"]], pal[["pastel"]])
)
sauvegarder(fig_C7, "C7_intention_csb", w = 7, h = 7)

# --- C7b. % connaissant où aller pour se faire soigner (toerana_mitsabo) ---
pct_connait_recours <- round(mean(df$toerana_mitsabo == "Oui", na.rm = TRUE) * 100, 1)
fig_C7b <- graphique_donut(
  pct_connait_recours,
  label_oui = "Sait où\naller",
  titre     = "% connaissant où aller pour se faire soigner du Mpox",
  couleur   = pal[["vert"]],
  subtitle  = "Proportion ayant identifié un établissement de santé pour le Mpox"
)
sauvegarder(fig_C7b, "C7b_connaissance_recours", w = 7, h = 7)

# --- C8. Distance au centre de santé — Q24 (barres + couleur) ---
fig_C8 <- ggplot(df %>% filter(!is.na(distance_cat)),
    aes(x = distance_cat, fill = distance_cat)) +
  geom_bar(width = 0.6, alpha = 0.9) +
  geom_text(stat = "count",
    aes(label = paste0(round(after_stat(count)/nrow(df)*100, 1), "%\n(n=", after_stat(count), ")")),
    vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("≤ 15 min"  = pal[["vert"]],
                                "16-30 min" = pal[["jaune"]],
                                "31-60 min" = pal[["pink"]],
                                "> 60 min"  = pal[["rouge"]])) +
  scale_x_discrete(limits = c("≤ 15 min", "16-30 min", "31-60 min", "> 60 min")) +
  scale_y_continuous(limits = c(0, max(table(df$distance_cat)) * 1.3)) +
  labs(title = "Temps d'accès au centre de santé le plus proche",
       x = NULL, y = "Nombre de répondants") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
sauvegarder(fig_C8, "C8_distance_csb")

# --- C9. Barrières à l'accès aux soins — Q26 (barres) ---
barriere_cols <- c(
  "Distance"               = "tsy_manantona/halavirina",
  "Peur"                   = "tsy_manantona/tahotra",
  "Coût"                   = "tsy_manantona/tsy_manambola",
  "Temps"                  = "tsy_manantona/fotoana",
  "Manque personnel"       = "tsy_manantona/mpiasa_fahasalamana",
  "Manque équipement"      = "tsy_manantona/fitaovana",
  "Barrière linguistique"  = "tsy_manantona/tsy_mitovy_fiteny",
  "Formation insuffisante" = "tsy_manantona/tsy_ampy_fiofanana",
  "Autre"                  = "tsy_manantona/hafa"
)
df_barrieres <- df %>%
  filter(!is.na(tsy_manantona)) %>%
  summarise(across(any_of(unname(barriere_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(barriere_cols)[match(col, unname(barriere_cols))])

fig_C9 <- graphique_barres(df_barrieres,
  "Barrières à l'accès aux centres de santé",
  couleur = pal[["pink"]],
  subtitle = "Parmi ceux qui n'iraient pas au CSB  |  Choix multiples")
sauvegarder(fig_C9, "C9_barrieres_acces")

# =============================================================================
# SECTION D — CANAUX DE COMMUNICATION (Q27 à Q34)
# =============================================================================
message("\n=== SECTION D : COMMUNICATION ===")

# --- D1. Canaux actuels (Q27) vs préférés (Q31) — comparaison ---
canaux_actuel_cols <- c(
  "Télévision"            = "vaovao/fahitalavitra",
  "Bouche-à-oreille"      = "vaovao/resaka_mivantana",
  "Journal"               = "vaovao/gazety",
  "Radio"                 = "vaovao/radio",
  "Réseaux sociaux"       = "vaovao/tambazotra",
  "SMS"                   = "vaovao/message_finday",
  "OMS"                   = "vaovao/oms",
  "Associations"          = "vaovao/fikambanana",
  "Ministère de la Santé" = "vaovao/ministeran_Fahasalamana",
  "Agents de santé"       = "vaovao/mpiasan_fahasalamana",
  "Agents communautaires" = "vaovao/mpanentana_fiarahamonina",
  "Centre de santé"       = "vaovao/tobim_pahasalamana",
  "École"                 = "vaovao/sekoly"
)
canaux_pref_cols <- c(
  "Télévision"            = "handraisana/fahitalavitra",
  "Bouche-à-oreille"      = "handraisana/resaka_mivantana",
  "Journal"               = "handraisana/gazety",
  "Radio"                 = "handraisana/radio",
  "Réseaux sociaux"       = "handraisana/tambazotra",
  "SMS"                   = "handraisana/message_finday",
  "OMS"                   = "handraisana/oms",
  "Associations"          = "handraisana/fikambanana",
  "Ministère de la Santé" = "handraisana/ministeran_Fahasalamana",
  "Agents de santé"       = "handraisana/mpiasan_fahasalamana",
  "Agents communautaires" = "handraisana/mpanentana_fiarahamonina",
  "Centre de santé"       = "handraisana/tobim_pahasalamana",
  "École"                 = "handraisana/sekoly"
)

df_canaux <- bind_rows(
  df %>%
    summarise(across(any_of(unname(canaux_actuel_cols)),
                     ~ round(mean(. %in% c(1, "1", TRUE, "true"), na.rm = TRUE)*100, 1))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
    mutate(modalite = names(canaux_actuel_cols)[match(col, unname(canaux_actuel_cols))],
           type = "Source actuelle d'information sur le Mpox") %>%
    filter(!is.na(modalite)),
  df %>%
    summarise(across(any_of(unname(canaux_pref_cols)),
                     ~ round(mean(. %in% c(1, "1", TRUE, "true"), na.rm = TRUE)*100, 1))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
    mutate(modalite = names(canaux_pref_cols)[match(col, unname(canaux_pref_cols))],
           type = "Canal d'information préféré pour l'avenir") %>%
    filter(!is.na(modalite))
)

# Divergent chart : montre l'écart entre canal actuel et préféré pour chaque source
df_canaux_wide <- df_canaux %>%
  pivot_wider(names_from = type, values_from = pct, values_fill = 0) %>%
  rename(actuel  = `Source actuelle d'information sur le Mpox`,
         prefere = `Canal d'information préféré pour l'avenir`) %>%
  filter((actuel > 0 | prefere > 0)) %>%
  mutate(
    ecart        = prefere - actuel,
    modalite_ord = fct_reorder(str_wrap(modalite, 30), prefere),
    # Pour le style divergent
    actuel_neg   = -actuel,
    prefere_pos  = prefere
  )

fig_D1 <- ggplot(df_canaux_wide) +
  # Barre actuelle (à gauche/négatif)
  geom_col(aes(x = actuel_neg, y = modalite_ord, fill = "Actuel"),
           position = "identity", width = 0.55, alpha = 0.92,
           colour = "white", linewidth = 0.4) +
  # Barre préféré (à droite/positif)
  geom_col(aes(x = prefere_pos, y = modalite_ord, fill = "Préféré"),
           position = "identity", width = 0.55, alpha = 0.92,
           colour = "white", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "white", linewidth = 1.2) +
  # Étiquettes Actuel (négatif)
  geom_text(aes(x = actuel_neg / 2, y = modalite_ord,
                label = ifelse(abs(actuel_neg) >= 5, paste0(actuel, "%"), "")),
            colour = "white", fontface = "bold", size = 3.5) +
  # Étiquettes Préféré (positif)
  geom_text(aes(x = prefere_pos / 2, y = modalite_ord,
                label = ifelse(prefere_pos >= 5, paste0(prefere, "%"), "")),
            colour = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Actuel"  = pal[["bleu_ciel"]],
                               "Préféré" = pal[["bleu"]]),
                    name = NULL) +
  scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
  labs(title = "Canaux d'information : actuels vs préférés",
       subtitle = "← Offre actuelle   |   Demande préférée →\nMouvement vers la droite = besoin de développement du canal",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 13),
        plot.subtitle = element_text(colour = "#666666", size = 10, hjust = 0.5),
        legend.position = "top",
        legend.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(colour = "#2C3E50", face = "bold", size = 10),
        panel.grid.major.y = element_blank())
sauvegarder(fig_D1, "D1_canaux_divergent", w = 12, h = 7)

# --- D2. Utilité des informations reçues — Q28 (pie) ---
fig_D2 <- graphique_pie(
  df %>% mutate(util_lab = recode(as.character(mahasoa_vaovao),
    "Oui" = "Utiles",
    "Non" = "Pas utiles",
    "Ne sait pas" = "Ne sait pas")),
  var = "util_lab",
  titre = "Les informations reçues sur le Mpox sont-elles utiles ?",
  couleurs = c(pal[["vert"]], pal[["rouge"]], pal[["pastel"]])
)
sauvegarder(fig_D2, "D2_utilite_informations", w = 7, h = 7)

# --- D3. Barrières à l'accès à l'information — Q32 (lollipop) ---
sakana_cols <- c(
  "Analphabétisme"       = "tsy_azahoana/tsy_mahay",
  "Handicap"             = "tsy_azahoana/fahasembanana",
  "Exclusion numérique"  = "tsy_azahoana/tsy_miditra",
  "Autre"                = "tsy_azahoana/hafa",
  "Aucune barrière"      = "tsy_azahoana/tsy_manana_havaly"
)
df_sakana <- df %>%
  summarise(across(any_of(unname(sakana_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(sakana_cols)[match(col, unname(sakana_cols))])

fig_D3 <- graphique_lollipop(df_sakana,
  "Barrières à l'accès aux informations sur le Mpox",
  couleur = pal[["rouge"]],
  subtitle = "Facteurs limitant l'accès aux informations Mpox")
sauvegarder(fig_D3, "D3_barrieres_information")

# --- D4. Sources de confiance — Q33 (lollipop, point CRM mis en évidence) ---
confiance_cols <- c(
  "Agents de santé"    = "azo_itokisana/mpiasa_fahasalamana",
  "CRM"                = "azo_itokisana/crm",
  "Centre de santé"    = "azo_itokisana/tobim_pahasalamana",
  "Pharmacie"          = "azo_itokisana/mpivarotra_fanafody",
  "Guérisseur trad."   = "azo_itokisana/mpitsabo_nentimpaharazana",
  "Scientifiques"      = "azo_itokisana/mahay_siansa",
  "Politiciens"        = "azo_itokisana/mpanao_politika",
  "Assistants sociaux" = "azo_itokisana/mpiasa_sosialy",
  "Associations"       = "azo_itokisana/fikambanana",
  "ONG"                = "azo_itokisana/ong",
  "Amis / Famille"     = "azo_itokisana/namana",
  "Enseignants"        = "azo_itokisana/mpampianatra",
  "Communauté"         = "azo_itokisana/fiarahamonina",
  "Leaders religieux"  = "azo_itokisana/mpitondra_fivavahana",
  "Leaders locaux"     = "azo_itokisana/mpitondra_eo_antoerana",
  "Jeunes leaders"     = "azo_itokisana/tanora_mpitarika"
)
df_confiance <- df %>%
  summarise(across(any_of(unname(confiance_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(confiance_cols)[match(col, unname(confiance_cols))],
         crm      = col == "azo_itokisana/crm")

fig_D4 <- ggplot(df_confiance,
    aes(x = pct,
        y = fct_reorder(str_wrap(modalite, 35), pct),
        colour = crm)) +
  geom_segment(aes(xend = 0, yend = fct_reorder(str_wrap(modalite, 35), pct)),
               colour = "#DDDDDD", linewidth = 0.9) +
  geom_point(aes(size = crm), alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.55, size = 3.3, fontface = "bold") +
  scale_colour_manual(values = c("FALSE" = pal[["bleu"]], "TRUE" = pal[["rouge"]]),
                      labels = c("FALSE" = "Autre", "TRUE" = "CRM ★"), name = "") +
  scale_size_manual(values  = c("FALSE" = 4.5, "TRUE" = 7), guide = "none") +
  scale_x_continuous(limits = c(0, max(df_confiance$pct) * 1.35),
                     labels = label_percent(scale = 1)) +
  labs(title = "Sources d'information perçues comme fiables",
       subtitle = "Point rouge agrandi = CRM (Croix-Rouge Malagasy)  |  Choix multiples",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 12),
        plot.subtitle = element_text(colour = "#666666", size = 9),
        legend.position    = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y = element_text(colour = "#2C3E50"))
sauvegarder(fig_D4, "D4_sources_confiance_lollipop", h = 7)

# --- D5. Langue souhaitée pour l'information sur le Mpox (fiteny) ---
if ("fiteny" %in% names(df)) {
  df_d5 <- df %>%
    filter(!is.na(fiteny)) %>%
    mutate(fiteny_lab = recode(as.character(fiteny),
      "francais" = "Français",
      "gasy"     = "Malagasy"))
  if (nrow(df_d5) > 0) {
    fig_D5 <- graphique_pie(
      df_d5,
      var      = "fiteny_lab",
      titre    = "Langue souhaitée pour recevoir des informations sur le Mpox",
      couleurs = c("#1c21f8", "#f81c1c")
    )
    sauvegarder(fig_D5, "D5_langue_souhaitee", w = 7, h = 7)
  }
}

# =============================================================================
# SECTION E — VACCINATION (Q35 à Q46)
# =============================================================================
message("\n=== SECTION E : VACCINATION ===")

# --- E1. Connaissance du vaccin — Q35 (pie avec les deux catégories) ---
fig_E1 <- graphique_pie(
  df %>% mutate(vaksiny_lab = case_when(
    vaksiny == "Oui" ~ "Connaît le vaccin",
    vaksiny == "Non" ~ "Ne connaît pas",
    TRUE             ~ "Ne sait pas"
  )),
  var      = "vaksiny_lab",
  titre    = "Connaissance de l'existence du vaccin Mpox",
  subtitle = "Avez-vous entendu parler du vaccin contre le Mpox ?",
  couleurs = c(pal[["bleu"]], pal[["rouge"]], pal[["vert"]])
)
sauvegarder(fig_E1, "E1_connaissance_vaccin", w = 7, h = 7)

# --- E2. Exposition aux rumeurs sur le vaccin — Q37 (donut) ---
pct_rumeur_vac <- round(mean(df$vaovao_ratsy == "Oui", na.rm = TRUE)*100, 1)
fig_E2 <- graphique_donut(pct_rumeur_vac, "Exposé\naux rumeurs",
  "Exposition à des rumeurs sur le vaccin Mpox",
  couleur = pal[["rouge"]])
sauvegarder(fig_E2, "E2_rumeurs_vaccin", w = 7, h = 7)

# --- E3. Acceptation vaccinale — Q41 (donut) ---
pct_accept_vac <- round(mean(df$vaksiny_vonona == "Oui", na.rm = TRUE)*100, 1)
fig_E3 <- graphique_donut(pct_accept_vac, "Accepte\nle vaccin",
  "Acceptation de la vaccination Mpox",
  couleur = pal[["vert"]])
sauvegarder(fig_E3, "E3_acceptation_vaccin", w = 7, h = 7)

# --- E4. Raisons de refus — Q38 (lollipop) ---
refus_cols <- c(
  "Peur effets secondaires" = "manakana/voka_dratsy",
  "Peur décès"              = "manakana/mahafaty",
  "Autre personne contre"   = "manakana/olona_hafa",
  "Manque d'information"    = "manakana/tsy_ampy_fahafantarana",
  "Vaccin non disponible"   = "manakana/tsy_ampy_vaksiny",
  "Peur opinion publique"   = "manakana/matahotra_olompirenena",
  "Séquelle COVID"          = "manakana/voan_covid",
  "Malade actuellement"     = "manakana/Marary",
  "Vaccin peu fiable"       = "manakana/tsy_tsara_vaksiny",
  "Vaccin inefficace"       = "manakana/tsy_mandaitra",
  "Déjà assez vacciné"      = "manakana/vaksiny_ampy",
  "Coût"                    = "manakana/vidiny",
  "Attendre de voir"        = "manakana/mijery_hafa"
)
df_refus <- df %>%
  filter(!is.na(manakana)) %>%
  summarise(across(any_of(unname(refus_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(refus_cols)[match(col, unname(refus_cols))]) %>%
  filter(pct > 0)

fig_E4 <- graphique_lollipop(df_refus,
  "Raisons de refus / hésitation à la vaccination Mpox",
  couleur = pal[["rouge"]],
  subtitle = "Parmi les hésitants et réfractaires  |  Choix multiples")
sauvegarder(fig_E4, "E4_raisons_refus_vaccin")

# --- E5. Autonomie décisionnelle vaccinale — Q43 (pie) ---
df_autonomie <- df %>%
  filter(!is.na(mahazo_lalana)) %>%
  mutate(auto_lab = recode(as.character(mahazo_lalana),
    "Oui"         = "Besoin d'autorisation",
    "Non"         = "Décision autonome",
    "Ne sait pas" = "Ne sait pas")) %>%
  count(auto_lab) %>%
  mutate(pct      = round(n / sum(n) * 100, 1),
         legende  = paste0(auto_lab, "  —  ", pct, "%"))

fig_E5 <- ggplot(df_autonomie, aes(x = "", y = pct, fill = auto_lab)) +
  geom_col(width = 1, colour = "#333333", linewidth = 1.4) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c(
    "Besoin d'autorisation" = pal[["rouge"]],
    "Décision autonome"     = pal[["vert"]],
    "Ne sait pas"           = pal[["pastel"]]),
    labels = setNames(df_autonomie$legende, df_autonomie$auto_lab)) +
  labs(title = "Autonomie décisionnelle pour la vaccination",
       subtitle = "Parmi ceux qui connaissent le vaccin Mpox") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", colour = "#2C3E50",
                                     size = 13, hjust = 0.5),
        plot.subtitle = element_text(colour = "#666666", hjust = 0.5, size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 11, colour = "#2C3E50", face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.box.just = "center",
        legend.background = element_rect(fill = "white", colour = "#333333", linewidth = 1.2),
        legend.title = element_blank(),
        legend.margin = margin(t = 8, r = 12, b = 8, l = 12))
sauvegarder(fig_E5, "E5_autonomie_vaccinale", w = 7, h = 7)

# --- E6. Site préféré pour la vaccination — Q45 (lollipop) ---
site_cols <- c(
  "Hôpital"            = "toerana_vaksiny/hopitaly",
  "Centre de santé"    = "toerana_vaksiny/tobim_pahasalamana",
  "Lieu de travail"    = "toerana_vaksiny/toeram_piasana",
  "Pharmacie"          = "toerana_vaksiny/fivarotam_panafody",
  "Site communautaire" = "toerana_vaksiny/ivon_toerana_communautaire",
  "Autre"              = "toerana_vaksiny/hafa",
  "Pas besoin"         = "toerana_vaksiny/tsy_mila",
  "Ne sait pas"        = "toerana_vaksiny/tsy_manana_havaly"
)
# Garder uniquement les colonnes qui existent dans les données
site_cols <- site_cols[unname(site_cols) %in% colnames(df)]

df_site <- df %>%
  summarise(across(any_of(unname(site_cols)), ~ round(mean(. == 1, na.rm = TRUE)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(site_cols)[match(col, unname(site_cols))]) %>%
  filter(pct > 0)

fig_E6 <- graphique_lollipop(df_site,
  "Site préféré pour la vaccination Mpox",
  couleur = pal[["bleu"]],
  subtitle = "Parmi ceux qui connaissent le vaccin  |  Choix multiples")
sauvegarder(fig_E6, "E6_site_vaccination")

# =============================================================================
# SECTION F — SATISFACTION DES SERVICES (Q47 à Q53) — Diverging bar
# =============================================================================
message("\n=== SECTION F : SATISFACTION ===")

palette_satisfaction <- c(
  "Très satisfait"   = "#1A7A4A",
  "Satisfait"        = pal[["vert"]],
  "Plutôt satisfait" = pal[["jaune"]],
  "Pas satisfait"    = pal[["rouge"]],
  "Ne sait pas"      = "#95A5A6"   # Gris = réponse neutre / sans opinion
)
niveaux_sat <- c("Pas satisfait", "Ne sait pas",
                 "Plutôt satisfait", "Satisfait", "Très satisfait")

vars_satisfaction <- c(
  "Équipe coordination"  = "ekipa",
  "Accueil vaccination"  = "afapo_vaksiny",
  "Soins au CSB"         = "fitsaboana_afapo"
)

df_sat_all <- purrr::map_dfr(names(vars_satisfaction), function(label) {
  col <- vars_satisfaction[[label]]
  df %>%
    filter(!is.na(.data[[col]])) %>%
    mutate(lab = recode(as.character(.data[[col]]),
      "afapo"        = "Satisfait",
      "mety_afapo"   = "Plutôt satisfait",
      "tsy_afapo"    = "Pas satisfait",
      "tena_afapo"   = "Très satisfait",
      "tsy_fantatro" = "Ne sait pas")) %>%
    count(lab) %>%
    mutate(pct = round(n / sum(n) * 100, 1), indicateur = label)
})

df_sat_div <- df_sat_all %>%
  mutate(
    lab        = factor(lab, levels = niveaux_sat),
    pct_div    = ifelse(lab %in% c("Pas satisfait", "Ne sait pas"), -pct, pct),
    indicateur = factor(indicateur, levels = rev(names(vars_satisfaction)))
  )

fig_F_divergent <- ggplot(df_sat_div,
    aes(x = pct_div, y = indicateur, fill = lab)) +
  geom_col(position = "stack", width = 0.55, alpha = 0.92,
           colour = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "white", linewidth = 1.2) +
  geom_text(aes(label = ifelse(abs(pct_div) >= 8, paste0(abs(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 3.8) +
  scale_fill_manual(
    values = palette_satisfaction,
    breaks = niveaux_sat,
    name   = "",
    labels = c(
      "Pas satisfait"    = "Pas satisfait",
      "Ne sait pas"      = "Ne sait pas / Sans opinion (gris)",
      "Plutôt satisfait" = "Plutôt satisfait",
      "Satisfait"        = "Satisfait",
      "Très satisfait"   = "Très satisfait"
    )
  ) +
  scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
  labs(title    = "Satisfaction des services liés au Mpox",
       subtitle = "← Insatisfaction   |   Satisfaction →\nGris = Ne sait pas / Sans opinion",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", colour = "#2C3E50", size = 13),
        plot.subtitle = element_text(colour = "#666666", size = 10, hjust = 0.5),
        legend.position    = "bottom",
        legend.text        = element_text(size = 9),
        axis.text.y        = element_text(colour = "#2C3E50", face = "bold", size = 10),
        panel.grid.major.y = element_blank())
sauvegarder(fig_F_divergent, "F_satisfaction_divergent", w = 11, h = 5)

# --- F2. Raisons d'insatisfaction sur les services Mpox (ekipa_tsy_afapo + tsy_afapo_tsabo) ---
vars_insatisfaction <- c(
  "Équipe de coordination" = "ekipa_tsy_afapo",
  "Soins au centre de santé" = "tsy_afapo_tsabo"
)
vars_insat_pres <- vars_insatisfaction[unname(vars_insatisfaction) %in% names(df)]

if (length(vars_insat_pres) > 0) {
  # Mapping pour regrouper les variantes de réponses (classées par catégorie)
  recode_insatisfaction <- function(x) {
    recode(as.character(x),
      # Peu/Pas de sensibilisation
      "Betsaka ny vaovao tsy fantatra" = "Information incomplète",
      "Mahalana ny fanentanana" = "Peu de sensibilisation",
      "Mbola tsy misy" = "Pas de sensibilisation",
      "Mbola tsy misy mpanentana" = "Pas de sensibilisation",
      "Mbola tsy nadalo" = "Pas de sensibilisation",
      "Mbola tsy nisy mpanentana" = "Pas de sensibilisation",
      "Mbola tsy nisy nandalo" = "Pas de sensibilisation",
      "Mbola tsy nisy nanentana" = "Pas de sensibilisation",
      "Mbola tsy tojo mintsy aho" = "Pas de sensibilisation",
      "Mbola vitsy ny fanentanana" = "Peu de sensibilisation",
      "Satria eto amin'ny Fokontany tsy dia ampy fanentanana loatra" = "Pas de sensibilisation",
      "Satria Mbola tsy nanao" = "Pas de sensibilisation",
      "Satria mbola tsy nisy nanentana" = "Pas de sensibilisation",
      "Satria mbola tsy nisy panadihady anay raha tsy vo zao " = "Pas de sensibilisation",
      "Satria tsy ataon'izy ireo matetika ilay fanentanana ka tsy de rarahian'ny olona" = "Peu de sensibilisation",
      "Satria tsy misy ihany ny fanentana atao" = "Pas de sensibilisation",
      "Satria tsy misy ny mpanentana momba io aretina io eto @ fokontany" = "Pas de sensibilisation",
      "Satria zay vo nisy panadihady mombany aretina vaovao ty teto" = "Peu de sensibilisation",
      "Tsisy fanentanana aty" = "Pas de sensibilisation",
      "Tsisy mpanentana" = "Pas de sensibilisation",
      "Tsisy nanentana " = "Pas de sensibilisation",
      "Tsisy sensibilisation aty aminay" = "Pas de sensibilisation",
      "Tsiampy ni olona manentana   arapahasalammana" = "Peu de sensibilisation",
      "Tsy ampi fanentanana tsara ,otrany resaka kobokobonina fotsin" = "Peu de sensibilisation",
      "Tsy ampy " = "Peu de sensibilisation",
      "Tsy ampy fanentanana" = "Peu de sensibilisation",
      "Tsy ampy fanentanana " = "Peu de sensibilisation",
      "Tsy ampy fanentanana ny ety @ fokontany" = "Peu de sensibilisation",
      "Tsy ampy fanetanana" = "Peu de sensibilisation",
      "Tsy ampy le fanetanana" = "Peu de sensibilisation",
      "Tsy ampy ny fanentanana" = "Peu de sensibilisation",
      "Tsy ampy ny fanentanana momban'ilay aretina" = "Pas de retour",
      "Tsy dia misy fanentanana" = "Peu de sensibilisation",
      "Tsy matetika" = "Peu de sensibilisation",
      "Tsy mbola matetika" = "Peu de sensibilisation",
      "Tsy mbola nisy nanentana tety aminay" = "Pas de sensibilisation",
      "Tsy misy ekipan'ny fahasalamana  midina aty" = "Peu de sensibilisation",
      "Tsy misy fanentanana" = "Pas de sensibilisation",
      "Tsy misy fanentanana aty " = "Pas de sensibilisation",
      "Tsy misy mampahafantatra anay" = "Pas de sensibilisation",
      "Tsy misy manentana aty @ fokontany" = "Pas de sensibilisation",
      "Tsy misy mboal taty aminay" = "Pas de sensibilisation",
      "Tsy misy mpanentana" = "Pas de sensibilisation",
      "Tsy misy ny mpanentana" = "Pas de sensibilisation",
      "Tsy manantona ny fiaraha monina ny mpiasan ny fahasalamana" = "Pas de sensibilisation",
      "Tsy nisy nanetana, sy nampahalala" = "Pas de sensibilisation",

      # Manque d'information
      "Tsy ampy fampitana vaovao " = "Manque d'information",
      "Tsy ampy ny fahalalana" = "Manque d'information",
      "Tsy ampy ny fahalalany olona" = "Manque d'information",
      "Tsy ampy ny fana zavana" = "Manque d'information",
      "Tsy ampy n'y fanazavana sy fitaovana ampiasaina " = "Manque d'information",
      "Tsy ampy ny fifandraisana" = "Manque d'information",
      "Tsy azoko tsara io" = "Manque d'information",
      "Tsy de mahita ny fampahalalana momba izany aho" = "Manque d'information",
      "Tsy dia mazava ny fanazavanay" = "Manque d'information",
      "Satria tsy dia mazava tsara ny fanazavan'izy ireo" = "Manque d'information",
      "Satria tsy lisy afaka anontaniana" = "Manque d'information",
      "Satria tsy naheno no tsy nahita marina ,tsy fantatra zay tena marina" = "Manque d'information",
      "Tsidia mazavalotra" = "Manque d'information",
      "Tsy mazava tsara ny fanazavana azo" = "Manque d'information",

      # Peur
      "Fahatahorana" = "Peur",
      "Mbola ts matoky saode mbola miverina" = "Peur si la maladie vas retourner",
      "Satria vaovao tsy. Marina daholo izany" = "Peur",
      "Tsy atokisana" = "Peur",

      # Problème de prise en charge
      "Mbola mitombo ny voan'aretina io ,midika fa tsy ampy fanaraha-maso" = "Pas de suivi",
      "Tsy mahazo fanafofy ao hôpital" = "Problème de prise en charge",
      "Tsy misy fandr1isana antanana alors rehefa Tonga any @hopitaly fa mandoa vola" = "Problème de prise en charge",
      "Zareo maika maika ihany d ety ampy ny fanazavana" = "Problème de prise en charge",

      # Perte de temps
      "Fandaniam-potoana fktsiny" = "Perte de temps",
      "Mandany fotoagna" = "Perte de temps",
      "Mandany fotoana" = "Perte de temps",

      # Autres raisons
      "Hatsaraina ny fandraisana marary" = "Amélioration de prise en charge",
      "Kely fotaona iresahana " = "Peu de temps d'en parler",
      "Kitranoatrano" = "Il y a des familles ou amis",
      "Masiaka ny rasazy" = "Agent de santé méchante",
      "Mavandy" = "Ne dis pas la vérité",
      "Medecin après la mort" = "Medecin après la mort",
      "Mifidifidy" = "Il y a des familles ou amis",
      "Mila entitra fa tsy sangisangy ny aretina" = "Il faut être strict",
      "Mila mirahina" = "Besoin d'accompagnement",
      "Milaza rah mbol tsisy" = "dire qu'il n'y a pas encore",
      "Sambany vao nifampiresqka" = "C'est la première fois qu'on se parle",
      "Satria aretina foronina fotsiny , politika fotsiny fa tsy misy akory" = "rumeur",
      "Satria tsy concerne amiko" = "je ne suis pas concerné",
      "Tsa naheno ny zava-misy" = "rumeur",
      "Tsara ny miaraka maro mbahandresilahatra" = "c'est mieux si on fait ensemble",
      "Tsisy tamberin'andraikitra" = "Pas de retour",
      "Tsiteanao" = "Ne veut pas faire",
      "Tsy liana" = "Non intéressé",
      "Tsy miara miasa aminy" = "Ne veut pas faire",
      "Tsy mino ilay aretina aho" = "Ne croix pas",
      "Tsy mino oe misy ilay aretina fa politika fotsiny" = "Ne croix pas",
      "Tsy mitovy hevitra" = "Ne pas de même avis",
      "Tsy manana havaly" = "Ne sait pas",
      "Tsy nisy fanomezana" = "Pas de cadeaux",
      "Tsy nisy loatra" = "il n'y a pas",
      "Tsy nizara zavatra " = "Pas de cadeaux",
      "Tsy tena misy" = "Ne croix pas",
      "Tsy teo antoerana" = "Ne pas dans le site",
      "Vonjitavanandro" = "volontarité",

      # Soins au centre de santé
      "Elabe vao mivaly" = "Problème de prise en charge",
      "Hamotra Fanafody fa aretina lainga" = "N'existe pas",
      "Marary vakisiny" = "Peur de vaccin",
      "Mbola tsy voany" = "Ne croix pas",
      "Miavona ny olona ao" = "Problème de prise en charge",
      "Namparary" = "Problème de prise en charge",
      "Nitombo ny asalohako" = "Ne veut pas faire",
      "Nohony hamafisany." = "Ne veut pas faire",
      "Satria izy tsy nitsabo bojo fa volantena no manefa" = "Problème de prise en charge",
      "Tsa nisy voa agnay" = "Ne veut pas faire",
      "Tsimahefa" = "Ne veut pas faire",
      "Tsy afapo" = "Ne pas satisfait",
      "Tsy ampy ny fampahafantara \nTsisy fanentanana" = "Manque d'information",
      "tsy ampy ny fikarakarana" = "Problème de prise en charge",
      "Tsy dia iasan'olona saina lotra izy io " = "Manque d'information",
      "Tsy mandray tsara" = "Problème de prise en charge",
      "Tsy nistraka satria aretina tsy misy akory" = "rumeur",
      "Tsy voaray tsara olo marary mandeha @ hopitaly" = "Problème de prise en charge",

      .default = as.character(x)
    )
  }

  df_insat <- purrr::map_dfr(names(vars_insat_pres), function(label) {
    col <- vars_insat_pres[[label]]
    df %>%
      filter(!is.na(.data[[col]]),
             str_trim(as.character(.data[[col]])) != "") %>%
      mutate(modalite_recoded = recode_insatisfaction(.data[[col]])) %>%
      count(modalite_recoded, name = "n", sort = TRUE) %>%
      rename(modalite = modalite_recoded) %>%
      slice_head(n = 6) %>%
      mutate(
        pct        = round(n / sum(n) * 100, 1),
        modalite   = str_trunc(as.character(modalite), 40),
        indicateur = label
      )
  })

  if (nrow(df_insat) > 0) {
    fig_F2 <- ggplot(df_insat,
        aes(x = pct,
            y = fct_reorder(str_wrap(modalite, 35), pct))) +
      geom_col(fill = pal[["rouge"]], alpha = 0.85, width = 0.65) +
      geom_text(aes(label = paste0(pct, "%")),
                hjust = -0.15, size = 3.4, fontface = "bold") +
      facet_wrap(~indicateur, scales = "free_y") +
      scale_x_continuous(limits = c(0, 120),
                         labels = label_percent(scale = 1)) +
      labs(title    = "Raisons d'insatisfaction sur les services liés au Mpox",
           subtitle = "Top réponses parmi les non-satisfaits",
           x = "% répondants", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(plot.title         = element_text(face = "bold", colour = "#2C3E50", size = 12),
            plot.subtitle      = element_text(colour = "#666666", size = 9),
            panel.grid.major.y = element_blank(),
            strip.text         = element_text(face = "bold", colour = "#2C3E50", size = 10))
    sauvegarder(fig_F2, "F2_raisons_insatisfaction", w = 12, h = 6)
  }
}

# --- F4. Importance mesures préventives après vaccination — Q49 (barres) ---
df_f4 <- df %>%
  filter(!is.na(fep_fiarovana)) %>%
  mutate(fep_lab = recode(as.character(fep_fiarovana),
    "tena_zavadehibe" = "Très important",
    "zavadehibe"      = "Important",
    "tsy_mila"        = "Pas nécessaire",
    "tsy_fantatro"    = "Ne sait pas"),
    fep_lab = factor(fep_lab, levels = c("Très important", "Important", "Pas nécessaire", "Ne sait pas"))) %>%
  count(fep_lab) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

fig_F4 <- ggplot(df_f4, aes(x = pct, y = fct_reorder(fep_lab, pct), fill = fep_lab)) +
  geom_col(width = 0.6, alpha = 0.9, colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, size = 3.8, fontface = "bold", colour = "#2C3E50") +
  scale_fill_manual(values = c("Très important"   = "#1A7A4A",
                               "Important"        = pal[["vert"]],
                               "Pas nécessaire"   = pal[["rouge"]],
                               "Ne sait pas"      = pal[["pastel"]]),
                    name = NULL) +
  scale_x_continuous(limits = c(0, max(df_f4$pct) * 1.3), labels = label_percent(scale = 1)) +
  labs(title = "Les mesures préventives restent-elles importantes après vaccination ?",
       subtitle = "Importance perçue des mesures même après vaccination",
       x = "% répondants", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = "#2C3E50"))
sauvegarder(fig_F4, "F4_prevention_post_vaccin", w = 9, h = 5.5)

# =============================================================================
# SECTION G — STIGMATISATION (Q54 à Q57)
# =============================================================================
message("\n=== SECTION G : STIGMATISATION ===")

# --- G1. Facilité à parler du Mpox — Q54 (donut) ---
# Debug : vérifier la colonne miresaka
message("Colonne miresaka - valeurs uniques:")
print(table(df$miresaka, useNA = "always"))

# Calculer le pourcentage (avec sécurité)
if ("miresaka" %in% colnames(df)) {
  pct_tabou <- round(mean(df$miresaka == "Non", na.rm = TRUE)*100, 1)
  if (is.na(pct_tabou) || is.nan(pct_tabou)) {
    pct_tabou <- 0
  }
} else {
  pct_tabou <- 0
  message("⚠️ Colonne miresaka non trouvée")
}

message(sprintf("Pourcentage tabou: %.1f%%", pct_tabou))

fig_G1 <- graphique_donut(pct_tabou, "Difficile\nd'en parler",
  "Le Mpox est-il tabou dans la communauté ?",
  couleur = pal[["rouge"]])
sauvegarder(fig_G1, "G1_tabou_mpox", w = 7, h = 7)

# --- G2. Comportements discriminatoires observés — Q56 (donut) ---
pct_discrim <- round(mean(df$fihetsika_ratsy == "oui", na.rm = TRUE)*100, 1)
fig_G2 <- graphique_donut(pct_discrim, "A observé\nde la discrim.",
  "Discrimination envers les malades Mpox observée",
  couleur = pal[["violet"]])
sauvegarder(fig_G2, "G2_discrimination_observee", w = 7, h = 7)

# --- G3. Groupes victimes de discrimination — Q57 (lollipop) ---
discrim_cols <- c(
  "Femmes"                = "manavaka/vehivavy",
  "Hommes"                = "manavaka/lehilahy",
  "Enfants"               = "manavaka/ankizy",
  "Jeunes"                = "manavaka/tanora",
  "Travailleuses du sexe" = "manavaka/mpivaro_tena",
  "Malades Mpox"          = "manavaka/voan_mpox",
  "homosexuel"            = "manavaka/lahy_lahy",
  "lesbienne"             = "manavaka/vavy_vavy",
  "Autre"                 = "manavaka/hafa",
  "Ne sait pas"           = "manavaka/tsy_fantatro"
)
df_discrim <- df %>%
  summarise(across(any_of(unname(discrim_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(discrim_cols)[match(col, unname(discrim_cols))]) %>%
  filter(pct > 0)

fig_G3 <- graphique_lollipop(df_discrim,
  "Groupes perçus comme victimes de discrimination liée au Mpox",
  couleur = pal[["violet"]],
  subtitle = "Choix multiples — la somme peut dépasser 100%")
sauvegarder(fig_G3, "G3_groupes_discrimination")

# --- G4. Solutions proposées contre la stigmatisation (azo_atao) ---
if ("azo_atao" %in% names(df)) {
  df_solutions <- df %>%
    filter(!is.na(azo_atao),
           str_trim(as.character(azo_atao)) != "") %>%
    count(azo_atao, name = "n", sort = TRUE) %>%
    rename(modalite = 1) %>%
    mutate(
      modalite = str_trim(as.character(modalite)),
      # Recodage des modalités en français
      modalite = case_when(
        modalite %in% c("Tsy fantatro", "Tsy haiko", "Tsy aiko") ~ "Je ne sais pas",
        modalite == "Fanentanana" ~ "Sensibilisation",
        modalite == "Tsy manana havaly" ~ "Pas de réponse",
        modalite == "Atokana" ~ "Isolé",
        modalite == "Tsy misy" ~ "Rien",
        TRUE ~ modalite
      )
    ) %>%
    group_by(modalite) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    arrange(desc(n)) %>%
    slice_head(n = 8) %>%
    mutate(
      pct      = round(n / sum(n) * 100, 1),
      modalite = str_trunc(as.character(modalite), 45)
    )

  if (nrow(df_solutions) > 0) {
    fig_G4 <- graphique_lollipop(df_solutions,
      "Solutions proposées pour réduire la stigmatisation liée au Mpox",
      couleur  = pal[["violet"]],
      subtitle = "Top réponses — Question ouverte")
    sauvegarder(fig_G4, "G4_solutions_stigmatisation")
  }
}

# =============================================================================
# SECTION H — ENGAGEMENT COMMUNAUTAIRE (Q58 à Q64)
# =============================================================================
message("\n=== SECTION H : ENGAGEMENT COMMUNAUTAIRE ===")

# --- H1. Acteurs représentant la communauté — Q58 (lollipop) ---
solo_cols <- c(
  "Leader communautaire"  = "solo_tena/mpitarika_communautaire",
  "Agent communautaire"   = "solo_tena/anti_mpanahy",
  "Leader religieux"      = "solo_tena/mpitondra_fivavahana",
  "Chef d'entreprise"     = "solo_tena/tompona_orinasa",
  "Groupe jeunes"         = "solo_tena/vondrona_tanora",
  "Groupe femmes"         = "solo_tena/vondrona_vehivavy",
  "Groupe entraide"       = "solo_tena/vondrona_fifanampiana",
  "Groupe religieux"      = "solo_tena/vondrona_fivavahana",
  "Gouvernement"          = "solo_tena/fitondrana",
  "ONG"                   = "solo_tena/ong",
  "Personnes handicapées" = "solo_tena/olona_fahasembanana",
  "Secteur national"      = "solo_tena/sehatra_nasionaly"
)
# Garder uniquement les colonnes qui existent dans les données
solo_cols <- solo_cols[unname(solo_cols) %in% colnames(df)]

df_solo <- df %>%
  summarise(across(any_of(unname(solo_cols)), ~ round(mean(. == 1, na.rm = TRUE)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(solo_cols)[match(col, unname(solo_cols))]) %>%
  filter(pct > 0)

fig_H1 <- graphique_lollipop(df_solo,
  "Acteurs représentant la communauté dans la réponse Mpox",
  couleur = pal[["bleu"]],
  subtitle = "Choix multiples")
sauvegarder(fig_H1, "H1_representants_communaute")

# --- H2. Avis consulté par les autorités — Q59 (donut) ---
pct_consulte <- round(mean(df$hevi_fahefana == "oui", na.rm = TRUE)*100, 1)
fig_H2 <- graphique_donut(pct_consulte, "Avis\nconsulté",
  "Avis consulté par les autorités sur le Mpox",
  couleur = pal[["vert"]])
sauvegarder(fig_H2, "H2_avis_consulte", w = 7, h = 7)

# --- H3. Connaissance mécanisme de feedback — Q62 (donut) ---
pct_feed <- round(mean(df$connait_feedback == 1, na.rm = TRUE)*100, 1)
fig_H3 <- graphique_donut(pct_feed, "Connaît\nle mécanisme",
  "Connaissance d'un mécanisme de feedback",
  couleur = pal[["pink"]])
sauvegarder(fig_H3, "H3_connaissance_feedback", w = 7, h = 7)

# --- H4. Canaux préférés pour le feedback — Q64 (lollipop) ---
feedback_cols <- c(
  "Visite à domicile"    = "soso_kevitra/mivantana_trano",
  "Contact direct autre" = "soso_kevitra/mivantana_hafa",
  "Téléphone"            = "soso_kevitra/telephone",
  "SMS"                  = "soso_kevitra/sms",
  "Email"                = "soso_kevitra/mail",
  "Courrier"             = "soso_kevitra/taratasy",
  "Réseaux sociaux"      = "soso_kevitra/serasera",
  "Boîte à suggestions"  = "soso_kevitra/boaty",
  "Autre"                = "soso_kevitra/hafa"
)
df_feedback <- df %>%
  summarise(across(any_of(unname(feedback_cols)), ~ round(mean(. == 1)*100, 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
  mutate(modalite = names(feedback_cols)[match(col, unname(feedback_cols))])

fig_H4 <- graphique_lollipop(df_feedback,
  "Canaux préférés pour transmettre feedback / plaintes sur le Mpox",
  couleur = pal[["pink"]],
  subtitle = "Choix multiples")
sauvegarder(fig_H4, "H4_canaux_feedback")

# --- H5. Acteurs de confiance dans l'engagement communautaire (itokisana) ---
itokisana_cols <- c(
  "Agent communautaire"           = "itokisana/anti_mpanahy",
  "Leader communautaire"          = "itokisana/mpitarika_fhm",
  "Leader religieux"              = "itokisana/fivavahana",
  "Guérisseur trad."              = "itokisana/mpitsabo_nentimpaharazana",
  "Groupe jeunes"                 = "itokisana/vondrona_tanora",
  "Enseignant"                    = "itokisana/mpampianatra",
  "Groupe femmes"                 = "itokisana/vondrona_vehivavy",
  "Autres associations"           = "itokisana/fikambanana_hafa",
  "Comité de santé"               = "itokisana/komity_fahasalamana",
  "Agent santé communautaire"     = "itokisana/mpiasan_fahasalamana_fhm",
  "Volontaires"                   = "itokisana/mpilatsaka_sitrapo",
  "Entreprise locale"             = "itokisana/tompona_orinasa",
  "Gouvernement"                  = "itokisana/fitondrana",
  "Groupe entraide"               = "itokisana/vondrona_fifanampiana"
)
itokisana_cols_pres <- itokisana_cols[unname(itokisana_cols) %in% names(df)]

if (length(itokisana_cols_pres) > 0) {
  df_itokisana <- df %>%
    summarise(across(any_of(unname(itokisana_cols_pres)),
                     ~ round(mean(. == 1, na.rm = TRUE) * 100, 1))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "pct") %>%
    mutate(modalite = names(itokisana_cols_pres)[
             match(col, unname(itokisana_cols_pres))]) %>%
    filter(pct > 0)

  if (nrow(df_itokisana) > 0) {
    fig_H5 <- graphique_lollipop(df_itokisana,
      "Acteurs de confiance pour l'engagement communautaire dans la réponse Mpox",
      couleur  = pal[["vert"]],
      subtitle = "Structures identifiées comme moteurs de la participation — Choix multiples")
    sauvegarder(fig_H5, "H5_acteurs_confiance_engagement")
  }
}

# =============================================================================
# SECTION I — TABLEAU RÉCAPITULATIF DES INDICATEURS CLÉS
# =============================================================================
message("\n=== SECTION I : TABLEAU RÉCAPITULATIF ===")

tableau_recapitulatif <- tibble(
  Theme = c(
    rep("A - Connaissance", 5),
    rep("B - Perception du risque", 5),
    rep("C - Pratiques", 3),
    rep("C - Accès aux soins", 4),
    rep("D - Communication", 3),
    rep("E - Vaccination", 4),
    rep("F - Satisfaction", 2),
    rep("G - Stigmatisation", 2),
    rep("H - Engagement communautaire", 3)
  ),
  Indicateur = c(
    "% ayant entendu parler du Mpox (Q1)",
    "% connaissant ≥2 modes de transmission corrects (Q3)",
    "% connaissant ≥2 symptômes corrects (Q4)",
    "% connaissant ≥2 mesures de prévention correctes (Q5)",
    "% avec un score de connaissance élevé (3/3)",
    "% croyant que le Mpox est réel (Q6)",
    "% croyant que le Mpox est présent dans leur communauté (Q8)",
    "% adhérant à des rumeurs sur l'origine du Mpox (Q7)",
    "% adhérant à la rumeur sur la raison de la communication du Mpox (Q8)",
    "% très/assez inquiets de contracter le Mpox (Q11)",
    "% lavant les mains avec eau+savon (Q15)",
    "% pratiquant ≥2 moments critiques de lavage (Q14)",
    "% utilisant le savon pour le lavage des mains (Q15)",
    "% qui iraient au CSB en cas de symptômes Mpox (Q25)",
    "% connaissant où recevoir un traitement (Q22)",
    "% avec accès potentiel aux soins (connaît ET irait)",
    "Distance médiane au CSB (minutes) (Q24)",
    "% trouvant les informations reçues utiles (Q28)",
    "% exposés à des rumeurs sur le vaccin (Q37)",
    "% ayant la radio comme source principale d'info (Q27)",
    "% connaissant l'existence du vaccin Mpox (Q35)",
    "% acceptant de se faire vacciner (Q41)",
    "% nécessitant une autorisation pour se vacciner (Q43)",
    "% ayant peur des effets secondaires comme frein (Q38)",
    "% satisfaits de l'équipe de coordination (Q50)",
    "% satisfaits des soins reçus au CSB (Q52)",
    "% ayant observé de la discrimination (Q56)",
    "% estimant difficile de parler du Mpox (Q54)",
    "% dont l'avis a été consulté par les autorités (Q59)",
    "% connaissant un mécanisme de feedback (Q62)",
    "% préférant le SMS / téléphone pour le feedback (Q64)"
  ),
  Valeur = c(
    round(mean(df$mpox_nandre == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$ind_trans_2plus, na.rm = TRUE)*100, 1),
    round(mean(df$ind_symp_2plus,  na.rm = TRUE)*100, 1),
    round(mean(df$ind_prev_2plus,  na.rm = TRUE)*100, 1),
    round(mean(df$niveau_connaissance == "Élevé (3/3)", na.rm = TRUE)*100, 1),
    round(mean(df$avy_aiza == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$misy_mpox == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$croit_rumeur_origine == 1, na.rm = TRUE)*100, 1),
    round(mean(df$misy_mpox == "Non", na.rm = TRUE)*100, 1),
    round(mean(df$fanahiana %in% c("manahy_mafy","manahy_be_dia_be"), na.rm = TRUE)*100, 1),
    round(mean(df$lavage_mains_savon == 1, na.rm = TRUE)*100, 1),
    round(mean(df$bonne_pratique_lavage == 1, na.rm = TRUE)*100, 1),
    round(mean(df$`manasa_tanana/rano_savony` == 1, na.rm = TRUE)*100, 1),
    round(mean(df$manatona_toby == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$toerana_mitsabo == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$acces_soins_ok == 1, na.rm = TRUE)*100, 1),
    median(df$min_toby, na.rm = TRUE),
    round(mean(df$mahasoa_vaovao == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$vaovao_ratsy == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$`vaovao/radio` == 1, na.rm = TRUE)*100, 1),
    round(mean(df$vaksiny == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$vaksiny_vonona == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$mahazo_lalana == "Oui", na.rm = TRUE)*100, 1),
    round(mean(df$`manakana/voka_dratsy` == 1, na.rm = TRUE)*100, 1),
    round(mean(df$ekipa == "afapo", na.rm = TRUE)*100, 1),
    round(mean(df$fitsaboana_afapo == "afapo", na.rm = TRUE)*100, 1),
    round(mean(df$fihetsika_ratsy == "oui", na.rm = TRUE)*100, 1),
    round(mean(df$miresaka == "Non", na.rm = TRUE)*100, 1),
    round(mean(df$hevi_fahefana == "oui", na.rm = TRUE)*100, 1),
    round(mean(df$connait_feedback == 1, na.rm = TRUE)*100, 1),
    round(mean(df$`soso_kevitra/sms` == 1 | df$`soso_kevitra/telephone` == 1,
               na.rm = TRUE)*100, 1)
  ),
  Unite = c(rep("%", 16), "min", rep("%", 14))
)

chemin_recap <- file.path(DOSSIER_SORTIES, "tableau_indicateurs_cles.xlsx")
write_xlsx(tableau_recapitulatif, chemin_recap)
message(sprintf("  Tableau récapitulatif exporté : %s", chemin_recap))
print(tableau_recapitulatif, n = Inf)

# =============================================================================
# SECTION J — ANALYSES TRANSVERSALES : SLOPE CHARTS
# =============================================================================
message("\n=== SECTION J : ANALYSES TRANSVERSALES ===")

# --- J1. GAP connaissance → pratique (barres groupées verticales) ---
df_j1 <- tibble(
  indicateur = c("Lavage mains\ncomme prévention",
                 "Lavage avec\neau + savon",
                 "≥2 moments\ncritiques"),
  Connaissance = c(
    round(mean(df$`fisorohana/manasa_tanana` == 1, na.rm = TRUE)*100, 1),
    round(mean(df$`fisorohana/manasa_tanana` == 1, na.rm = TRUE)*100, 1),
    round(mean(df$`fisorohana/manasa_tanana` == 1, na.rm = TRUE)*100, 1)
  ),
  Pratique = c(
    round(mean(df$lavage_mains_savon       == 1, na.rm = TRUE)*100, 1),
    round(mean(df$lavage_mains_savon       == 1, na.rm = TRUE)*100, 1),
    round(mean(df$bonne_pratique_lavage    == 1, na.rm = TRUE)*100, 1)
  )
) %>%
  pivot_longer(c(Connaissance, Pratique), names_to = "etape", values_to = "pct") %>%
  mutate(etape = factor(etape, levels = c("Connaissance", "Pratique")))

fig_J1 <- ggplot(df_j1, aes(x = indicateur, y = pct, fill = etape)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.9,
           colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(0.7), vjust = -0.4,
            size = 3.8, fontface = "bold", colour = "#2C3E50") +
  scale_fill_manual(values = c("Connaissance" = pal[["bleu"]],
                               "Pratique"     = pal[["rouge"]]),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 110), labels = label_percent(scale = 1)) +
  labs(title    = "GAP : Connaissance vs Pratique du lavage des mains",
       subtitle = "Comparaison entre ce que les répondants savent et ce qu'ils pratiquent réellement",
       x = NULL, y = "% répondants") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", colour = "#2C3E50", size = 13),
    plot.subtitle   = element_text(colour = "#666666", size = 9),
    legend.position = "top",
    legend.text     = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x     = element_text(colour = "#2C3E50", size = 11)
  )
sauvegarder(fig_J1, "J1_gap_connaissance_pratique", w = 9, h = 6)

# --- J2. Parcours vaccinal : 3 indicateurs en barres horizontales ---
df_j2 <- tibble(
  indicateur = factor(
    c("Connaît le vaccin Mpox",
      "A entendu des rumeurs\nsur le vaccin",
      "Intention de se\nvacciner"),
    levels = rev(c("Connaît le vaccin Mpox",
                   "A entendu des rumeurs\nsur le vaccin",
                   "Intention de se\nvacciner"))
  ),
  pct = c(
    round(mean(df$vaksiny         == "Oui", na.rm = TRUE) * 100, 1),
    round(mean(df$vaovao_ratsy    == "Oui", na.rm = TRUE) * 100, 1),
    round(mean(df$vaksiny_vonona  == "Oui", na.rm = TRUE) * 100, 1)
  ),
  couleur = c(pal[["bleu"]], pal[["rouge"]], pal[["vert"]])
)

fig_J2 <- ggplot(df_j2, aes(x = pct, y = indicateur, fill = indicateur)) +
  geom_col(width = 0.55, alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.2,
            size = 4.5, fontface = "bold", colour = "#2C3E50") +
  scale_fill_manual(values = setNames(df_j2$couleur, df_j2$indicateur)) +
  scale_x_continuous(limits = c(0, 115), labels = label_percent(scale = 1)) +
  labs(title    = "Parcours vaccinal : connaissance, rumeurs et intention",
       subtitle = "Proportion de répondants pour chaque étape du parcours vaccinal Mpox",
       x = "% répondants", y = NULL,
       caption  = "Source : Enquête KAP Mpox — CRM Madagascar 2026") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", colour = "#2C3E50", size = 13),
    plot.subtitle   = element_text(colour = "#666666", size = 9),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y     = element_text(face = "bold", size = 11, colour = "#2C3E50")
  )
sauvegarder(fig_J2, "J2_parcours_vaccinal", w = 9, h = 6)

# =============================================================================
# SECTION K — TABLEAU DE BORD DONUT (6 indicateurs clés)
# =============================================================================
message("\n=== SECTION K : DASHBOARD DONUT ===")

d1 <- graphique_donut(
  round(mean(df$score_connaissance_total == 3, na.rm = TRUE) * 100),
  "Score KAP\ncomplet (3/3)",
  "Score connaissance global",
  pal[["vert"]]
)
d2 <- graphique_donut(
  round(mean(df$score_moments_lavage >= 3, na.rm = TRUE) * 100),
  "Lavage mains\n≥3 moments",
  "Pratique lavage mains",
  pal[["bleu"]]
)
d3 <- graphique_donut(
  round(mean(df$avy_aiza == "Oui", na.rm = TRUE) * 100),
  "Croient en\nla maladie",
  "Croyance réalité Mpox",
  pal[["pink"]]
)
d4 <- graphique_donut(
  round(mean(df$vaksiny_vonona == "Oui", na.rm = TRUE) * 100),
  "Intention\nde vaccin",
  "Acceptation vaccinale",
  pal[["violet"]]
)
d5 <- graphique_donut(
  round(mean(df$manatona_toby == "Oui", na.rm = TRUE) * 100),
  "Iraient\nau CSB",
  "Recours aux soins formels",
  pal[["pastel"]]
)
d6 <- graphique_donut(
  round(mean(df$fihetsika_ratsy == "Non", na.rm = TRUE) * 100),
  "Attitude\nde soutien",
  "Non-stigmatisation",
  pal[["vert"]]
)

fig_K <- (d1 | d2 | d3) / (d4 | d5 | d6) +
  plot_annotation(
    title    = "Tableau de bord KAP — Indicateurs clés Mpox",
    subtitle = "CRM Madagascar — Enquête 2026",
    caption  = "Source : Enquête KAP Mpox — CRM Madagascar 2026",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, colour = "#2C3E50", hjust = 0.5),
      plot.subtitle = element_text(size = 11, colour = "#666666", hjust = 0.5),
      plot.caption  = element_text(size = 8, colour = "#999999", hjust = 0.5)
    )
  )
sauvegarder(fig_K, "K_dashboard_indicateurs", w = 16, h = 10)

# =============================================================================
# FIN DU SCRIPT
# =============================================================================
message("
╔══════════════════════════════════════════════════════════╗
║   ANALYSE KAP MPOX — TERMINÉE AVEC SUCCÈS               ║
║   CRM Madagascar 2026                                    ║
║   Figures sauvegardées dans : sorties/figures/           ║
║   Tableau de bord : sorties/tableau_indicateurs_cles.xlsx║
╚══════════════════════════════════════════════════════════╝
")
