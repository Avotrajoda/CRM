# =============================================================================
# SCRIPT 01 : NETTOYAGE DES DONNÉES - ENQUÊTE KAP MPOX
# =============================================================================
# Description : Ce script nettoie les données brutes de l'enquête KAP Mpox
#               collectées via KoboToolbox.
# Auteur      : Analyste de données - CRM Madagascar
# Date        : 2026-04-22
# Données     : mpox.xlsx (feuille "Formulaire Mpox")
#               dico_mpox.xlsx (feuille "survey" et "choices")
# =============================================================================

# -----------------------------------------------------------------------------
# 0. CHARGEMENT DES PACKAGES
# -----------------------------------------------------------------------------
# Installer les packages manquants si nécessaire :
# install.packages(c("readxl", "dplyr", "tidyr", "stringr", "lubridate",
#                    "janitor", "writexl", "here"))
{
  library(readxl)      # Lecture des fichiers Excel
  library(dplyr)       # Manipulation de données
  library(tidyr)       # Pivot et restructuration
  library(stringr)     # Manipulation de chaînes de caractères
  library(lubridate)   # Gestion des dates
  library(janitor)     # Nettoyage des noms de colonnes et tabulations
  library(writexl)     # Export Excel
  library(here)
}
        # Gestion des chemins de fichiers

# -----------------------------------------------------------------------------
# 1. DÉFINITION DES CHEMINS
# -----------------------------------------------------------------------------
# Adapter ces chemins à votre environnement de travail

DOSSIER_DONNEES  <- here("D:/Avotra/asa/kobo/mpox/projet_mpox/donnees")   # Dossier contenant les fichiers source
DOSSIER_SORTIES  <- here("D:/Avotra/asa/kobo/mpox/projet_mpox/sorties")   # Dossier pour les fichiers nettoyés

# Créer les dossiers de sortie s'ils n'existent pas
if (!dir.exists(DOSSIER_SORTIES)) dir.create(DOSSIER_SORTIES, recursive = TRUE)

# Chemins des fichiers
FICHIER_DONNEES  <- file.path(DOSSIER_DONNEES, "mpox.xlsx")
FICHIER_DICO     <- file.path(DOSSIER_DONNEES, "dico_mpox.xlsx")

# -----------------------------------------------------------------------------
# 2. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------

message("=== Chargement des données brutes ===")

# Données principales
donnees_brutes <- read_excel(FICHIER_DONNEES, sheet = "Formulaire Mpox")

# Dictionnaire de données
dico_survey  <- read_excel(FICHIER_DICO, sheet = "survey")
dico_choices <- read_excel(FICHIER_DICO, sheet = "choices")

message(sprintf("Données chargées : %d observations, %d variables",
                nrow(donnees_brutes), ncol(donnees_brutes)))

# -----------------------------------------------------------------------------
# 3. APERÇU INITIAL DES DONNÉES
# -----------------------------------------------------------------------------

apercu_initial <- list(
  n_observations   = nrow(donnees_brutes),
  n_variables      = ncol(donnees_brutes),
  periode_collecte = range(donnees_brutes$date, na.rm = TRUE),
  regions          = table(donnees_brutes$region),
  taux_completion  = round(mean(complete.cases(donnees_brutes)) * 100, 1)
)

message("--- Aperçu initial ---")
message(sprintf("  Observations      : %d", apercu_initial$n_observations))
message(sprintf("  Variables         : %d", apercu_initial$n_variables))
message(sprintf("  Taux de complétion des lignes : %.1f%%", apercu_initial$taux_completion))

# -----------------------------------------------------------------------------
# 4. TRAVAIL SUR UNE COPIE DES DONNÉES
# -----------------------------------------------------------------------------

df <- donnees_brutes

# Renommer les colonnes métadonnées KoboToolbox pour lisibilité
df <- df %>%
  rename(
    id_kobo          = `_id`,
    uuid             = `_uuid`,
    heure_soumission = `_submission_time`,
    statut           = `_status`,
    soumis_par       = `_submitted_by`
  )

# -----------------------------------------------------------------------------
# 5. NETTOYAGE DES TYPES DE VARIABLES
# -----------------------------------------------------------------------------

message("=== Nettoyage des types de variables ===")

# 5.1 Dates
df <- df %>%
  mutate(
    date             = as.Date(date),
    heure_soumission = as.POSIXct(heure_soumission),
    # Créer des variables temporelles dérivées
    mois_collecte    = month(date, label = TRUE, abbr = FALSE),
    semaine_collecte = week(date)
  )

# 5.2 Variables ordinales / catégorielles principales
variables_oui_non <- c("mpox_nandre", "avy_aiza", "misy_mpox",
                       "toerana_mitsabo", "manatona_toby", "mahasoa_vaovao",
                       "vaksiny", "vaovao_ratsy", "vaksiny_vonona",
                       "mahazo_lalana", "miresaka", "fihetsika_ratsy",
                       "hevi_fahefana", "fitarainana")

# Recodage en facteur ordonné
df <- df %>%
  mutate(across(all_of(variables_oui_non), function(x) {
    factor(x,
           levels = c("oui", "non", "tsy_azoko"),
           labels = c("Oui", "Non", "Ne sait pas"))
  }))

# 5.3 Variable niveau de peur (fanahiana) - échelle ordinale
df <- df %>%
  mutate(
    fanahiana_ord = factor(fanahiana,
                           levels = c("tsy_manahy", "manahy_kely",
                                      "manahy_mafy", "manahy_be_dia_be"),
                           labels = c("Pas du tout inquiet",
                                      "Un peu inquiet",
                                      "Assez inquiet",
                                      "Très inquiet"),
                           ordered = TRUE)
  )

# 5.4 Distance au centre de santé (min_toby) — variable numérique
df <- df %>%
  mutate(
    min_toby = as.numeric(min_toby),
    # Catégoriser la distance
    distance_cat = case_when(
      min_toby <= 15             ~ "≤ 15 min",
      min_toby > 15 & min_toby <= 30 ~ "16-30 min",
      min_toby > 30 & min_toby <= 60 ~ "31-60 min",
      min_toby > 60              ~ "> 60 min",
      TRUE                       ~ NA_character_
    ),
    distance_cat = factor(distance_cat,
                          levels = c("≤ 15 min", "16-30 min",
                                     "31-60 min", "> 60 min"),
                          ordered = TRUE)
  )

# -----------------------------------------------------------------------------
# 6. GESTION DES VALEURS MANQUANTES
# -----------------------------------------------------------------------------

message("=== Analyse des valeurs manquantes ===")

# 6.1 Calculer le taux de manquants par variable
valeurs_manquantes <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_manquants") %>%
  mutate(
    pct_manquants = round(n_manquants / nrow(df) * 100, 1),
    alerte        = case_when(
      pct_manquants == 0            ~ "OK",
      pct_manquants > 0  & pct_manquants <= 5  ~ "Faible (≤5%)",
      pct_manquants > 5  & pct_manquants <= 20 ~ "Modéré (5-20%)",
      pct_manquants > 20            ~ "Élevé (>20%)"
    )
  ) %>%
  arrange(desc(pct_manquants))

# Afficher un résumé
message(sprintf("  Variables sans manquants : %d/%d",
                sum(valeurs_manquantes$n_manquants == 0),
                nrow(valeurs_manquantes)))
message(sprintf("  Variables avec >20%% manquants : %d",
                sum(valeurs_manquantes$pct_manquants > 20)))

# 6.2 Imputation contextuelle pour les questions conditionnelles
# Ex : mifindra n'est posé que si mpox_nandre = "Oui" → NA normal si "Non"
df <- df %>%
  mutate(
    # Marquer les NA conditionnels vs NA réels
    mifindra_na_type = case_when(
      mpox_nandre == "Non" & is.na(mifindra) ~ "Non applicable",
      mpox_nandre == "Oui" & is.na(mifindra) ~ "Manquant",
      !is.na(mifindra)                        ~ "Renseigné"
    ),
    # Autres variables conditionnelles
    hevitra_na_type = case_when(
      avy_aiza == "Non" & is.na(hevitra) ~ "Non applicable",
      avy_aiza == "Oui" & is.na(hevitra) ~ "Manquant",
      !is.na(hevitra)                    ~ "Renseigné"
    )
  )

# 6.3 Variables binaires (0/1) : les NA signifient souvent 0 pour les select_multiple
# On garde les NA pour les variables agrégées (string), mais les 0/1 sont déjà complets
cols_binary <- df %>%
  select(contains("/")) %>%
  names()

message(sprintf("  Variables binaires (select_multiple) : %d", length(cols_binary)))

# Vérifier cohérence : si la variable parent est renseignée, les binaires ne devraient pas être NA
# (dans ce jeu de données, les binaires sont déjà complètes)

# -----------------------------------------------------------------------------
# 7. DÉTECTION ET TRAITEMENT DES VALEURS ABERRANTES
# -----------------------------------------------------------------------------

message("=== Détection des valeurs aberrantes ===")

# 7.1 Variable numérique : min_toby (distance en minutes)
stats_distance <- df %>%
  summarise(
    n        = n(),
    min      = min(min_toby, na.rm = TRUE),
    q1       = quantile(min_toby, 0.25, na.rm = TRUE),
    mediane  = median(min_toby, na.rm = TRUE),
    moyenne  = round(mean(min_toby, na.rm = TRUE), 1),
    q3       = quantile(min_toby, 0.75, na.rm = TRUE),
    max      = max(min_toby, na.rm = TRUE),
    ecart_type = round(sd(min_toby, na.rm = TRUE), 1)
  )

message("  Distance au CSB (minutes) :")
print(stats_distance)

# Détection outliers par la règle IQR
iqr_distance <- IQR(df$min_toby, na.rm = TRUE)
borne_basse   <- quantile(df$min_toby, 0.25, na.rm = TRUE) - 1.5 * iqr_distance
borne_haute   <- quantile(df$min_toby, 0.75, na.rm = TRUE) + 1.5 * iqr_distance

df <- df %>%
  mutate(
    min_toby_outlier = min_toby < borne_basse | min_toby > borne_haute,
    # Winsorisation : remplacer les outliers par les bornes
    min_toby_winsorise = pmax(pmin(min_toby, borne_haute), borne_basse)
  )

n_outliers_distance <- sum(df$min_toby_outlier, na.rm = TRUE)
message(sprintf("  Outliers détectés (distance) : %d observations",
                n_outliers_distance))

# 7.2 Contrôle de cohérence des dates
df <- df %>%
  mutate(
    date_coherente = date >= as.Date("2026-02-23") &
                     date <= as.Date("2026-03-03"),
    date_anomalie  = !date_coherente
  )

n_dates_hors_periode <- sum(df$date_anomalie, na.rm = TRUE)
if (n_dates_hors_periode > 0) {
  message(sprintf("  ATTENTION : %d observations hors période de collecte prévue",
                  n_dates_hors_periode))
} else {
  message("  Dates : aucune anomalie détectée")
}

# 7.3 Doublons potentiels
doublons <- df %>%
  group_by(uuid) %>%
  filter(n() > 1) %>%
  nrow()

message(sprintf("  Doublons (uuid) : %d", doublons))

# -----------------------------------------------------------------------------
# 8. CRÉATION DES SCORES DE CONNAISSANCE (KAP)
# -----------------------------------------------------------------------------

message("=== Calcul des scores de connaissance ===")

# --- Réponses correctes scientifiquement validées ---

# TRANSMISSION correcte (Q3 - miparitaka)
# Modes corrects : olona_voa (contact personne infectée), ra (sang),
#                  diky (sécrétions), tanana_maloto (mains sales),
#                  firaisana_aranofo (contact sexuel)
cols_transmission_correctes <- c(
  "miparitaka/olona_voa",
  "miparitaka/ra",
  "miparitaka/diky",
  "miparitaka/tanana_maloto"
)

# SYMPTÔMES corrects (Q4 - fiseho)
# Corrects : atody_tarimo (éruption), manavy (fièvre), reraka (fatigue),
#            mandoa (maux de tête), marary_hoditra (lésions cutanées)
cols_symptomes_correctes <- c(
  "fiseho/atody_tarimo",
  "fiseho/manavy",
  "fiseho/reraka",
  "fiseho/mandoa",
  "fiseho/marary_hoditra"
)

# PRÉVENTION correcte (Q5 - fisorohana)
# Correctes : manasa_tanana (lavage mains), tsy_mifampikasoka (éviter contact),
#             manao_vaksiny (vaccination), atao_masaka_tsara_ny_sakafo (cuire)
cols_prevention_correctes <- c(
  "fisorohana/manasa_tanana",
  "fisorohana/tsy_mifampikasoka",
  "fisorohana/manao_vaksiny",
  "fisorohana/Atao_masaka_tsara_ny_sakafo"
)

# Calcul des scores
df <- df %>%
  mutate(
    # Nombre de modes de transmission corrects cités
    score_transmission = rowSums(pick(all_of(cols_transmission_correctes)),
                                  na.rm = TRUE),

    # Nombre de symptômes corrects cités
    score_symptomes    = rowSums(pick(all_of(cols_symptomes_correctes)),
                                  na.rm = TRUE),

    # Nombre de mesures de prévention correctes citées
    score_prevention   = rowSums(pick(all_of(cols_prevention_correctes)),
                                  na.rm = TRUE),

    # Indicateurs binaires (au moins 2 corrects)
    ind_trans_2plus  = as.integer(score_transmission >= 2),
    ind_symp_2plus   = as.integer(score_symptomes >= 2),
    ind_prev_2plus   = as.integer(score_prevention >= 2),

    # Score global de connaissance (0-3)
    score_connaissance_total = ind_trans_2plus + ind_symp_2plus + ind_prev_2plus,

    # Niveau de connaissance
    niveau_connaissance = case_when(
      score_connaissance_total == 3 ~ "Élevé (3/3)",
      score_connaissance_total == 2 ~ "Moyen (2/3)",
      score_connaissance_total <= 1 ~ "Faible (0-1/3)"
    ),
    niveau_connaissance = factor(niveau_connaissance,
                                  levels = c("Faible (0-1/3)",
                                             "Moyen (2/3)",
                                             "Élevé (3/3)"),
                                  ordered = TRUE)
  )

# Résumé des scores
message("  Distribution du niveau de connaissance :")
print(table(df$niveau_connaissance))

# -----------------------------------------------------------------------------
# 9. VARIABLES DÉRIVÉES SUPPLÉMENTAIRES
# -----------------------------------------------------------------------------

message("=== Création des variables dérivées ===")

# Vecteur défini AVANT le mutate (interdit à l'intérieur)
cols_moments_critiques <- c(
  "fahazarana_manasa_tanana/avy_wc",
  "fahazarana_manasa_tanana/alohan_mikarakara_sakafo",
  "fahazarana_manasa_tanana/alohan_sakafo",
  "fahazarana_manasa_tanana/rehefa_avy_nikasika_biby",
  "fahazarana_manasa_tanana/nifandray_marary"
)

df <- df %>%
  mutate(
    # 9.1 Connaissance de l'existence du vaccin
    connait_vaccin = case_when(
      vaksiny == "Oui" ~ 1L,
      vaksiny == "Non" ~ 0L,
      TRUE             ~ NA_integer_
    ),

    # 9.2 Acceptation vaccinale
    accepte_vaccin = case_when(
      vaksiny_vonona == "Oui" ~ 1L,
      vaksiny_vonona == "Non" ~ 0L,
      TRUE                    ~ NA_integer_
    ),

    # 9.3 Score de rumeurs (Q6/Q10) :
    # Croyance à une source fausse de la maladie
    rumeur_origine = case_when(
      !is.na(hevitra) ~ rowSums(
        select(., `hevitra/ozona`, `hevitra/lainga`,
               `hevitra/vazaha`, `hevitra/mpivarotena`,
               `hevitra/natiora`),
        na.rm = TRUE
      ),
      TRUE ~ NA_real_
    ),
    croit_rumeur_origine = as.integer(rumeur_origine > 0),

    # 9.4 Accès aux soins : connaît le lieu de traitement ET irait consulter
    acces_soins_ok = as.integer(
      toerana_mitsabo == "Oui" & manatona_toby == "Oui"
    ),

    # 9.5 Pratique de lavage de mains avec savon
    lavage_mains_savon = `manasa_tanana/rano_savony`,

    # 9.6 Moments critiques de lavage (Q14) : au moins 2 moments corrects
    score_moments_lavage = rowSums(
      select(., all_of(cols_moments_critiques)),
      na.rm = TRUE
    ),
    bonne_pratique_lavage = as.integer(score_moments_lavage >= 2),

    # 9.7 Stigmatisation perçue
    stigmatisation = as.integer(fihetsika_ratsy == "Oui"),

    # 9.8 Engagement communautaire (participe aux décisions)
    engagement_communautaire = as.integer(hevi_fahefana == "Oui"),

    # 9.9 Connaissance du mécanisme de feedback
    connait_feedback = as.integer(fitarainana == "Oui")
  )

# (cols_moments_critiques est un vecteur R, pas une colonne — rien à supprimer)

# -----------------------------------------------------------------------------
# 10. CONTRÔLE QUALITÉ FINAL
# -----------------------------------------------------------------------------

message("=== Contrôle qualité final ===")

# 10.1 Résumé des variables clés
resume_cles <- df %>%
  summarise(
    n_total                     = n(),
    pct_connait_mpox            = round(mean(mpox_nandre == "Oui",
                                            na.rm = TRUE) * 100, 1),
    pct_trans_2plus             = round(mean(ind_trans_2plus, na.rm = TRUE)*100, 1),
    pct_symp_2plus              = round(mean(ind_symp_2plus,  na.rm = TRUE)*100, 1),
    pct_prev_2plus              = round(mean(ind_prev_2plus,  na.rm = TRUE)*100, 1),
    pct_connaissance_elevee     = round(mean(niveau_connaissance == "Élevé (3/3)",
                                            na.rm = TRUE)*100, 1),
    pct_acceptation_vaccin      = round(mean(accepte_vaccin == 1,
                                            na.rm = TRUE)*100, 1),
    pct_lavage_savon            = round(mean(lavage_mains_savon == 1,
                                            na.rm = TRUE)*100, 1),
    pct_acces_soins             = round(mean(acces_soins_ok == 1,
                                            na.rm = TRUE)*100, 1),
    pct_stigmatisation          = round(mean(stigmatisation == 1,
                                            na.rm = TRUE)*100, 1),
    distance_mediane_csb_min    = median(min_toby, na.rm = TRUE)
  )

message("\n  === RÉSUMÉ DES INDICATEURS CLÉS ===")
print(t(resume_cles))

# 10.2 Vérification de la cohérence des scores
stopifnot(
  "Score transmission hors bornes [0-4]" =
    isTRUE(all(df$score_transmission >= 0 & df$score_transmission <= 4, na.rm = TRUE)),
  "Score symptômes hors bornes [0-5]" =
    isTRUE(all(df$score_symptomes >= 0 & df$score_symptomes <= 5, na.rm = TRUE)),
  "Score prévention hors bornes [0-4]" =
    isTRUE(all(df$score_prevention >= 0 & df$score_prevention <= 4, na.rm = TRUE)),
  "Score global hors bornes [0-3]" =
    isTRUE(all(df$score_connaissance_total >= 0 & df$score_connaissance_total <= 3, na.rm = TRUE))
)
message("  Cohérence des scores : OK ✓")

# -----------------------------------------------------------------------------
# 11. EXPORT DES DONNÉES NETTOYÉES
# -----------------------------------------------------------------------------

message("=== Export des données nettoyées ===")

# 11.1 Données complètes nettoyées
chemin_export_principal <- file.path(DOSSIER_SORTIES, "mpox_nettoye.xlsx")
write_xlsx(df, chemin_export_principal)
message(sprintf("  Données nettoyées exportées : %s", chemin_export_principal))

# 11.2 Rapport des valeurs manquantes
chemin_manquants <- file.path(DOSSIER_SORTIES, "rapport_valeurs_manquantes.xlsx")
write_xlsx(valeurs_manquantes, chemin_manquants)
message(sprintf("  Rapport valeurs manquantes  : %s", chemin_manquants))

# 11.3 Résumé des indicateurs clés
chemin_resume <- file.path(DOSSIER_SORTIES, "resume_indicateurs.xlsx")
write_xlsx(as.data.frame(t(resume_cles)), chemin_resume)
message(sprintf("  Résumé indicateurs exporté  : %s", chemin_resume))

# 11.4 Sauvegarde de l'environnement R pour le script d'analyse
chemin_rdata <- file.path(DOSSIER_SORTIES, "mpox_nettoye.RData")
save(df, dico_survey, dico_choices, valeurs_manquantes, resume_cles,
     file = chemin_rdata)
message(sprintf("  Environnement R sauvegardé  : %s", chemin_rdata))

message("\n=== NETTOYAGE TERMINÉ AVEC SUCCÈS ===")
message(sprintf("Données finales : %d observations × %d variables",
                nrow(df), ncol(df)))

