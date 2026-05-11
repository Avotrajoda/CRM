# =============================================================================
# SCRIPT 06 : GÉNÉRER RAPPORT HTML D'ANALYSE MPOX
# =============================================================================
# Description : Ce script génère le rapport HTML complet avec tous les
#               graphiques, descriptions et interprétations.
# Auteur      : Analyste de données - CRM Madagascar
# Date        : 2026-05-11
# =============================================================================

library(rmarkdown)
library(here)

# ── Chemins ───────────────────────────────────────────────────────────────
DOSSIER_PROJET <- "D:/Avotra/asa/kobo/mpox/projet_mpox"
FICHIER_RMD    <- file.path(DOSSIER_PROJET, "05_rapport_graphiques_analyses.Rmd")
DOSSIER_SORTIES <- file.path(DOSSIER_PROJET, "sorties")

# ── Étape 1 : Exécuter le script d'analyse pour générer les graphiques ────
message("════════════════════════════════════════════════════════════════════════")
message("ÉTAPE 1 : Génération des graphiques")
message("════════════════════════════════════════════════════════════════════════")

source(file.path(DOSSIER_PROJET, "02_analyse_mpox.R"))

message("\n✅ Tous les graphiques ont été générés.")

# ── Étape 2 : Générer le rapport HTML ──────────────────────────────────────
message("\n════════════════════════════════════════════════════════════════════════")
message("ÉTAPE 2 : Génération du rapport HTML")
message("════════════════════════════════════════════════════════════════════════")

if (!file.exists(FICHIER_RMD)) {
  stop(sprintf("Fichier Rmd introuvable : %s", FICHIER_RMD))
}

# Générer le rapport HTML
output_file <- rmarkdown::render(
  FICHIER_RMD,
  output_format = "html_document",
  output_file = file.path(DOSSIER_SORTIES, "05_rapport_graphiques_analyses.html"),
  quiet = FALSE
)

message("\n════════════════════════════════════════════════════════════════════════")
message("✅ SUCCÈS !")
message("════════════════════════════════════════════════════════════════════════")
message(sprintf("Rapport HTML généré : %s", output_file))
message("\nLe rapport inclut :")
message("  • Logo Croix-Rouge Malagasy")
message("  • Tous les graphiques d'analyse")
message("  • Descriptions et interprétations détaillées")
message("  • Boutons de téléchargement PNG pour chaque graphique")
message("════════════════════════════════════════════════════════════════════════")
