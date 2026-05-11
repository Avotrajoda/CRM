# ── Génération des rapports HTML et PDF ──────────────────────────────────────
# Script pour rendre le rapport Rmd en HTML et PDF
# Date : 2026-04-26

library(rmarkdown)

# ── Fonctions de rendu ───────────────────────────────────────────────────────
render_html <- function(input_file, output_path) {
  dir_output <- dirname(output_path)
  file_output <- basename(output_path)

  if (!dir.exists(dir_output)) {
    dir.create(dir_output, recursive = TRUE)
  }

  cat(sprintf("📄 Génération HTML : %s\n", output_path))
  render(input_file,
         output_format = "html_document",
         output_file = file_output,
         output_dir = dir_output,
         quiet = FALSE)
  cat("✅ HTML généré avec succès\n\n")
}

render_pdf <- function(input_file, output_path) {
  dir_output <- dirname(output_path)
  file_output <- basename(output_path)

  if (!dir.exists(dir_output)) {
    dir.create(dir_output, recursive = TRUE)
  }

  cat(sprintf("📑 Génération PDF : %s\n", output_path))
  render(input_file,
         output_format = "pdf_document",
         output_file = file_output,
         output_dir = dir_output,
         quiet = FALSE)
  cat("✅ PDF généré avec succès\n\n")
}

# ── Exécution ────────────────────────────────────────────────────────────────
cat("▶ Lancement de la génération des rapports...\n\n")

# Générer les deux formats
render_html("03_rapport_qualite.Rmd", "sorties/rapport_mpox_20260426.html")
render_pdf("03_rapport_qualite.Rmd", "sorties/rapport_mpox_20260426.pdf")

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("✅ Les deux rapports ont été générés avec succès :\n")
cat("  📄 HTML : sorties/rapport_mpox_20260426.html\n")
cat("  📑 PDF  : sorties/rapport_mpox_20260426.pdf\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
