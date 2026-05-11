---
name: Projet Mpox KAP Madagascar
description: Structure, conventions et variables clés du projet KAP Mpox CRM Madagascar 2026
type: project
---

Projet d'analyse KAP (Knowledge, Attitudes, Practices) sur le Mpox, conduit par CRM Madagascar en 2026.

**Chemins clés**
- Script nettoyage : `01_nettoyage_mpox.R`
- Script analyse   : `02_analyse_mpox.R` (script principal où ajouter les blocs de code)
- Données nettoyées : `sorties/mpox_nettoye.xlsx` (chargées dans `df`)
- Figures sauvegardées : `sorties/figures/`

**Fonctions utilitaires déjà définies dans 02_analyse_mpox.R**
- `sauvegarder(plot, nom, w, h)` — ggsave wrapper vers `sorties/figures/`
- `graphique_barres()`, `graphique_pie()`, `graphique_donut()`, `graphique_lollipop()`
- `tableau_freq()`, `pct_bin()`

**Palette définie comme `pal` (vecteur nommé)**
```r
pal <- c(rouge="#e71b3f", vert="#3fe58b", bleu="#3462cd",
         pink="#DB6A8F", violet="#8E44AD", pastel="#FBBFB8",
         jaune="#FCFE19", bleu_ciel="#B6D8F2")
```

**Variables de connaissance clés dans `df`**
- `mpox_nandre` : "Oui"/"Non"/"Ne sait pas"
- `ind_trans_2plus`, `ind_symp_2plus`, `ind_prev_2plus` : binaires 0/1
- `niveau_connaissance` : facteur ordonné "Faible (0-1/3)"/"Moyen (2/3)"/"Élevé (3/3)"
- Fausses croyances : colonnes avec `/` dans le nom (ex: `miparitaka/rano_maloto`)

**Why:** Projet de santé publique épidémiologique, rapports pour CRM Madagascar.
**How to apply:** Toujours intégrer le code comme blocs autonomes dans 02_analyse_mpox.R. Utiliser `any_of()` pour les colonnes à noms incertains. Respecter le style `theme_minimal` + palette `pal`.
