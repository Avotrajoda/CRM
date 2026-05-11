====================================================================
  PROJET : ANALYSE KAP MPOX — CRM MADAGASCAR 2026
  Scripts R de nettoyage et d'analyse
====================================================================

FICHIERS FOURNIS
-----------------
  01_nettoyage_mpox.R   → Nettoyage et préparation des données
  02_analyse_mpox.R     → Analyse, indicateurs et visualisations

DONNÉES REQUISES (à placer dans le sous-dossier "donnees/")
------------------------------------------------------------
  mpox.xlsx             → Données brutes KoboToolbox
  dico_mpox.xlsx        → Dictionnaire de données

STRUCTURE DE DOSSIERS RECOMMANDÉE
-----------------------------------
  projet_mpox/
  ├── donnees/
  │   ├── mpox.xlsx
  │   └── dico_mpox.xlsx
  ├── sorties/
  │   └── figures/        (créé automatiquement)
  ├── 00_README.txt
  ├── 01_nettoyage_mpox.R
  └── 02_analyse_mpox.R

ORDRE D'EXÉCUTION
------------------
  1. Ouvrir R ou RStudio dans le dossier "projet_mpox/"
  2. Exécuter : source("01_nettoyage_mpox.R")
  3. Exécuter : source("02_analyse_mpox.R")

PACKAGES REQUIS
----------------
  readxl, dplyr, tidyr, stringr, lubridate, janitor,
  writexl, ggplot2, scales, forcats, patchwork, purrr, here

  Installer tout en une fois :
  install.packages(c("readxl","dplyr","tidyr","stringr","lubridate",
                     "janitor","writexl","ggplot2","scales","forcats",
                     "patchwork","purrr","here"))

SORTIES PRODUITES PAR LE SCRIPT 01
-------------------------------------
  sorties/mpox_nettoye.xlsx                  → Données nettoyées
  sorties/rapport_valeurs_manquantes.xlsx    → Taux de manquants
  sorties/resume_indicateurs.xlsx            → Résumé des KPIs
  sorties/mpox_nettoye.RData                 → Environnement R

SORTIES PRODUITES PAR LE SCRIPT 02
-------------------------------------
  sorties/tableau_indicateurs_cles.xlsx      → Tableau de bord KAP
  sorties/figures/A2_transmission.png        → Modes de transmission
  sorties/figures/A3_symptomes.png           → Symptômes
  sorties/figures/A4_prevention.png          → Mesures de prévention
  sorties/figures/A5_score_connaissance.png  → Score global
  sorties/figures/B1_realite_mpox.png        → Croyance réalité maladie
  sorties/figures/B2_origines_percues.png    → Origines perçues/rumeurs
  sorties/figures/B3_niveau_peur.png         → Niveau d'inquiétude
  sorties/figures/B4_groupes_risque.png      → Groupes à risque perçus
  sorties/figures/C1_moments_lavage.png      → Moments lavage mains
  sorties/figures/C2_produits_lavage.png     → Produits lavage
  sorties/figures/C3_recours_soins.png       → Recours aux soins
  sorties/figures/C4_distance_csb.png        → Distance centre santé
  sorties/figures/C5_barrieres_acces.png     → Barrières accès soins
  sorties/figures/D1_canaux_communication.png→ Canaux info actuels/préférés
  sorties/figures/D2_sources_confiance.png   → Sources de confiance (CRM)
  sorties/figures/E1_vaccin_indicateurs.png  → Indicateurs vaccination
  sorties/figures/E2_raisons_refus_vaccin.png→ Raisons refus vaccin
  sorties/figures/E3_autonomie_vaccinale.png → Autonomie décisionnelle
  sorties/figures/F1_satisfaction_services.png → Satisfaction services
  sorties/figures/G1_groupes_discrimination.png → Stigmatisation
  sorties/figures/H1_representants_communaute.png → Engagement
  sorties/figures/H2_canaux_feedback.png     → Canaux de feedback
  sorties/figures/J1_gap_connaissance_pratique.png → GAP KAP

CORRESPONDANCE AVEC LE PLAN D'ANALYSE
---------------------------------------
  Section A → Connaissance (Q1-Q5)
  Section B → Perception du risque (Q6-Q13)
  Section C → Pratiques et accès aux soins (Q14-Q26)
  Section D → Communication (Q27-Q34)
  Section E → Vaccination (Q35-Q46)
  Section F → Satisfaction (Q47-Q53)
  Section G → Stigmatisation (Q50-Q54)
  Section H → Engagement communautaire (Q55-Q64)
  Section J → Analyse transversale GAP connaissance/pratique

====================================================================
  Contact : avotra@crmada.org — CRM Madagascar
====================================================================
