# 📊 Rapport d'Analyse MPOX - Guide Rapide

## 🎯 Résumé

Ce projet génère automatiquement un **rapport HTML complet** avec :
- ✅ Logo Croix-Rouge Malagasy
- ✅ Tous les graphiques d'analyse (A, B, D, E, F, G, H)
- ✅ Descriptions détaillées de chaque graphique
- ✅ Interprétations contextualisées
- ✅ Boutons de téléchargement PNG pour chaque graphique

---

## ⚡ Démarrage Rapide (5 minutes)

### 1️⃣ Générer le rapport (une ligne de code !)

Ouvrez **RStudio** et exécutez :

```r
source("D:/Avotra/asa/kobo/mpox/projet_mpox/06_generer_rapport_html.R")
```

**Résultat :** 
- ✅ Tous les graphiques générés dans `sorties/figures/`
- ✅ Rapport HTML créé : `sorties/05_rapport_graphiques_analyses.html`

### 2️⃣ Voir le rapport

Double-cliquez sur `05_rapport_graphiques_analyses.html` pour l'ouvrir dans votre navigateur.

### 3️⃣ Partager ou uploader

Le rapport est prêt à être :
- 📧 Envoyé par email
- 🌐 Uploadé sur GitHub
- 🖨️ Imprimé en PDF (Ctrl+P depuis le navigateur)
- 📊 Présenté aux stakeholders

---

## 📁 Structure des fichiers

```
projet_mpox/
├── 02_analyse_mpox.R                          # Script d'analyse (génère graphiques)
├── 05_rapport_graphiques_analyses.Rmd         # Rapport template (NEW)
├── 06_generer_rapport_html.R                  # Script de génération (NEW)
├── GUIDE_GITHUB.md                            # Guide complet GitHub (NEW)
├── README_RAPPORT.md                          # Ce fichier (NEW)
│
├── sorties/
│   ├── 05_rapport_graphiques_analyses.html    # Rapport final (généré)
│   ├── mpox_nettoye.xlsx                      # Données nettoyées
│   └── figures/
│       ├── A1_connaissance_mpox.png
│       ├── A2_mpox_communaute.png
│       ├── B1_facteurs_vulnerabilite.png
│       ├── D1_sources_information.png
│       ├── D4_sources_confiance_lollipop.png
│       ├── D5_langue_souhaitee.png
│       ├── E1_connaissance_vaccin.png
│       ├── E2_intention_vaccination.png
│       ├── F1_groupes_discrimination.png
│       ├── G3_groupes_discrimination.png
│       ├── G4_solutions_stigmatisation.png
│       ├── H1_representants_communaute.png
│       ├── H2_avis_consulte.png
│       ├── H3_connaissance_feedback.png
│       ├── H4_canaux_feedback.png
│       └── H5_acteurs_confiance_engagement.png
│
└── donnees/
    ├── mpox.xlsx                               # Données brutes
    └── dico_mpox.xlsx                          # Dictionnaire
```

---

## 🚀 Uploader sur GitHub (5 étapes)

### Étape 1 : Initialiser Git (première fois seulement)
```bash
cd D:\Avotra\asa\kobo\mpox\projet_mpox
git init
git config user.name "Votre Nom"
git config user.email "avotra@crmada.org"
```

### Étape 2 : Créer un dépôt sur GitHub
- Allez sur [github.com](https://github.com)
- Cliquez **"+"** → **"New repository"**
- Nommez-le `mpox-kap-report`
- Créez-le

### Étape 3 : Ajouter le remote
```bash
git remote add origin https://github.com/VotreUsername/mpox-kap-report.git
```

### Étape 4 : Commiter et pousser
```bash
git add .
git commit -m "Ajouter rapport HTML avec graphiques et descriptions MPOX"
git push -u origin main
```

### Étape 5 : Activer GitHub Pages
- Allez dans **Settings** → **Pages**
- Source : `main` branch, `/sorties` folder
- Cliquez **Save**
- ✅ Votre rapport est en ligne ! 🎉

**URL publique :** 
```
https://VotreUsername.github.io/mpox-kap-report/05_rapport_graphiques_analyses.html
```

---

## 📋 Checklist pour les mises à jour

Chaque fois que vous avez de nouvelles données :

- [ ] Exécuter `01_nettoyage_mpox.R` pour nettoyer les données
- [ ] Exécuter `06_generer_rapport_html.R` pour générer le rapport
- [ ] Vérifier que le rapport s'ouvre correctement
- [ ] Commiter et pousser sur GitHub
  ```bash
  git add sorties/
  git commit -m "Mise à jour rapport MPOX - $(date +%Y-%m-%d)"
  git push origin main
  ```

---

## 🎨 Customisation

### Changer les couleurs
Modifiez `05_rapport_graphiques_analyses.Rmd` :
```r
couleur  = pal[["rouge"]]   # Ou "vert", "bleu", "orange"
```

### Ajouter des sections
Copiez-collez une section existante et modifiez :
```markdown
## Nouvelle Section

<div class="graph-container">
<div class="graph-wrapper">
<!-- Insérer image PNG ici -->
</div>
<div class="description">...</div>
<div class="interpretation">...</div>
</div>
```

### Changer le logo
Dans `05_rapport_graphiques_analyses.Rmd`, modifiez :
```r
LOGO <- "Votre/Nouveau/Chemin/logo.png"
```

---

## ❓ FAQ

### Q : Comment télécharger les graphiques ?
**R :** Cliquez sur le bouton "⬇️ Télécharger PNG" sous chaque graphique dans le rapport HTML.

### Q : Puis-je modifier le texte des descriptions ?
**R :** Oui ! Éditez `05_rapport_graphiques_analyses.Rmd` dans RStudio ou un éditeur de texte.

### Q : Le rapport met-il à jour automatiquement ?
**R :** Non, vous devez exécuter `06_generer_rapport_html.R` quand vous avez de nouvelles données.

### Q : Puis-je imprimer le rapport ?
**R :** Oui ! Ouvrez le HTML dans le navigateur et appuyez sur Ctrl+P, puis "Sauvegarder en PDF".

### Q : Comment partager le rapport ?
**R :** 
- 📧 Email : Envoyez le fichier HTML
- 🌐 Web : Uploadez sur GitHub Pages (voir guide)
- 📊 Présentation : Ouvrez dans le navigateur et présentez

---

## 🔗 Pour plus de détails

Consultez les guides complets :
- **GitHub** : Voir `GUIDE_GITHUB.md`
- **Installation packages** : Voir `CLAUDE.md`
- **Scripts d'analyse** : Voir `02_analyse_mpox.R`

---

## 👥 Support

Pour des questions ou problèmes :
- 📧 Contactez : avotra@crmada.org
- 📝 Consultez : GUIDE_GITHUB.md
- 💡 Vérifiez : CLAUDE.md (architecture du projet)

---

**Dernière mise à jour :** 2026-05-11  
**Version :** 1.0  
**Auteur :** Analyste de données CRM Madagascar
