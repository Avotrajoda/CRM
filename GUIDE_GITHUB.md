# Guide : Générer et Publier le Rapport HTML sur GitHub

## 📊 PARTIE 1 : Générer le Rapport HTML Localement

### Étape 1.1 : Préparer vos données
```r
# Assurez-vous que le script de nettoyage a été exécuté
# Fichier requis : sorties/mpox_nettoye.xlsx
```

### Étape 1.2 : Générer les graphiques et le rapport
```r
# Dans RStudio, exécutez ce script qui fera tout automatiquement :
source("D:/Avotra/asa/kobo/mpox/projet_mpox/06_generer_rapport_html.R")
```

**Cela va :**
1. ✅ Exécuter `02_analyse_mpox.R` → génère tous les graphiques PNG
2. ✅ Exécuter `05_rapport_graphiques_analyses.Rmd` → génère le rapport HTML

**Fichier généré :**
```
D:/Avotra/asa/kobo/mpox/projet_mpox/sorties/05_rapport_graphiques_analyses.html
```

### Étape 1.3 : Vérifier le rapport
Ouvrez `05_rapport_graphiques_analyses.html` dans votre navigateur pour vérifier :
- ✅ Le logo Croix-Rouge s'affiche
- ✅ Les graphiques s'affichent
- ✅ Les boutons "Télécharger PNG" fonctionnent
- ✅ Les descriptions et interprétations sont lisibles

---

## 🚀 PARTIE 2 : Préparer GitHub

### Étape 2.1 : Créer un dépôt GitHub (si ce n'est pas déjà fait)

#### Option A : Via le site GitHub.com (Interface Web)

1. Allez sur [github.com](https://github.com) et connectez-vous
2. Cliquez sur **"+"** (en haut à droite) → **"New repository"**
3. Configurez :
   - **Repository name** : `mpox-kap-report` (ou `avotra-mpox-analyses`)
   - **Description** : "Rapport KAP Mpox - Croix-Rouge Malagasy"
   - **Visibility** : Public (pour permettre aux autres de voir)
   - **Initialize with README** : ✅ Cochez cette case
4. Cliquez sur **"Create repository"**

#### Option B : Via Git en ligne de commande

```bash
# Créer un nouveau dépôt sur GitHub via l'interface web d'abord,
# puis cloner localement
```

### Étape 2.2 : Préparer votre répertoire local

Si vous avez déjà un dépôt git dans le projet :
```bash
cd D:\Avotra\asa\kobo\mpox\projet_mpox
git status
```

Sinon, initialisez git :
```bash
cd D:\Avotra\asa\kobo\mpox\projet_mpox
git init
git config user.name "Votre Nom"
git config user.email "avotra@crmada.org"
```

---

## 📤 PARTIE 3 : Uploader les fichiers sur GitHub

### Étape 3.1 : Ajouter les fichiers

```bash
cd D:\Avotra\asa\kobo\mpox\projet_mpox

# Ajouter le rapport HTML et les graphiques
git add sorties/05_rapport_graphiques_analyses.html
git add sorties/figures/
git add 05_rapport_graphiques_analyses.Rmd
git add 06_generer_rapport_html.R
git add GUIDE_GITHUB.md
```

### Étape 3.2 : Créer un commit

```bash
git commit -m "Ajouter rapport HTML complet avec graphiques et descriptions

- Rapport d'analyse MPOX avec logo CRM
- Tous les graphiques d'analyse avec descriptions
- Interprétations de chaque graphique
- Boutons de téléchargement PNG
- Guide GitHub pour future publication"
```

### Étape 3.3 : Ajouter le dépôt distant (si première fois)

```bash
# Remplacez "VotreUsername" par votre identifiant GitHub
git remote add origin https://github.com/VotreUsername/mpox-kap-report.git

# Vérifier que le remote est bien ajouté
git remote -v
```

### Étape 3.4 : Uploader sur GitHub

```bash
# Pour la première fois, utilisez -u pour créer le tracking
git push -u origin main

# Les fois suivantes :
git push origin main
```

---

## ✅ PARTIE 4 : Vérifier et Partager

### Étape 4.1 : Vérifier sur GitHub

1. Allez sur votre dépôt GitHub : `https://github.com/VotreUsername/mpox-kap-report`
2. Vérifiez que tous les fichiers sont présents :
   - `05_rapport_graphiques_analyses.html`
   - `sorties/figures/` (tous les PNG)
   - `sorties/mpox_nettoye.xlsx`
   - `README.md`

### Étape 4.2 : Publier le rapport en ligne

#### Option A : GitHub Pages (Gratuit & Facile)

1. Allez dans **Settings** → **Pages**
2. Sous "Build and deployment" :
   - **Source** : Deploy from a branch
   - **Branch** : main
   - **Folder** : /sorties
3. Cliquez **Save**
4. Attendez 1-2 minutes
5. GitHub va créer une URL publique : `https://VotreUsername.github.io/mpox-kap-report/`
6. Le rapport est accessible à : `https://VotreUsername.github.io/mpox-kap-report/05_rapport_graphiques_analyses.html`

#### Option B : Partager le lien direct du fichier

Sans GitHub Pages, partagez simplement :
```
https://github.com/VotreUsername/mpox-kap-report/blob/main/sorties/05_rapport_graphiques_analyses.html
```

(GitHub affichera une prévisualisation du HTML)

### Étape 4.3 : Partager le rapport

Une fois publié, partagez le lien avec :
- Les stakeholders CRM
- Les partenaires
- L'équipe de direction
- Les bailleurs de fonds

**Exemple de message :**
```
Rapport KAP Mpox maintenant disponible en ligne :
📊 https://votreusername.github.io/mpox-kap-report/05_rapport_graphiques_analyses.html

Le rapport inclut :
✅ Tous les graphiques d'analyse avec descriptions
✅ Interprétations contextualisées
✅ Téléchargement PNG pour présentation
✅ Logo et branding CRM
```

---

## 🔄 PARTIE 5 : Mises à jour futures

### Pour mettre à jour le rapport avec de nouvelles données :

```bash
# 1. Régénérer le rapport localement
source("D:/Avotra/asa/kobo/mpox/projet_mpox/06_generer_rapport_html.R")

# 2. Commiter et pousser vers GitHub
git add sorties/
git commit -m "Mise à jour : rapport MPOX avec données du [DATE]"
git push origin main

# 3. GitHub Pages se mettra à jour automatiquement (1-2 min)
```

---

## 🆘 Dépannage

### Problème : "fatal: not a git repository"
```bash
# Solution : Initialiser git
git init
git config user.name "Votre Nom"
git config user.email "avotra@crmada.org"
```

### Problème : "Permission denied" lors du push
```bash
# Solution 1 : Utiliser HTTPS (plus simple)
git remote remove origin
git remote add origin https://github.com/VotreUsername/mpox-kap-report.git

# Solution 2 : Configurer un token GitHub
# https://github.com/settings/tokens
```

### Problème : Les graphiques ne s'affichent pas dans GitHub Pages
```bash
# Solution : Vérifier que les chemins des fichiers PNG sont corrects
# Dans le Rmd, utilisez des chemins relatifs ou absolus
# Example : file.path(DIR_FIGURES, "A1_connaissance_mpox.png")
```

### Problème : Le rapport HTML ne charge pas bien
```bash
# Solution : Régénérer le rapport
source("D:/Avotra/asa/kobo/mpox/projet_mpox/06_generer_rapport_html.R")
```

---

## 📚 Ressources supplémentaires

- [Aide GitHub Desktop](https://docs.github.com/en/desktop/installing-and-configuring-github-desktop)
- [Aide GitHub Pages](https://docs.github.com/en/pages)
- [Aide R Markdown](https://rmarkdown.rstudio.com/)

---

## 💡 Conseils professionnels

1. **Nommage des commits** : Soyez descriptif
   - ✅ Bon : "Ajouter rapport HTML avec graphiques analysés"
   - ❌ Mauvais : "fix"

2. **README.md** : Créez un README expliquant le projet
   ```markdown
   # Rapport KAP Mpox
   Croix-Rouge Malagasy 2026
   
   ## Fichiers
   - `05_rapport_graphiques_analyses.html` : Rapport complet
   - `sorties/figures/` : Graphiques individuels en PNG
   ```

3. **Confidentialité** : Ne committez JAMAIS
   - Les données brutes avec données personnelles
   - Les credentials ou tokens API
   - Créez un `.gitignore` pour ces fichiers

4. **Fréquence des updates** : Mettez à jour le rapport
   - ✅ Chaque mois avec nouvelles données
   - ✅ Après corrections majeures
   - ❌ N'exceédez pas une version par jour

---

**Questions ?** Contactez le responsable données de la CRM
📧 avotra@crmada.org
