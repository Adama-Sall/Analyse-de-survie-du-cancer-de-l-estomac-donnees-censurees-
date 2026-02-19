# Analyse de survie du cancer de l’estomac (Données censurées) 

_Projet de données censurées

## Contexte et objectif

Ce dépôt contient le code et les résultats du projet intitulé  
**« Modélisation de la durée de survie des patients atteints d’un cancer de l’estomac »**.

L’objectif est :
- De modéliser la **durée de survie** de patients atteints d’un cancer de l’estomac.
- D’identifier les **facteurs pronostiques** associés à la survie (âge, sexe, traitement, antécédents familiaux, etc.).
- De tenir compte des **données censurées** pour proposer des modèles de survie pertinents (Kaplan-Meier, Cox).

## Données

- **Population étudiée** : 200 patients suivis pour un cancer de l’estomac.
- **Période de suivi** : jusqu’à 3600 jours.
- **Nombre d’événements (décès)** : 98.

Principales variables :
- `DureeSurvieJr` : durée de survie en jours (min 2, max 3600).
- `DECES` : statut (1 = décès observé, 0 = censuré).
- `AGE` : âge au diagnostic (26 à 82 ans).
- `DureeSymptom` : durée des symptômes (en mois).
- `SEXE` : 1 = homme (99), 2 = femme (101).
- `Traitement` : 0, 1, 2, 3 (schémas thérapeutiques différents).
- `AntFamil` : antécédents familiaux (0/1).[file:12]
- Autres variables cliniques éventuellement utilisées dans les modèles.

## Méthodologie

### 1. Analyse descriptive

- Résumés statistiques (min, Q1, médiane, moyenne, Q3, max) pour la durée de survie, l’âge, la durée des symptômes.
- Répartition des patients par sexe, type de traitement, classes d’âge.
- Visualisations (histogrammes, diagramme en camembert pour les classes d’âge, etc.).

### 2. Analyse de survie (Kaplan-Meier)

- Estimation de la **fonction de survie globale** (Kaplan-Meier) avec intervalles de confiance.
- Interprétation :
  - Probabilité de survie ≈ 98,5 % au début (2 jours).
  - ≈ 81,5 % à 30 jours, 75 % à 60 jours, 60,5 % à 1 an.
  - ≈ 51 % à 3600 jours (environ la moitié des patients survivent à la fin du suivi).

Analyses par sous-groupes :
- **Par sexe** (hommes vs femmes) :
  - Courbes proches, p-value du test de log-rank ≈ 0,5 → pas de différence significative.
- **Par type de traitement** (0, 1, 2, 3) :
  - Différences marquées entre les groupes.
  - Test de log-rank p < 0,0001 → différence significative entre les traitements.
- **Par classes d’âge** (25–45, 45–65, 65–82) :
  - Courbes visuellement différentes, mais p-value ≈ 0,13 → pas de différence significative au seuil 5 %.

### 3. Fonction de hasard instantané

- Estimation de la **fonction de hasard** (muhaz) pour décrire l’évolution du risque instantané de décès dans le temps.
- Profil observé :
  - Risque élevé au début du suivi, puis diminution et stabilisation.
  - Rehausse du risque après environ 2500 jours.

### 4. Modélisation par régression de Cox

Plusieurs modèles de Cox ont été ajustés pour étudier l’effet des covariables :

- **Modèle 1** : effet du traitement et de l’âge.
  - Traitement 1 et 3 : effets fortement protecteurs (HR ≪ 1, p < 0,001), réduction importante du risque de décès.
  - L’âge n’est pas significatif dans ce modèle (p > 0,05).

- **Modèle 2** : interaction Traitement × âge.
  - Hazard ratios des traitements < 1 (effet protecteur) mais non significatifs.
  - Interactions avec l’âge non significatives.

- **Modèle 3** : effet global du traitement.  
  - HR ≈ 0,78, réduction significative d’environ 22 % du risque de décès (p ≈ 0,03).

- **Modèle 4** : effet des antécédents familiaux.  
  - HR ≈ 4,48, effet significatif et défavorable : risque de décès environ 4,5 fois plus élevé en présence d’antécédents (p < 0,001).

- **Modèles 5–6** : combinaisons traitement, âge, antécédents familiaux, interactions, avec comparaison des modèles via AIC et tests d’ANOVA. 
  - Le modèle final retient l’effet fort des **antécédents familiaux** et un effet protecteur du traitement, avec vérification de l’hypothèse de proportionnalité des risques (résidus de Schoenfeld).

## Structure du dépôt

```text
.
├── data/
│   └── donnees_survie.csv           # Données de survie (DureeSurvieJr, DECES, AGE, Traitement, etc.)
├── R/
│   ├── 01_description.R             # Statistiques descriptives
│   ├── 02_kaplan_meier.R           # Courbes de survie KM + log-rank
│   ├── 03_hazard_function.R        # Estimation du hasard instantané
│   ├── 04_cox_models.R             # Modèles de Cox (model1–model6, ANOVA, AIC)
│   └── utils.R                     # Fonctions auxiliaires
├── reports/
│   └── projet-DC-Adama-sall.pdf    # Rapport détaillé
├── figures/
│   ├── km_global.png
│   ├── km_sexe.png
│   ├── km_traitement.png
│   ├── km_age.png
│   └── hazard_plot.png
└── README.md
