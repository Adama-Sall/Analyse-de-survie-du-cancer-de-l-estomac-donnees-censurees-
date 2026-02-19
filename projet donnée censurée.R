### Packages
library(readxl)
library(survival) 
library(CoxR2)
library(timereg)
library(survminer) 
library(stringi)
library(muhaz)    
library(knitr)
library(Hmisc)
library(ggplot2)
library(plotly)
### importation des données
donnee <- read_excel("C:/Users/Hp/Desktop/Master 2/Donnee censurée/Base_Projet.xlsx")
head(donnee)
View(donnee)
attach(donnee)
### Reconversion des donnees
Traitement=as.factor(Traitement)
SEXE = as.factor(SEXE)
### vérifier les types
str(donnee)
### statistiques descriptives
summary(donnee)
kable(summary(donnee))
describe(donnee)
### variable sexe 
describe(SEXE)
describe(donnee)
################################################################################
###Visualisation 
### Pour la variable durrée de survie
# Boxplot et Histogramme avec Plotly
p1 <- plot_ly(data = donnee,
              x = ~AGE,  
              color = ~SEXE,  
              type = "box") %>%
  layout(title = "Boxplot de l'âge en fonction du sexe",
         xaxis = list(title = "Âge"),
         yaxis = list(title = "Sexe"))
p2 <- plot_ly(data = donnee,
              x = ~AGE,  
              type = "histogram",
              marker = list(color = 'skyblue')) %>%
  layout(title = "Histogramme de l'âge",
         xaxis = list(title = "Âge"),
         yaxis = list(title = "Fréquence"))
fig1 <- subplot(p1, p2, nrows = 2, margin = 0.05, heights = c(0.5, 0.5)) %>%
  layout(title = "Distribution de l'âge combinée")
fig1

### Pour la variable durrée de survie
p3 <- plot_ly(data = donnee,
              x = ~DureeSurvieJr,  
              type = "box") %>%
  layout(title = "Boxplot de la durée de survie",
         xaxis = list(title = "Âge"),
         yaxis = list(title = "Sexe"))
p4 <- plot_ly(data = donnee,
              x = ~DureeSurvieJr,  
              type = "histogram",
              marker = list(color = 'skyblue')) %>%
  layout(title = "Histogramme de la durée de survie",
         xaxis = list(title = "Âge"),
         yaxis = list(title = "Fréquence"))
fig2 <- subplot(p3, p4, nrows = 2, margin = 0.05, heights = c(0.5, 0.5)) %>%
  layout(title = "Distribution de la durée de survie")
fig2

################################################################################
### 1)la fonction de survie de Kaplan Meier avec les intervalles de confiance.
fit1 <- survfit(Surv(DureeSurvieJr, DECES) ~ 1, data = donnee)
fit1
summary(fit1)
x11()
ggsurvplot(fit1,
           data = donnee,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           legend.title = "Impact de la maladie",
           title = "Fonctions de survie de Kaplan-Meier",
           risk.table.height = 0.2,
           ggtheme = theme_bw())

################################################################################
### 2) Estimer la fonction de survie pour les hommes et pour les femmes.
# risque cumule pour deux groupes
km_sexe <- survfit(Surv(DureeSurvieJr, DECES) ~ SEXE, data = donnee)
km_sexe
summary(km_sexe)
## Comparaison de fonction de survie par sexe.
ggsurvplot(km_sexe,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, 
           risk.table.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv",  
           ggtheme = theme_bw(), 
           legend.labs=c("Homme=1","Femme=2"),
           palette = c("#E7B800", "#2E9FDF"))
#### test de comparaison de courbe de survie.
test.survie = survdiff(Surv(DureeSurvieJr, DECES) ~ SEXE, data = donnee)
test.survie
################################################################################
### 4) Estimer la fonction de survie pour les différents types de traitement.
km_traitement <- survfit(Surv(DureeSurvieJr, DECES) ~ Traitement, data = donnee)
km_traitement
## Comparaison de fonction de survie par sexe.
x11()
ggsurvplot(km_traitement,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           surv.scale = "percent",
           legend.labs = c("Traitement 0", "Traitement 1", "Traitement 2", "Traitement 3"),
           legend.title = "Impact traitement",
           palette = c("dodgerblue4", "orchid2", "forestgreen", "goldenrod"), # 4 couleurs
           title = "Fonctions de survie de Kaplan-Meier",
           risk.table.height = .2)
### 5)  Comparer les trois fonctions de survie (pour les différents types de traitement) de façon 
#graphique et de façon théorique. Interpréter les résultats.
test.survie_traitement =  survdiff(Surv(DureeSurvieJr, DECES) ~ Traitement, data = donnee)
test.survie_traitement
################################################################################
###  6) Décomposer la variable « AGE » en trois classes 
classe_age = cut(AGE, breaks=c(25,45,65,82))
levels(classe_age) = c("classe 1","classe 2","classe 3")
eff_age = table(classe_age)
eff_age
### proportion
prop = prop.table(table(classe_age))
prop
###  diagramme en camembert
x11()
pie(prop, 
    main = "Répartition des groupes d'âge",
    col = c("blue", "red", "green"), # Ajustez selon le nombre de groupes
    border = "black")
################################################################################
### 7) Estimer la fonction de survie pour les différentes classes d’âge obtenues. 
# estimation des fonctions de survie
km_age = survfit(Surv(DureeSurvieJr, DECES)~classe_age,data=donnee)
km_age
summary(km_age)
################################################################################
### 8) Comparaison des trois fonctions de survie (graphique et théorique) .
ggsurvplot(km_age,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           legend.labs = c("classe 1", "classe 2", "classe 3"),
           legend.title = "Impact traitement",
           palette = c("blue", "red", "green"),
           title = "Fonctions de survie de Kaplan-Meier",
           risk.table.height = .2)
### test
test.survie_age =  survdiff(Surv(DureeSurvieJr, DECES) ~ classe_age, data = donnee)
test.survie_age
###################################################################################
### 9) Estimer la fonction de hasard instantané et tracer son graphe. 
hasard1 = muhaz(DureeSurvieJr,DECES,min.time = 2,max.time = 3600)
hasard1
# Tracer les graphiques
plot(hasard, 
     main = "Hasard instantané",
     xlab = "Temps", 
     ylab = "Hasard instantané")

################################################################################
### 10) Ajuster sur les données un modèle de régression de Cox.
### modele 1: effet du traitement et de l'âge
model1 = coxph(Surv(DureeSurvieJr, DECES)~Traitement+AGE, data=ovarian, method="breslow")
summary(model1)
coxr2(model1) 
### Modèle 2 : Interaction Traitement × age
model2 = coxph(Surv(DureeSurvieJr, DECES)~Traitement*AGE, data=ovarian, method="breslow")
summary(model2)
coxr2(model2)
### Modele 3: effet du traitement 
model3 = coxph(Surv(DureeSurvieJr, DECES)~ Traitement, data=donnee, method="breslow")
summary(model3)
coxr2(model3)
### Modele 4: effet des antécedents familliaux
model4 = coxph(Surv(DureeSurvieJr, DECES)~AntFamil, data=donnee, method="breslow")
summary(model4)
coxr2(model4)
### Modele 5: effet du traitement, de l'âge et leur interaction
model5 = coxph(Surv(DureeSurvieJr, DECES)~Traitement+AGE+Traitement:AGE,data=donnee, method="breslow")
summary(model5)
coxr2(model5)
### Modele 6: effet del'âge du traitement et de l'antecedent familliale
model6 = coxph(Surv(DureeSurvieJr, DECES)~AGE+ Traitement +AntFamil, data = donnee, method = "breslow")
summary(model6)
coxr2(model6)
### Test de l'Anova
anova(model3, model1)
anova(model1, model2)
anova(model3, model5)
anova(model5, model6)
### Comparaison des modeles
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)
################################################################################
### 11) Etudier l’adéquation du modèle final obtenu. 
model_final = model6
### Test des résidus de Schoenfeld
test_zph <- cox.zph(model_final)
test_zph
par(mfrow = c(2,3))
### graphe des residus
ggcoxzph(test_zph)
################################################################################
### Modèle avec ajustement dépendant du temps
model_corrige <- coxph(
  Surv(DureeSurvieJr, DECES) ~ strata(Traitement, classe_age) + AntFamil, 
  data = donnee)
### Comparaison AIC
AIC(model6, model_corrige)
### test des residus de Schoenfeld pour le modele corrigé
test_zph <- cox.zph(model_corrige)
test_zph
par(mfrow = c(2,3))
### Graphe des residus
ggcoxzph(test_zph)

