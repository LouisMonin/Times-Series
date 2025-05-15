#Mini-Projet. Introduction aux séries temporelles

#Partie 1 : Caractéristiques de la série

#Description et caractéristiques de la série

#A l'origine le fichier représente la température quotidienne régionale
#(depuis le 1er janvier 2016 au 31 janvier 2024)

#Ce jeu de données présente les températures minimales, 
#maximales et moyennes quotidiennes (en degré celsius), 
#par région administrative française, du 1er janvier 2016 
#à aujourd'hui.

#Il est basé sur les mesures officielles du réseau 
#de stations météorologiques françaises. La mise à jour 
#de ce jeu de données est mensuelle.

#Lien du site internet : https://www.data.gouv.fr/fr/datasets/temperature-quotidienne-regionale-depuis-janvier-2016/

#URL du dataset : https://www.data.gouv.fr/fr/datasets/r/d707a3b3-c0eb-404b-b3ef-2d4e0ddb96f4

#Description du fichier

#ID: id[text]
#Date: date[date] Date de l’observation
#Code INSEE région: code_insee_region[int] Code INSEE région administrative
#Région: region[text] Région administrative
#TMin (°C): tmin[double] Température minimale quotidienne
#TMax (°C): tmax[double] Température maximale quotidienne
#TMoy (°C): tmoy[double] Température moyenne quotidienne

#A l'aide d'un programme python nous avons supprimer 
#toutes les données des régions qui n'était pas
#celles de l'ile-de-France

#import pandas as pd
# Charger le fichier CSV
#df = pd.read_csv('votre_fichier.csv')
# Filtrer les lignes qui contiennent le mot "Île-de-France" dans la première colonne
# Assurez-vous de remplacer 'Nom_de_la_première_colonne' par le nom réel de la première colonne
#df_idf = df[df.iloc[:, 0].str.contains('Île-de-France', case=False, na=False)]
# Trier les lignes par ordre de dates
# Assurez-vous de remplacer 'Nom_de_la_colonne_date' par le nom réel de la colonne contenant les dates
# Si vous n'avez pas une colonne de dates, vous devez spécifier la colonne que vous souhaitez utiliser pour le tri
#df_idf_sorted = df_idf.sort_values(by='Nom_de_la_colonne_date')
# Enregistrer le résultat dans un nouveau fichier CSV
#df_idf_sorted.to_csv('idf_seulement_trie.csv', index=False)

#Puis nous avons supprimer les colonnes contenant
#L'ID, le code_insee_region, la région, la température minimale et maximale
#Nous avons conservé uniquement la date et la température moyenne.
#Grâce à l'algorithme python suivant

#import pandas as pd
# Charger le fichier CSV en spécifiant le délimiteur comme ';'
#df = pd.read_csv('idf_seulement.csv', delimiter=';')
# Renommer les colonnes
#df.columns = ['ID', 'Date', 'code_insee_region', 'region', 'tmin', 'tmax', 'tmoy']
# Supprimer les colonnes spécifiées
#colonnes_a_supprimer = ['code_insee_region', 'region', 'tmin', 'tmax']
#df = df.drop(columns=colonnes_a_supprimer)
# Enregistrer le fichier modifié
#df.to_csv('Temperature_moyenne_ile_de_france.csv', index=False)

#Enfin nous avons fait une moyenne par mois des températures.
#import pandas as pd
# Charger les données
#df = pd.read_csv('Temperature_moyenne_ile_de_france.csv')
# Convertir les dates en datetime pour extraire facilement l'année et le mois
#df['Date'] = pd.to_datetime(df['Date'], format='%d/%m/%y')
# Ajouter des colonnes pour l'année et le mois
#df['Year'] = df['Date'].dt.year
#df['Month'] = df['Date'].dt.month
# Dictionnaire pour le nom des mois en français
#months_fr = {
  #1: 'Janvier', 2: 'Février', 3: 'Mars', 4: 'Avril', 5: 'Mai', 6: 'Juin',
  #7: 'Juillet', 8: 'Août', 9: 'Septembre', 10: 'Octobre', 11: 'Novembre', 12: 'Décembre'
#}
# Calculer la moyenne des températures par mois et par année
#monthly_avg = df.groupby(['Year', 'Month'])['tmoy'].mean().reset_index()
# Convertir le numéro du mois en son nom en français et ajouter l'année
#monthly_avg['Mois-Annee'] = monthly_avg.apply(lambda x: f"{months_fr[x['Month']]}-{x['Year']}", axis=1)
# Sélectionner les colonnes d'intérêt pour le fichier final
#result = monthly_avg[['Mois-Annee', 'tmoy']]
# Sauvegarder le résultat dans un nouveau fichier Excel
#result.to_excel('Temperature_moyenne_ile_de_france_v2.xlsx', index=False)


#Partie 1 : Caractéristique de la série

  #Description de la série

#Chargement du dataset

install.packages("xlsx")
install.packages("forecast")
install.packages("openxlsx")

library(xlsx)
library(forecast)
library(readxl)
library(openxlsx)

donnees <- read.xlsx("Temperature_moyenne_mensuelle_ile_de_france_v2.xlsx", sheet = 1)
View(donnees)

#Générer le tableau des valeurs avec knitr

library(knitr)

knitr::kable(donnees, booktabs = TRUE, col.names = c("Mois-Annee", "tmoy"))

#Notre dataset correspond aux relevés de la température moyenne en Ile-de-France par mois
#entre Janvier 2016 et Janvier 2024 publié par le gouvernement.

#Ce dataset est composé de 2 colonnes les mois entre 2016 et 2024 et la température moyenne.

# Créer une série temporelle

donnees_ts <- ts(donnees$tmoy, frequency=12, start=c(2016,1), end =c(2024,1))
donnees_ts

#Cette ligne transforme les données detempérature moyenne en une série temporelle R (ts), 
#en supposant une fréquence annuelle de 12 mois (ce qui est approprié pour des données mensuelles) 
#et en indiquant que la série commence en 2016.

# Utiliser ggseasonplot pour visualiser les données
# Charger les packages nécessaires

library(forecast)
library(ggplot2)

ggseasonplot(donnees_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Température moyenne (°C)") +
  xlab("Date (mois-années)") +
  ggtitle("Tracé saisonnier : Températures moyennes en Île-de-France")

# Utiliser ggseasonplot en mode polaire pour visualiser les données
ggseasonplot(donnees_ts, polar=TRUE) +
  ylab("Température moyenne (°C)") +
  xlab("Date (mois-années)") +
  ggtitle("Tracé saisonnier polaire : Températures moyennes en Île-de-France")

ggsubseriesplot(donnees_ts) +
  ylab("Température moyenne (°C)") +
  xlab("Date (mois-années)") +
  ggtitle("Graphique de sous-séries saisonnières : Températures moyennes en Île-de-France")

#Analyse de la tendance et de la saisonnalité
# Décomposition STL

decomp <- stl(donnees_ts, s.window="periodic")

# Visualiser la décomposition

plot(decomp)

#Décomposition STL

#Le premier graphique montre la décomposition STL de votre série temporelle 
#en trois composantes : saisonnalité, tendance et résidus.
#Données saisonnières (deuxième graphique du haut) : Il semble y avoir un motif 
#saisonnier clair et cohérent qui se répète chaque année, ce qui est attendu pour 
#des données de température (plus chaud en été, plus froid en hiver).
#Tendance (troisième graphique du haut) : La tendance est relativement lisse 
#sans fluctuations extrêmes, suggérant des changements graduels dans les 
#températures moyennes sur plusieurs années.
#Résidus (graphique du bas) : Les résidus semblent être relativement faibles 
#et aléatoires, ce qui indique que la décomposition a capturé la plupart des 
#comportements systématiques de la série temporelle, et ce qui reste est 
#principalement du bruit.

  
#Analyse de la corrélation

#Autocorrélation

acf(donnees_ts)

#La fonction d'autocorrélation pour la série temporelle. On observe que 
#l'autocorrélation décroît lentement, ce qui est typique des séries temporelles 
#avec une forte composante de tendance ou une saisonnalité non complètement modélisée.

# Autocorrélation partielle

pacf(donnees_ts)

#La fonction d'autocorrélation partielle. Les barres sont toutes très courtes et 
#situées à l'intérieur des limites de confiance, ce qui suggère qu'il n'y a pas 
#d'autocorrélations partielles significatives aux retards supérieurs à 1. 
#Ce résultat indique qu'un modèle autorégressif simple (AR) ne serait 
#probablement pas approprié pour cette série temporelle, ou que le modèle AR 
#nécessaire serait d'un ordre très bas.

#Analyse de la stationnarité

install.packages("tseries")
library(tseries)
adf.test(donnees_ts)

#Le test de Dickey-Fuller augmenté (ADF) est utilisé pour déterminer si une série 
#temporelle est stationnaire ou non, c'est-à-dire si la série présente une 
#tendance au fil du temps. Voici les éléments clés de votre résultat ADF :

#Valeur de Dickey-Fuller: -10.548. Cette statistique est négative, ce qui est 
#un bon indicateur. Plus cette valeur est négative, plus la preuve contre 
#l'hypothèse nulle (la présence d'une racine unitaire, indiquant une 
#non-stationnarité) est forte.
#Ordre de retard (Lag order): 4. Cela indique que le test a utilisé 4 retards 
#dans la construction de l'équation de test ADF pour la série temporelle, ce qui 
#correspond au nombre de périodes prises en compte pour calculer la corrélation 
#dans la série.
#Valeur-p: 0.01. En statistiques, la valeur-p est utilisée pour déterminer 
#la signification statistique du résultat du test. Une valeur-p inférieure à 
#un seuil (généralement 0.05) indique que vous pouvez rejeter l'hypothèse nulle. 
#Dans ce cas, une valeur-p de 0.01 suggère que vous pouvez rejeter l'hypothèse de 
#non-stationnarité avec une confiance de 99 %.
#Hypothèse alternative: stationnaire. Le test ADF a une hypothèse alternative 
#selon laquelle la série est stationnaire.
#Le message d'avertissement indique que la valeur-p réelle est encore plus petite 
#que la valeur imprimée, renforçant l'évidence contre l'hypothèse nulle.
#En conclusion, le résultat du test ADF suggère fortement que la série temporelle 
#est stationnaire. Cela signifie que la série temporelle n'a pas de tendance ou de 
#modèle autorégressif intégré, ce qui est une propriété souhaitable lors de 
#l'utilisation de modèles de prévision tels que ARIMA. Cela signifie également 
#que la série temporelle est appropriée pour une analyse plus approfondie et le 
#développement de modèles prédictifs sans nécessiter de différenciation pour 
#rendre la série stationnaire.

#Type de modèle
#Analyse avec les graphiques de la tendance, la saisonnalité
#et les résidus.
#La tendance est relativement lisse et montre une légère augmentation sur la 
#période, ce qui suggère qu'un modèle additif pourrait être approprié si ces 
#changements de tendance sont uniformes au fil du temps.
#La saisonnalité est très claire et montre des motifs répétitifs chaque année, 
#ce qui est typique pour les données de température. Cependant, sans une analyse 
#plus approfondie, il est difficile de dire si l'amplitude de cette saisonnalité 
#reste constante ou non par rapport au niveau de la tendance. Si l'amplitude reste
#constante, cela soutiendrait un modèle additif. Si elle augmente avec le niveau de 
#la tendance, cela soutiendrait un modèle multiplicatif.
#Les résidus ne montrent pas de structure claire, ce qui est bon pour la modélisation
#car cela signifie que la majeure partie de l'information a été capturée par la 
#tendance et la saisonnalité.

#Effectuer la décomposition saisonnière

decomp <- decompose(donnees_ts)
decomp
attributes(decomp)
plot(decomp)
recomp <- decomp$trend + decomp$seasonal
par(mfcol=c(2,1))
plot(recomp)
plot(donnees_ts)


#Construction d'une série d'application avec 80% des observations et d'une série de validation
#avec les 20% restants

ts_train <- window(donnees_ts, start = c(2016,1), end = c(2020,12))
ts_test <- window(donnees_ts, start = c(2021,1), end = c(2023,12))
par(mfcol=c(2,1))
plot(ts_train)
plot(ts_test)

#On divise notre série en 2 avec une série d'application et une série de validation

train = donnees[c(1:72),]
test = donnees[c(73:97),]

#Régression linéaire sur les données lissées par moyenne mobile

Xt_CVS <- read.xlsx("LAMRINI_Mohamed_MONIN_Louis_ING2MI2_Tableur_Porjet_Series_Temporelles.xlsx", sheet = "Xt_CVS MoyMob")
View(Xt_CVS)

reg_moymob <- lm(`Xt_CSV(Additif)` ~ t, data = Xt_CVS)
summary(reg_moymob)

prevision_moymob <- read.xlsx("LAMRINI_Mohamed_MONIN_Louis_ING2MI2_Tableur_Porjet_Series_Temporelles.xlsx", sheet = "Prevision MoyMob")
View(prevision_moymob)

#Régression linéaire sur les données d'origine

test$t <- seq.int(nrow(test))

regr <-lm(tmoy ~ t,data=train)
summary(regr)

test$T <- test$t

reg <- lm(tmoy ~ t + T,data=train)
summary(reg)

preds <- predict(regr, newdata = test)

par(mfcol=c(1,1))
plot(test$tmoy, type = "l",lwd=2.0,col="blue",xlab="Temps",font.lab= 2, ylab="Température moyenne (°C)",main="Prévision de la température moyenne")
lines(prevision_moymob$Prevision_MoyMob + 0.46, type = "l", col = "green", lwd = 2.0)
lines(prevision_moymob$Prevision_MoyMob, type = "l",col="red",lwd=2.0)
legend("topright", legend=c("Température observée", "Prédictions Moyennes mobiles", "Prédictions Régression linéaire"), col=c("blue", "red", "green"), lty=1, cex=0.8)


#Partie 3 : Prévision par lissage exponentiel

  #Méthode 1 : Lissage exponentiel double

fitLED <- ets(ts_train,model="AAN")
summary(fitLED)
predLED <- forecast(fitLED,h=36)
plot(predLED)
points(ts_test,type='l',col='darkgreen',lwd=2)
legend('top',c("Valeurs observées","Prédictions"),col=c("darkgreen","blue"),lty=rep(1,2),lwd = rep(2,2))

  #Méthode 2 : Lissage Holt-Winters  

fitHW <- ets(ts_train,model="AAA")
summary(fitHW)
predHW <- forecast(fitHW,h=36)
plot(predHW)
points(ts_test,type='l',col='darkgreen',lwd=2)
legend('top',c("Valeurs observées","Prédictions"),col=c("darkgreen","blue"),lty=rep(1,2),lwd = rep(2,2))

