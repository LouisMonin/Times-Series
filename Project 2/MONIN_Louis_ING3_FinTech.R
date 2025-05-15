# Mini-Projet. Introduction aux séries temporelles #

# 1 Introduction #

#Le mini-projet a pour but, dans un premier temps, de simuler des modèles ARIMA et de
#créer une fonction de prévision personnalisée en appliquant les formules mathématiques établies
#dans le cours. Dans un second temps, les binômes devront utiliser les fonctions prédéfinies de R
#pour mettre en œuvre les Modèles ARMA, ARIMA et SARIMA sur un jeu de données spécifique.
#Les étapes comprennent la compréhension du contexte sous-jacent du problème, la collecte et le
#prétraitement des données, la sélection judicieuse des modèles potentiels pour ajuster ces données,
#et enfin, la présentation claire des résultats obtenus. L’objectif ultime est d’évaluer la compréhension
#approfondie des concepts, la précision des modèles, ainsi que la qualité globale de la présentation des
#résultats. Chaque binôme sera évalué en fonction de ces critères, avec l’opportunité d’approfondir
#ses connaissances en modélisation de séries temporelles.

# 2 Partie I : Simulation #

#Question a
#Simuler un processus ARIMA(p,d,q) stationnaire avec d = 1, p ≥ 2, et q ≥ 1, en développant
#une fonction personnalisée nommée Arima_sim.
 
Arima_sim <- function(n, p = 2, q = 1, d = 1, ar_coeffs = NULL, ma_coeffs = NULL, seed = NULL) {
  # Vérification des paramètres
  if (p < 2 || q < 1 || d != 1) {
    stop("Cette fonction simule uniquement un ARIMA avec p >= 2, q >= 1 et d = 1.")
  }
  
  # Fixer la graine pour la reproductibilité si spécifiée
  if (!is.null(seed)) set.seed(seed)
  
  # Générer les coefficients AR stationnaires si non spécifiés
  if (is.null(ar_coeffs)) {
    repeat {
      ar_coeffs <- runif(p, -0.9, 0.9)
      if (all(abs(polyroot(c(1, -ar_coeffs))) > 1)) break
    }
  }
  
  # Générer les coefficients MA inversibles si non spécifiés
  if (is.null(ma_coeffs)) {
    repeat {
      ma_coeffs <- runif(q, -0.9, 0.9)
      if (all(abs(polyroot(c(1, ma_coeffs))) > 1)) break
    }
  }
  
  # Simuler la série ARIMA
  series <- arima.sim(
    n = n,
    model = list(order = c(p, d, q), ar = ar_coeffs, ma = ma_coeffs)
  )
  
  # Retourner la série simulée
  return(list(
    series = series,
    ar_coeffs = ar_coeffs,
    ma_coeffs = ma_coeffs
  ))
}

# Exemple d'utilisation
result <- Arima_sim(n = 500, p = 3, q = 2, seed = 123)

# Visualisation de la série simulée
plot(result$series, type = "l", main = "Simulation d'un processus ARIMA(3, 1, 2)",
     xlab = "Temps", ylab = "Valeurs")

#Question b
#Implémenter une fonction de prévision personnalisée, Forecast_Per, en appliquant
#les formules mathématiques présentées dans le cours (qui devront être rappelées)
#Tester cette fonction sur les données simulées pour effectuer une prévision d'ordre h=2

Forecast_Per <- function(series, p, q, d, ar_coeffs, ma_coeffs, h) {
  # Vérifier les paramètres
  if (d != 1) stop("Cette fonction supporte uniquement d = 1.")
  
  # Différencier la série
  diff_series <- diff(series, differences = d)
  n <- length(diff_series)
  
  # Initialiser les prévisions
  forecasts <- numeric(h)
  
  # Prévoir les valeurs futures
  for (t in 1:h) {
    # Calcul des termes AR
    ar_part <- 0
    if (p > 0) {
      for (i in 1:min(p, n + t - 1)) {
        ar_part <- ar_part + ar_coeffs[i] * (if (t - i <= 0) series[n + t - i] else forecasts[t - i])
      }
    }
    
    # Calcul des termes MA
    ma_part <- 0
    if (q > 0) {
      for (j in 1:min(q, n + t - 1)) {
        ma_part <- ma_part + ma_coeffs[j] * (if (t - j <= 0) 0 else 0) # Innovations non observables
      }
    }
    
    # Ajouter les parties AR et MA
    forecasts[t] <- ar_part + ma_part
  }
  
  # Reconstruire la série d'origine (somme des différences)
  final_forecasts <- cumsum(c(series[n], forecasts))
  return(final_forecasts[-1])
}

# Tester la fonction
sim_data <- result$series
ar_coeffs <- result$ar_coeffs
ma_coeffs <- result$ma_coeffs

# Prévision pour h = 2
forecasted_values <- Forecast_Per(
  series = sim_data,
  p = 3,
  q = 2,
  d = 1,
  ar_coeffs = ar_coeffs,
  ma_coeffs = ma_coeffs,
  h = 2
)

print(forecasted_values)

#Question c
#Comparer les résultats obtenus avec ceux fournis par la fonction Arima du package forecast
#dans R. Cette comparaison doit inclure une analyse graphique ainsi qu’une évaluation basée
#sur des critères d’erreurs de prédiction, tels que le MSE, et d’autres métriques appropriées.

# Charger le package forecast
library(forecast)

# Ajuster le modèle ARIMA sur les données simulées
modele_arima <- Arima(sim_data, order = c(3, 1, 2))

# Effectuer une prévision avec la fonction Arima
resultat_prevision <- forecast(modele_arima, h = 2)

# Comparaison graphique entre les prévisions des deux méthodes
plot(resultat_prevision, main = "Comparaison des prévisions entre Arima et Forecast_Per",
     xlab = "Temps", ylab = "Valeurs")
points(length(sim_data) + 1:length(forecasted_values), forecasted_values, col = "red", pch = 19)
legend("topright", legend = c("Arima (forecast)", "Forecast_Per"), 
       col = c("blue", "red"), pch = 19)

# Calcul du MSE pour évaluer la précision des prévisions
# Simuler des "valeurs réelles" comme proxy à partir des dernières observations de sim_data
valeurs_reelles <- sim_data[(length(sim_data) - 1):length(sim_data)]  # Les deux derniers points

# Vérifier si les longueurs correspondent
if (length(valeurs_reelles) == length(forecasted_values)) {
  mse_personnalise <- mean((valeurs_reelles - forecasted_values)^2, na.rm = TRUE)
  mse_arima <- mean((valeurs_reelles - resultat_prevision$mean)^2, na.rm = TRUE)
} else {
  cat("Erreur : Les longueurs des prévisions et des valeurs réelles ne correspondent pas.\n")
  mse_personnalise <- NA
  mse_arima <- NA
}

# Affichage des résultats
cat("MSE Forecast_Per :", mse_personnalise, "\n")
cat("MSE Arima (forecast) :", mse_arima, "\n")



# 3 Partie II : Pratique sur un Jeu de Données Réel #

# 3.1 Le Jeu de Données #

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


# 3.2 Études descriptives succintes #

#Description de la série

#Chargement du dataset

install.packages("xlsx")
install.packages("forecast")
install.packages("openxlsx")

library(xlsx)
library(forecast)
library(readxl)
library(openxlsx)

donnees <- read.xlsx("H:/Desktop/Methode_de_prevision_de_series_temporelles/Projet/Temperature_moyenne_mensuelle_ile_de_france_v2.xlsx", sheet = 1)
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

# Analyse de la tendance et de la saisonnalité
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

#Valeur de Dickey-Fuller: -10.751. Cette statistique est négative, ce qui est 
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

# 3.3 Utilisation des Méthodes Stochastiques ARIMA et SARIMA #

# 3.3.1 Stationnarisation éventuelle de la série pour la ramener à un processus stationnaire #

# Test ADF pour stationnarité

adf_test <- adf.test(donnees_ts)
cat("P-value du test ADF:", adf_test$p.value, "\n")

#Cette partie du code réalise une différenciation sur notre série si la p-value
#de la série est supérieure à 0.05.

if (adf_test$p.value > 0.05) {
  cat("La série n'est pas stationnaire, différenciation en cours...\n")
  donnees_diff <- diff(donnees_ts)
  adf_test_diff <- adf.test(donnees_diff)
  cat("P-value après différenciation:", adf_test_diff$p.value, "\n")
} else {
  donnees_diff <- donnees_ts
}

#La plupart des modèles ARIMA nécessitent une série stationnaire (statistiques constantes
#au cours du temps). Pour vérifier cela, on applique le test ADF
#Si la p-value est < 0.05. La série est stationnaire et aucune différenciation
#n'est nécessaire
#Si la p-value est >= 0.05. La série n'est pas stationnaire. Une différenciation
#est nécessaire pour supprimer la tendance.

#Dans notre cas,on obtient une p-value = 0.01 < 0.05 donc ma série est bien stationnaire.

# 3.3.2 Identification a priori de modèles potentiels #

acf(donnees_diff, main="ACF")
pacf(donnees_diff, main="PACF")

#L'objectif est de déterminer les paramètres (p, d, q) pour le modèle ARIMA à l'aide des 
#graphiques ACF (fonction d'autocorrélation) et PACF (fonction d'autocorrélation partielle)

#L'ACF identifie le degré de dépendance entre observation. Si des pics significatifs décroissent
#lentement, cela peut indiquer un composant MA (moving average).
#Le PACF met en évidence les lags AR (auto-régressifs). Des pics significatifs indiquent la valeur
#potentielle de p

#ARIMA(p, d, q) :
#p : L’ordre AR est déterminé par le nombre de lags significatifs dans le PACF.
#q : L’ordre MA est déterminé par le nombre de lags significatifs dans l’ACF.
#d : Déjà défini par le nombre de différenciations appliquées.

#ACF (Autocorrelation Function)

#Le graphique de l'ACF montre les corrélations entre la série temporelle et ses lags successifs.
#Interprétation :
#Les premiers lags présentent des corrélations significatives (valeurs au-delà des bandes bleues).
#Les corrélations semblent décroître lentement, ce qui pourrait indiquer la présence d’un composant MA (Moving Average). Le nombre de lags significatifs avant que la corrélation ne devienne insignifiante suggère la valeur potentielle de q.

#PACF (Partial Autocorrelation Function)

#Le graphique de la PACF examine les corrélations partielles, en éliminant l'influence des lags intermédiaires.
#Interprétation :
#Le premier lag est significatif dans la PACF, mais les lags suivants deviennent rapidement insignifiants.
#Cela suggère un processus AR (Auto-Regressive) d'ordre 1, donc un potentiel p = 1.

#En combinant les analyses :
#ARIMA(p, d, q) :
#p=1 (d'après la PACF).
#q≈1 ou 2 (d'après l’ACF).
#d=0 car pas de différenciation

# 3.3.3 Estimation des paramètres de ces modèles #

model_arima <- auto.arima(donnees_ts, seasonal = FALSE)
model_sarima <- auto.arima(donnees_ts, seasonal = TRUE)

model_arima
model_sarima

# 3.3.4 Vérifiaction des modèles #

checkresiduals(model_arima)
checkresiduals(model_sarima)

#ARIMA(4,0,0) :
#Les résidus ne sont pas indépendants (test Ljung-Box significatif).
#Des autocorrélations significatives subsistent dans l’ACF des résidus.
#Ce modèle est inapproprié pour vos données.

#SARIMA(0,0,0)(2,1,0)[12] :
#Les résidus sont indépendants (test Ljung-Box non significatif, p>0.05p>0.05).
#Aucun pic significatif dans l’ACF des résidus, confirmant qu’ils ressemblent à un bruit blanc.
#Ce modèle est bien adapté à vos données et devrait être utilisé pour effectuer des prévisions.

# 3.3.5 Choix définit d'un modèle en donnant la forme explicite de son équation #

#Ce code permet de comparer l'AIC des deux modèles proposés précedemment.
#On préfère un modèle à un autre lorsque l'AIC est plus faible.

if (AIC(model_arima) < AIC(model_sarima)) {
  final_model <- model_arima
} else {
  final_model <- model_sarima
}
final_model

# 3.3.6 Prévision à l'aide du modèle choisi

#Ce code permet de montrer les prévisions à l'aide du modèle choisit qui est le modèle
#ARIMA(0,0,0)(2,1,0)[12]

forecast_result <- forecast(final_model, h = 12)
plot(forecast_result)

#Le graphique montre la série réelle (historique), les prévision pour les 12 prochains mois
#ainsi que l'intervalle de confiance à 95%

# 3.3.7 Analyse a posteriori des prévisions #

accuracy(forecast_result)

