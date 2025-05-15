import pandas as pd

# Charger le fichier CSV
df = pd.read_csv('temperature-quotidienne-regionale.csv')

# Filtrer les lignes qui contiennent le mot "Île-de-France" dans la première colonne
df_idf = df[df.iloc[:, 0].str.contains('Île-de-France', case=False, na=False)]

# Trier les lignes par ordre de dates
df_idf_sorted = df_idf.sort_values(by='Nom_de_la_colonne_date')

# Enregistrer le résultat dans un nouveau fichier CSV
df_idf_sorted.to_csv('idf_seulement.csv', index=False)

# Charger le fichier CSV en spécifiant le délimiteur comme ';'
df = pd.read_csv('idf_seulement.csv', delimiter=';')

# Renommer les colonnes
df.columns = ['ID', 'Date', 'code_insee_region', 'region', 'tmin', 'tmax', 'tmoy']

# Supprimer les colonnes spécifiées
colonnes_a_supprimer = ['code_insee_region', 'region', 'tmin', 'tmax']
df = df.drop(columns=colonnes_a_supprimer)

# Enregistrer le fichier modifié
df.to_csv('Temperature_moyenne_ile_de_france.csv', index=False)

#Enfin nous avons fait une moyenne par mois des températures.

# Charger les données
df = pd.read_csv('Temperature_moyenne_ile_de_france.csv')

# Convertir les dates en datetime pour extraire facilement l'année et le mois
df['Date'] = pd.to_datetime(df['Date'], format='%d/%m/%y')

# Ajouter des colonnes pour l'année et le mois
df['Year'] = df['Date'].dt.year
df['Month'] = df['Date'].dt.month
# Dictionnaire pour le nom des mois en français
months_fr = {
  1: 'Janvier', 2: 'Février', 3: 'Mars', 4: 'Avril', 5: 'Mai', 6: 'Juin',
  7: 'Juillet', 8: 'Août', 9: 'Septembre', 10: 'Octobre', 11: 'Novembre', 12: 'Décembre'
}

# Calculer la moyenne des températures par mois et par année
monthly_avg = df.groupby(['Year', 'Month'])['tmoy'].mean().reset_index()

# Convertir le numéro du mois en son nom en français et ajouter l'année
monthly_avg['Mois-Annee'] = monthly_avg.apply(lambda x: f"{months_fr[x['Month']]}-{x['Year']}", axis=1)

# Sélectionner les colonnes d'intérêt pour le fichier final
result = monthly_avg[['Mois-Annee', 'tmoy']]

# Sauvegarder le résultat dans un nouveau fichier Excel
result.to_excel('Temperature_moyenne_ile_de_france_v2.xlsx', index=False)


