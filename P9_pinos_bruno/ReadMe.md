# Table des matières
1. [Mise en situation](#Scenario) 
2. [Compétences évaluées](#Comp)
3. [Données](#data)
4. [Mission](#mission)

# Mise en situation <a name="Scenario"></a>
Vous êtes employé chez Enercoop, société coopérative qui s'est développée grâce à la libéralisation du marché de l’électricité en France. Elle est spécialisée dans les énergies renouvelables.

La plupart de ces énergies renouvelables est cependant intermittente, il est donc difficile de prévoir les capacités de production d'électricité. De plus, la demande en électricité des utilisateurs varie au cours du temps, et dépend de paramètres comme la météo (température, luminosité, etc.) Tout le challenge est de mettre en adéquation l'offre et la demande !

# Compétences évaluées <a name="Comp"></a>
- Maitriser les méthodes de lissage et la méthode de Holt-Winters
- Maitriser les notions de composantes et de modèles de décomposition
- Maitriser la méthode ARMA
- Représenter graphiquement une série temporelle

# Les données <a name="data"></a>
Vous téléchargerez les données de consommation d'électricité mensuelles à partir de cette page : http://www.rte-france.com/fr/eco2mix/eco2mix-telechargement

Les données météo que vous utiliserez pour corriger les données de l'effet température sont présentes ici : https://cegibat.grdf.fr/simulateur/calcul-dju

# Votre mission <a name="mission"></a>
Vous vous concentrerez uniquement sur la prédiction de la demande en électricité.

- Corrigez les données de consommation mensuelles de l'effet température (dues au chauffage électrique) en utilisant une régression - linéaire.
- Effectuez une désaisonnalisation de la consommation que vous aurez obtenue après correction, grâce aux moyennes mobiles.
- Effectuez une prévision de la consommation (corrigée de l'effet température) sur un an, en utilisant la méthode de Holt Winters (lissage exponentiel) puis la méthode SARIMA sur la série temporelle.
- Pour chaque traitement effectué (correction de l'effet température, désaisonnalisation, etc.), vous présenterez les 2 séries temporelles avant et après traitement, sur un graphique où les deux séries temporelles seront superposées.
