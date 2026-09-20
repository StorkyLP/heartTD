# Permet de lire les valeurs de notre fichier et les mettre dans notre variable
patients <- read.csv("heart.csv")
#head(patients)

############################################ ETUDE SUR LA VARIABLE AGE ############################################
# Variable quantitative
summary(patients$Age)
hist(patients$Age, main = "Répartition des patients d'âge", xlab = "Tranches d'âge", ylab = "Nombre de patients", col = "steelblue")

############################################ ETUDE SUR LA VARIABLE Sexe ############################################
# Variable qualitative
summary(patients$Sex)
barplot(table(patients$Sex), main = "Repartition des sexes", xlab = "Sex", ylab = "Nombre de patients", col = "Steelblue")

############################################ ETUDE SUR LA VARIABLE ChestPainType ############################################
#TODO expliquer la différence entre les maladies
# Variable qualitative
summary(patients$ChestPainType)
barplot(table(patients$ChestPainType), main = "Repartition des type de douleurs", xlab = "Type de maladie", ylab = "Population", col = "Steelblue")

############################################ ETUDE SUR LA VARIABLE RestingBP ############################################
#pression artérielle au repos, exprimée en mm Hg.
# Variable quantitative
summary(patients$RestingBP)
hist(patients$RestingBP, main = "Pression artérielle au repos en mm Hg", xlab = "Pression artérielle (repos)", ylab = "Population", col = "steelblue")

############################################ ETUDE SUR LA VARIABLE Cholesterol ############################################
# taux de cholestérol sérique, exprimé en mg/dl
# Variable quantitative
summary(patients$Cholesterol)
hist(patients$Cholesterol, main = "Taux de Cholestérol mg/dl", xlab = "Cholestérol", ylab = "Population", col = "steelblue")

############################################ ETUDE SUR LA VARIABLE FastingBS ############################################
#glycémie à jeun.
# qualitative
# — 1 : si la glycémie à jeun est supérieure à 120 mg/dl ;
# — 0 : sinon
barplot(table(patients$FastingBS), main = "Glycémie à jeun", xlab = "Glycémie", ylab = "Population", col = "Steelblue")

############################################ ETUDE SUR LA VARIABLE RestingECG ############################################
#résultats de l’électrocardiogramme au repos.
# Variable quanlitative
# — Normal : résultat normal ;
# — ST : présence d’anomalies de l’onde ST-T, telles que des inversions de l’onde T et/ou une élévation ou une dépression du segment ST supérieure à 0,05 mV ;
#— LVH : hypertrophie ventriculaire gauche probable ou certaine selon les critères d’Estes.
# TODO faire un deuxieme graph avec les autres valeurs car ca va jusqu'a 600
barplot(table(patients$RestingECG), main = "Au repos", xlab = "Type de résultat", ylab = "Population", col = "steelblue")

############################################ ETUDE SUR LA VARIABLE MaxHR ############################################
# fréquence cardiaque maximale atteinte, avec une valeur comprise entre 60 et 202.
# Variable quantitative
hist(patients$MaxHR, main = "Frequence cardiaque maximale", xlab = "Fréquence", ylab = "Population", col = "Steelblue", las = 2)


############################################ ETUDE SUR LA VARIABLE ExerciseAngina ############################################
# présence d’une angine de poitrine provoquée par l’exercice.
# Variable qualitative
# — Y : oui
# — N : non
barplot(table(patients$ExerciseAngina), main = "Exercice angine poitrine", xlab = "Presence d'angine", ylab = "Population", col = "steelblue")


############################################ ETUDE SUR LA VARIABLE Oldpeak ############################################
# dépression du segment ST mesurée pendant l’exercice
# quantitative continue
hist(
  patients$Oldpeak,
  breaks = 20,
  main = "Distribution de la dépression du segment ST (Oldpeak)",
  xlab = "Oldpeak (mm)",
  ylab = "Nombre de patients",
  col = "steelblue",
  border = "white"
)

############################################ ETUDE SUR LA VARIABLE ST_Slope ############################################
# pente du segment ST au pic de l’exercice.
# qualitative
# — Up : pente ascendante
# — Flat : pente plate
# — Down : pente descendante
barplot(table(patients$ST_Slope), main = "Pente du sgemnt ST au pic de l'exercice", xlab = "Etat de la pente", ylab = "Population", col = "Steelblue")

############################################ ETUDE SUR LA VARIABLE HeartDisease ############################################
# variable cible indiquant la présence ou l’absence d’une maladie cardiaque.
# qualitative
# — 1 : présence d’une maladie cardiaque ;
# — 0 : absence de maladie cardiaque
barplot(table(patients$HeartDisease), main = "Presence de maladie cardiaque", ylab = "Population", col = "Steelblue")




# TEST CHI2 pour qualitative
# Test Corellation pour quantitative
# S'arreter aux tests pour savoir si les variables sont indépendantes
# Faire une regression logistique (Bonus)


# Afficher les variables quantitatives pour avoir les valeurs aberrantes (par 4)
par(mfrow = c(1, 4))
boxplot(patients$Age, main = "Age", col = "steelblue")
boxplot(patients$RestingBP, main = "RestingBP", col = "steelblue")
boxplot(patients$Cholesterol, main = "Cholesterol", col = "steelblue")
boxplot(patients$MaxHR, main = "MaxHR", col = "steelblue")
# Rétablit l'affichage standard à 1 seul graphique
par(mfrow = c(1, 1))

# Demande du prof pour test
test <- t.test(patients$Age ~ patients$HeartDisease, data = patients, var.equal = TRUE)
test

########################### TEST INDEPENDANCE ###########################
# ---------------- HYPOTHESES ----------------
  # H0 -> Les deux variables n'ont aucun lien et sont donc indépendantes ON NE PREND PAS
  # H1 -> Les deux variables ont un lien et sont donc dépendantes si p-value < 0,05 ON PREND


# ---------------- KH2 test variable qualitative ----------------
# Sex -> ANS = p-value < 2.2e-16
kh2_sex <- chisq.test(patients$HeartDisease, patients$Sex)
kh2_sex
# ChestPainType p-value < 2.2e-16
kh2_ChestPainType <- chisq.test(patients$HeartDisease, patients$ChestPainType)
kh2_ChestPainType
# FastingBS p-value = 1.057e-15
kh2_FastingBS <- chisq.test(patients$HeartDisease, patients$FastingBS)
kh2_FastingBS
# RestingECG p-value = 0.004229
kh2_RestingECG <- chisq.test(patients$HeartDisease, patients$RestingECG)
kh2_RestingECG
# ExerciseAngina p-value < 2.2e-16
kh2_ExerciseAngina <- chisq.test(patients$HeartDisease, patients$ExerciseAngina)
kh2_ExerciseAngina
# ST_Slope p-value < 2.2e-16
kh2_ST_Slope <- chisq.test(patients$HeartDisease, patients$ST_Slope)
kh2_ST_Slope

# Pour chacune de ces six variables qualitatives, on rejette l'hypothèse nulle H0 au risque de 5 %. Il existe une dépendance statistique entre chacune de ces variables et la présence d'une maladie cardiovasculaire (HeartDisease). Elles ont toutes un pouvoir discriminant et sont pertinentes à conserver pour la modélisation



# ---------------- Student test variable quantitative ----------------
# Age p-value < 2.2e-16
student_age <- t.test(Age ~ HeartDisease, data = patients)
student_age
# RestingBP p-value = 0.0008732
student_RestingBP <- t.test(RestingBP ~ HeartDisease, data = patients)
student_RestingBP
# Cholesterol p-value = 6.481e-14
student_Cholesterol <- t.test(Cholesterol ~ HeartDisease, data = patients)
student_Cholesterol
# MaxHR p-value < 2.2e-16
student_MaxHR <- t.test(MaxHR ~ HeartDisease, data = patients)
student_MaxHR
# Oldpeak p-value < 2.2e-16
student_Oldpeak <- t.test(Oldpeak ~ HeartDisease, data = patients)
student_Oldpeak

# ---------------- Cor test variable ----------------
# On isole les 5 variables quantitative
tabTest <- patients[, c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")]
# On fait un test de corrélation -> round sert a arrondir les resultats avec 2 chiffres après la virgule
test_cor <- round(cor(tabTest), 2)
test_cor
## On observe dans ce tableau que la valeur maximale est de -O,38 et 0,24 on élimine que si la différence entre 2 variables est (|r| > 0,7), on ne compte pas la diagonale.
## Si une variable à |r| > 0,7 alors on regarde laquelle des deux à le p-value le plus proche de 0 et on élimine l'autre
############### INTERPRETATION EXEMPLE ##################
## Le coefficient de corrélation linéaire entre l'âge et le cholestérol est très faible (r = -0,10), ce qui traduit une absence de relation linéaire significative entre l'âge du patient et son taux de cholestérol dans cet échantillon
## Le coefficient est négatif et modéré. Cela indique une relation inverse : plus l'âge d'un patient augmente, plus sa fréquence cardiaque maximale atteinte à l'effort (MaxHR) a tendance à diminuer


############################# MODELE #############################
indice_train <- sample(nrow(patients), size = 0.70 * nrow(patients))
train_data <- patients[indice_train, ]
test_data <- patients[-indice_train, ]

# entrainement du modele sur 70%
modele_reg_70 <- glm(HeartDisease ~ Sex +
                     ChestPainType +
                     FastingBS +
                     RestingECG +
                     ExerciseAngina +
                     ST_Slope +
                     Age +
                     RestingBP +
                     Cholesterol +
                     MaxHR +
                     Oldpeak,
                     data = train_data,
                     family = binomial)

summary(modele_reg_70)

# Prediction sur les 30%
test_data$proba_predict <- predict(modele_reg_70, newdata = test_data, type = "response")
test_data$prediction    <- ifelse(test_data$proba_predict >= 0.5, "Oui", "Non")

matrice_test <- table(Reel = test_data$HeartDisease, Predit = test_data$prediction)
matrice_test

Accuracy <- sum(diag(matrice_test)) / sum(matrice_test) * 100
cat("Accuracy sur le jeu de test (30%) : ", round(Accuracy, 2), "%\n")
