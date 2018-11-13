
# Doug Locke
# 3-31-2018
# Build & evaluate TRAINING model

install.packages("gmodels")
install.packages("LogisticDx")
install.packages("psych")
install.packages("car")

library(gmodels)
library(LogisticDx)
library(psych)
library(car)

# We will leverage this function as well. 

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1 - exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / (1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for Logistic Regression\n")
  cat("Hotitanicer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2          ", round(R.n, 3),     "\n")
}

# Remove scientific notation
options(scipen=999)

#  ---------- LOAD DATA ----------

train <- read.csv("train_set.csv")
titanic <- train
View(titanic)

# ---------- PRELIMINARY STEPS ----------

summary <- describe(titanic)
View(summary)

train3 = na.omit(train)
describe(train3)
titanic <- train3

# EXPLORATORY ANALYSIS
hist(titanic$SibSp)
hist(titanic$Pclass)
hist(titanic$Survived)
hist(titanic$Age)
hist(titanic$Fare)

quantile(titanic$Fare)
quantile(titanic$Fare, 0.95)
#34.86

table(titanic$Sex)  
table(titanic$SibSp)
table(titanic$Pclass)
table(titanic$Survived)
table(titanic$Parch)

# crosstables 

CrossTable(titanic$Sex, titanic$Pclass, expected = TRUE, format="SPSS")  # run the same with the other variables, in various combinations. Discuss your findings
CrossTable(titanic$Sex, titanic$Survived, expected = TRUE, format="SPSS") 

titanic$isChild[titanic$Age <=16] <- 1 ; titanic$isChild[titanic$Age > 16] <- 0 ; 
CrossTable(titanic$isChild, titanic$Survived, expected = TRUE, format="SPSS") 

CrossTable(titanic$Pclass, titanic$Survived, expected = TRUE, format="SPSS") 
CrossTable(titanic$Pclass, titanic$isHighFare, expected = TRUE, format="SPSS")
CrossTable(titanic$Pclass, titanic$Sex, titanic$Survived, expected = TRUE, format="SPSS")

#  ---------- CORE ASSIGNMENT ----------

# NAIVE MODEL........

titanic$Embarked_Q[titanic$Embarked == "Q"] <- 1 ; titanic$Embarked_Q[titanic$Embarked == "S"] <- 0 ; titanic$Embarked_Q[titanic$Embarked == "C"] <- 0
titanic$Embarked_S[titanic$Embarked == "Q"] <- 0 ; titanic$Embarked_S[titanic$Embarked == "S"] <- 1 ; titanic$Embarked_S[titanic$Embarked == "C"] <- 0
titanic$Embarked_C[titanic$Embarked == "Q"] <- 0 ; titanic$Embarked_C[titanic$Embarked == "S"] <- 0 ; titanic$Embarked_C[titanic$Embarked == "C"] <- 1
titanic$female[titanic$Sex == "male"] <- 0 ; titanic$female[titanic$Sex == "female"] <- 1
titanic$male[titanic$Sex == "female"] <- 0 ; titanic$male[titanic$Sex == "male"] <- 1
titanic$Class_1[titanic$Pclass == "1"] <- 1 ; titanic$Class_1[titanic$Pclass == "2"] <- 0 ; titanic$Class_1[titanic$Pclass == "3"] <- 0
titanic$Class_2[titanic$Pclass == "1"] <- 0 ; titanic$Class_2[titanic$Pclass == "2"] <- 1 ; titanic$Class_2[titanic$Pclass == "3"] <- 0
titanic$Class_3[titanic$Pclass == "1"] <- 0 ; titanic$Class_3[titanic$Pclass == "2"] <- 0 ; titanic$Class_3[titanic$Pclass == "3"] <- 1
titanic$SibSp_isMulti[titanic$SibSp > 1] <- 1 ; titanic$SibSp_isMulti[titanic$SibSp <= 1] <- 0 ; 
titanic$SibSp_isOne[titanic$SibSp == 1] <- 1 ; titanic$SibSp_isOne[titanic$SibSp != 1] <- 0 ; 
titanic$SibSp_isNone[titanic$SibSp == 0] <- 1 ; titanic$SibSp_isNone[titanic$SibSp > 0] <- 0 ; 
titanic$Parch_isMulti[titanic$Parch > 1] <- 1 ; titanic$Parch_isMulti[titanic$Parch <= 1] <- 0 ; 
titanic$Parch_isOne[titanic$Parch == 1] <- 1 ; titanic$Parch_isOne[titanic$Parch != 1] <- 0 ; 
titanic$Parch_isNone[titanic$Parch == 0] <- 1 ; titanic$Parch_isNone[titanic$Parch > 0] <- 0 ; 
titanic$Age_0_10[titanic$Age > 0]<- 0 ; titanic$Age_0_10[titanic$Age <=10] <- 1 ; 
titanic$Age_11_20[titanic$Age > 0] <- 0 ; titanic$Age_11_20[titanic$Age > 10 & titanic$Age <=20 ] <- 1 ;
titanic$Age_21_30[titanic$Age > 0] <- 0 ; titanic$Age_21_30[titanic$Age > 20 & titanic$Age <=30 ] <- 1 ;
titanic$Age_31_40[titanic$Age > 0] <- 0 ; titanic$Age_31_40[titanic$Age > 30 & titanic$Age <=40 ] <- 1 ;
titanic$Age_41_50[titanic$Age > 0] <- 0 ; titanic$Age_41_50[titanic$Age > 40 & titanic$Age <=50 ] <- 1 ;
titanic$Age_51_60[titanic$Age > 0] <- 0 ; titanic$Age_51_60[titanic$Age > 50 & titanic$Age <=60 ] <- 1 ;
titanic$Age_61_70[titanic$Age > 0] <- 0 ; titanic$Age_61_70[titanic$Age > 60 & titanic$Age <=70 ] <- 1 ;
titanic$Age_71_110[titanic$Age > 0] <- 0 ; titanic$Age_71_110[titanic$Age > 70 & titanic$Age <=110 ] <- 1 ;

#90th percentile for high fare
titanic$isHighFare[titanic$Fare >=120] <- 1 ; titanic$isHighFare[titanic$Fare < 120 ] <- 0 ;
titanic$Survived_value[titanic$Survived == 0] <- "Died" ; titanic$Survived_value[titanic$Survived == 1] <- "Survived"
View(titanic)

# (1) Create Model

titanic_final <- glm(Survived ~ female + Class_1 + Class_2 + Age_0_10 + Age_11_20 + Age_21_30 + Age_31_40 + SibSp_isMulti, family=binomial, data=titanic)
summary(titanic_final)
exp(coef(titanic_final))

prob_final = predict(titanic_final,type="response")
titanic$prob_final <- prob_final

quantile(titanic$prob_final)

pred_final = rep("Died", 564) 
pred_final[titanic$prob_final>0.50] = "Survived" 
titanic$pred_final <- pred_final

table(pred_final,titanic$Survived_value)
misClassifiError = mean(pred_final != titanic$Survived_value)
print(paste('Accuracy', 1 - misClassifiError))

exp(coef(titanic_final))
logisticPseudoR2s(titanic_final)

library(pROC)
rocplot <- plot.roc(titanic$Survived_value,titanic$prob_final)
plot(rocplot)

