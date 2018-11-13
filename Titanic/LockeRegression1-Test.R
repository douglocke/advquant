
# Douglas Locke
# Adv Quant  3-31-2018
# This file build the final model using the train data
# and then runs the testing data against the trained model

install.packages("gmodels")
install.packages("LogisticDx")
install.packages("psych")
install.packages("car")

# Now, lets load them into our current working session. 

library(gmodels)
library(LogisticDx)
library(psych)
library(car)

# We will leverage this function as well. Run the below code to load it into your environment.

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
titanic_train <- train
View(titanic_train)

test <- read.csv("test_set.csv")
titanic_test <- test
View(titanic_test)

# ---------- PRELIMINARY STEPS ----------

train = na.omit(titanic_train)
describe(train)

test = na.omit(titanic_test)
describe(test)

#no null values
titanic <- train
titanic_test <- test


#build dummies & Variable prep TRAIN  
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
titanic$isHighFare[titanic$Fare >=120] <- 1 ; titanic$isHighFare[titanic$Fare < 120 ] <- 0 ;
titanic$Survived_value[titanic$Survived == 0] <- "Died" ; titanic$Survived_value[titanic$Survived == 1] <- "Survived"

#build dummies & Variable prep TEST  
titanic_test$Embarked_Q[titanic_test$Embarked == "Q"] <- 1 ; titanic_test$Embarked_Q[titanic_test$Embarked == "S"] <- 0 ; titanic_test$Embarked_Q[titanic_test$Embarked == "C"] <- 0
titanic_test$Embarked_S[titanic_test$Embarked == "Q"] <- 0 ; titanic_test$Embarked_S[titanic_test$Embarked == "S"] <- 1 ; titanic_test$Embarked_S[titanic_test$Embarked == "C"] <- 0
titanic_test$Embarked_C[titanic_test$Embarked == "Q"] <- 0 ; titanic_test$Embarked_C[titanic_test$Embarked == "S"] <- 0 ; titanic_test$Embarked_C[titanic_test$Embarked == "C"] <- 1
titanic_test$female[titanic_test$Sex == "male"] <- 0 ; titanic_test$female[titanic_test$Sex == "female"] <- 1
titanic_test$male[titanic_test$Sex == "female"] <- 0 ; titanic_test$male[titanic_test$Sex == "male"] <- 1
titanic_test$Class_1[titanic_test$Pclass == "1"] <- 1 ; titanic_test$Class_1[titanic_test$Pclass == "2"] <- 0 ; titanic_test$Class_1[titanic_test$Pclass == "3"] <- 0
titanic_test$Class_2[titanic_test$Pclass == "1"] <- 0 ; titanic_test$Class_2[titanic_test$Pclass == "2"] <- 1 ; titanic_test$Class_2[titanic_test$Pclass == "3"] <- 0
titanic_test$Class_3[titanic_test$Pclass == "1"] <- 0 ; titanic_test$Class_3[titanic_test$Pclass == "2"] <- 0 ; titanic_test$Class_3[titanic_test$Pclass == "3"] <- 1
titanic_test$SibSp_isMulti[titanic_test$SibSp > 1] <- 1 ; titanic_test$SibSp_isMulti[titanic_test$SibSp <= 1] <- 0 ; 
titanic_test$SibSp_isOne[titanic_test$SibSp == 1] <- 1 ; titanic_test$SibSp_isOne[titanic_test$SibSp != 1] <- 0 ; 
titanic_test$SibSp_isNone[titanic_test$SibSp == 0] <- 1 ; titanic_test$SibSp_isNone[titanic_test$SibSp > 0] <- 0 ; 
titanic_test$Parch_isMulti[titanic_test$Parch > 1] <- 1 ; titanic_test$Parch_isMulti[titanic_test$Parch <= 1] <- 0 ; 
titanic_test$Parch_isOne[titanic_test$Parch == 1] <- 1 ; titanic_test$Parch_isOne[titanic_test$Parch != 1] <- 0 ; 
titanic_test$Parch_isNone[titanic_test$Parch == 0] <- 1 ; titanic_test$Parch_isNone[titanic_test$Parch > 0] <- 0 ; 
titanic_test$Age_0_10[titanic_test$Age > 0]<- 0 ; titanic_test$Age_0_10[titanic_test$Age <=10] <- 1 ; 
titanic_test$Age_11_20[titanic_test$Age > 0] <- 0 ; titanic_test$Age_11_20[titanic_test$Age > 10 & titanic_test$Age <=20 ] <- 1 ;
titanic_test$Age_21_30[titanic_test$Age > 0] <- 0 ; titanic_test$Age_21_30[titanic_test$Age > 20 & titanic_test$Age <=30 ] <- 1 ;
titanic_test$Age_31_40[titanic_test$Age > 0] <- 0 ; titanic_test$Age_31_40[titanic_test$Age > 30 & titanic_test$Age <=40 ] <- 1 ;
titanic_test$Age_41_50[titanic_test$Age > 0] <- 0 ; titanic_test$Age_41_50[titanic_test$Age > 40 & titanic_test$Age <=50 ] <- 1 ;
titanic_test$Age_51_60[titanic_test$Age > 0] <- 0 ; titanic_test$Age_51_60[titanic_test$Age > 50 & titanic_test$Age <=60 ] <- 1 ;
titanic_test$Age_61_70[titanic_test$Age > 0] <- 0 ; titanic_test$Age_61_70[titanic_test$Age > 60 & titanic_test$Age <=70 ] <- 1 ;
titanic_test$Age_71_110[titanic_test$Age > 0] <- 0 ; titanic_test$Age_71_110[titanic_test$Age > 70 & titanic_test$Age <=110 ] <- 1 ;
titanic_test$isHighFare[titanic_test$Fare >=120] <- 1 ; titanic_test$isHighFare[titanic_test$Fare < 120 ] <- 0 ;
titanic_test$Survived_value[titanic_test$Survived == 0] <- "Died" ; titanic_test$Survived_value[titanic_test$Survived == 1] <- "Survived"

# (1) Create Model

titanic_final <- glm(Survived ~ female + Class_1 + Class_2 + Age_0_10 + Age_11_20 + Age_21_30 + Age_31_40 + SibSp_isMulti, family=binomial, data=titanic)
summary(titanic_final)
exp(coef(titanic_final))

prob_final = predict(titanic_final, titanic_test, type="response")
titanic_test$prob_final <- prob_final
quantile(titanic_test$prob_final)
# (2) Run Diagnostics

pred_final = rep("Died", 148) 
pred_final[titanic_test$prob_final>0.50] = "Survived" 
titanic_test$pred_final <- pred_final

table(pred_final,titanic_test$Survived_value)
misClassifiError = mean(pred_final != titanic_test$Survived_value)
print(paste('Accuracy', 1 - misClassifiError))

