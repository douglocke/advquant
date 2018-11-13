
# Welcome to the Logistic Regression Lab. As discussed in the assignment, we are leveraging the titanic dataset from Kaggle. 

# ---------- PREP ENVIRONMENT ----------

# First, make sure you have the appropriate packages installed. You only have to do this once. 

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

# This version of the Logistic lab leverages the titanic dataset from Kaggle
# The data is loaded in the files on canvas and is called "train_set". Lets open it, then add it as an object.
# We'll call it "titanic" for ease and understanding.


train <- read.csv("train_set.csv")
titanic <- train
View(titanic)

# ---------- PRELIMINARY STEPS ----------
# 1.	Before we do any statistical analysis we should get to know our dataset and our sample. 
# This typically involves running some frequencies and means. The teaching assistant will show you how to do this.

# a.	Summarize the variables in the dataset. Discuss measures of central tendency and variation.

summary <- describe(titanic)
View(summary)

# Discuss your findings. 

# You'll notice that the "Age" column is not entirely populated. While there are a lot of solutions for this missing data issue,
# we will remove the individuals that do not have a reported age. 

train3 = na.omit(train)
describe(train3)

# Now `train2` does not include any null values. Lets replace the object "titanic" so we don't have these NA values while we do our analysis. 

titanic <- train3

# b.	Run frequencies on the following variables:  Sex, SibSp, Pclass, Survived.  
# Report the basic frequencies. Use the description of the dataset above to help you interpret the output. 
# This will explain what the "0" and "1" codes mean.

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

# c.	Analyze how the various variables differ by levels of the Survuved variable. This will give you an overview for the modelling. 
# We use CrossTable () to show the relationship between two categorical variables. 

CrossTable(titanic$Sex, titanic$Pclass, expected = TRUE, format="SPSS")  # run the same with the other variables, in various combinations. Discuss your findings

CrossTable(titanic$Sex, titanic$Survived, expected = TRUE, format="SPSS") 

titanic$isChild[titanic$Age <=16] <- 1 ; titanic$isChild[titanic$Age > 16] <- 0 ; 
CrossTable(titanic$isChild, titanic$Survived, expected = TRUE, format="SPSS") 

CrossTable(titanic$Pclass, titanic$Survived, expected = TRUE, format="SPSS") 

CrossTable(titanic$Pclass, titanic$isHighFare, expected = TRUE, format="SPSS")

titanic$isHighFare


#  ---------- CORE ASSIGNMENT ----------

# NAIVE MODEL........

# First, lets make some new columns that will help our analysis.
# We will create a dummy variable for all cities listed in Embark.
# We will take the "Survived" column and create a new one with the actual value in words. 
# We will also make a dummy variable for both males and females.

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

#upper quartile for high fare
titanic$isHighFare[titanic$Fare >=120] <- 1 ; titanic$isHighFare[titanic$Fare < 120 ] <- 0 ;


titanic$Survived_value[titanic$Survived == 0] <- "Died" ; titanic$Survived_value[titanic$Survived == 1] <- "Survived"
View(titanic)

# (1) Create Model
# Use "glm" to create a naive model, which calculates the probability of the survival without any predictor values.
# This probability, therefore, will be the same for everyone. 

titanic_naive <- glm(Survived ~ 1, data = titanic, family = binomial())
summary(titanic_naive)

# (2) Insert model probability into our dataset
# Now our model is able to assign probabilities of survival to each individual. 
# To do that, create an object "prob_naive" to store the prediction based on the naive model.

prob_naive = predict(titanic_naive,type="response") # this is where we predict their probability based on their characteristics
titanic$prob_naive <- prob_naive # we can insert that probability into the dataset

# This probability value is now stored for each data element in our dataset. View for yourself, you should see the extra column:
View(titanic)

# Let's view the quantile breakdown of these probabilities:
quantile(titanic$prob_naive)

# This result shows that the probabilities are the same for each data point. This makes sense, because our model doesn't have any parameters.
# The naive model just bases the probability on the overall probability -- (# survived) / (total)

# (3) Predict "Survived" or "Died" based on model
# we'll create a vector that predicts "Survived" or "Died" based on our model's probabilities. A little background here: a vector is just a fancy
# column, or list of elements. It is similar to an array. In this vector, each value will take on the value of "died", unless we tell this value 
# to be survived. We will tell it to change to survived if the probability of survival is high. In this case, that threshold will be 50%. 

pred_naive = rep("Died", 564) # Creates a vector of 564 "Died" elements
pred_naive[titanic$prob_naive>0.5] = "Survived" # Changes the "Died" elements to "Survived" if probability for that element from the model is above 0.5
pred_naive[1:40] # Prints the first 40 elements
titanic$pred_naive <- pred_naive # Adds our vector to our dataset so we can view it next to our data

# Now your predictions are available in your dataset. Take another look at titanic and you'll see your prediction. 
View(titanic)
# Yikes, it looks like everyone died. Why is that? Well, the probability of survival (without any predictors) is 0.40. So, if our 
# prediction is based on the probability being above 50%, and no probabilities are above 50%, then everyone is listed as "died". 
# We know conceptually this didn't happen. Some people survived. What are the reasons they survived? These will be helpful in our models. 

# Lets compare our predictions to reality. How many false positives (died, predicted survived) and false negatives (survived, predicted died) does this model have?

table(titanic$pred_naive) # This tells us: what are the results of our prediction model? 
table(pred_naive,titanic$Survived_value) # This creates a confusion table comparing our prediction to reality. 
# Our prediction is on the vertical, the real result is on the horizontal. 

# Calculate your prediction accuracy

misClassifiError = mean(pred_naive != titanic$Survived_value)
print(paste('Accuracy', 1 - misClassifiError))

# FIRST MODEL WITH PREDICTORS...........

# (1) Create Model

titanic_1 <- glm(Survived ~ female, family=binomial, data=titanic)
summary(titanic_1)

# (2) Run Diagnostics
exp(coef(titanic_1))
#your likliehood 11x more likey to survive if you're female

# look up Pseddo R2, look at neagelkerke .... how much of model variance you're accounting for.

logisticPseudoR2s(titanic_1)

# (3) Insert model probability into our dataset

prob_1 = predict(titanic_1,type="response")
titanic$prob_1 <- prob_1
quantile(titanic$prob_1)

# (4) Predict "Survived" or "Died" based on model

pred_1 = rep("Died", 564) # Creates a vector of 564 "Died" elements
pred_1[titanic$prob_1>0.5] = "Survived" # Changes the "Died" elements to "Survived" if probability is above 0.5
pred_1[0:40]
titanic$pred_1 <- pred_1

table(titanic$pred_1)
table(pred_1,titanic$Survived_value) # Confusion table that compares our prediction to the Survived that was reality. How many false positves/negatives do you have?

# Calculate your prediction accuracy

misClassifiError = mean(pred_1 != titanic$Survived_value)
print(paste('Accuracy', 1 - misClassifiError))

# CONTINUE BUILDING ALL MODELS.........

# Repeat the above code for all your models. Here's a template, although you may want to expand your analysis to find a final model:
View(titanic)

titanic_2 <- glm(Survived ~ female + Class_1 + Class_2 + Age_0_10 + Age_11_20 + Age_21_30 + Age_31_40 + SibSp_isMulti, family=binomial, data=titanic)
summary(titanic_2)
exp(coef(titanic_2))

prob_2 = predict(titanic_2,type="response")
titanic$prob_2 <- prob_2

quantile(titanic$prob_2)
# (2) Run Diagnostics

pred_2 = rep("Died", 564) 
pred_2[titanic$prob_2>0.50] = "Survived" 
titanic$pred_2 <- pred_2

table(pred_2,titanic$Survived_value)
misClassifiError = mean(pred_2 != titanic$Survived_value)
print(paste('Accuracy', 1 - misClassifiError))


exp(coef(titanic_2))
logisticPseudoR2s(titanic_2)


# Repeat the same process for as many models as you'd like to make before you decide on your final model

# BUILD YOUR FINAL MODEL...........

# (1) Create Model

titanic_final <- glm(Survived ~ female + Age + Embarked_Q, family=binomial, data=titanic)
summary(titanic_final)

# (2) Run Diagnostics

exp(coef(titanic_final))
logisticPseudoR2s(titanic_final)

# (3) Insert model probability into our dataset

prob_final = predict(titanic_final,type="response")
titanic$prob_final <- prob_final

quantile(titanic$prob_final)

# (4) Predict "Survived" or "Died" based on model

pred_final = rep("Died", 564) # Creates a vector of 564 "Died" elements
pred_final[titanic$prob_final>0.5] = "Survived" # Changes the "Died" elements to "Survived" if probability is above 0.5
pred_final[1:40]
titanic$pred_final <- pred_final

table(titanic$pred_final)
table(pred_final,titanic$Survived_value) # Confusion table that compares our prediction to the Survived that was reality

# Calculate your prediction accuracy

misClassifiError = mean(pred_final != titanic$Survived_value)
print(paste('Accuracy', 1 - misClassifiError))


#Doug 
#MODEL, PROBABILITY, PREDICTION
# testing set is one of the easiest.


# slopes negative   relationship negative  not enough to say there is a relationship
# if independent goes up by 1 then what happen?  
# explain odds ratio - expotentiaed ... focus here on lecture notes .

#interpreting    male adult third class.... in interpretation, 

#if you have an interaction term, you should also have a main effects()
# do 2  2 way interactions, male by class then male by adult.
# do a 3 way cross tab to understand before going to coeifficients
# look at regression notes on interation 
