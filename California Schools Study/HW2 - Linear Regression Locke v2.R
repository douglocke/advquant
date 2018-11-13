#**********************R Syntax for Regression Assignment******************

# Please note that this is a text file -- not a word doc. 
# You will be able to copy paste everything straight from this document into R, or open the R version of this file posted on canvas.
# The best way to discuss your progress through this lab is to either (1) run through in R and use screenshots copied to word to show work, or (2) utilize R Notebook or R Markdown. 

# The first task when we open RStudio is to set our working directory. As covered in both of the optional labs, and in the documentation, navigate in the files window (bottom right) to your documents folder for this class. In "More", click "Set as Working Directory".
# Now let's load in the dataset. You can either open it from your working directory (if you've saved it there) or pull it up with the below command. Change the file path to make sure it matches yours. 

calschooldist <- read.csv("calschooldist.csv")
View(calschooldist)

# We can rename our dataset as an object. You can name this whatever you want, but I recommend calschool because it will be in line with the code below.

calschool <- calschooldist

# We need to load "packages" in R which include commands and functions that we can leverage. These packages will vary based on the lab. Today we are going to be using the below for the assignment. You only need to do this once:

install.packages("car")
install.packages("psych")
install.packages("corrplot")

# Activate these packages: You need to do this every time you open R. We've only installed them, now we need to activate them for this instance. 

library(car)  
library(psych)
library(corrplot)

# If you're having trouble installing the car package, try the following code:

install.packages("lme4")
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
install.packages(packageurl, repos=NULL, type="source")
install.packages("car")
library("car")

# To see our numbers (particularly the p values) more clearly, we can remove scientific notation. We need to repeat this every time we open R.  

options(scipen=999)

# Now that our working directory is set, our packages are loaded, we can begin the steps outlined in the prompt.

# STEP 1: no R code to complete this

# STEP 2: no R code to complete this

# STEP 3: no R code to complete this

# STEP 4: Perform basic checks of the candidate variables. Do you have any missing value or out of range data problems? If so, what did you do to resolve them, if anything?
# First, let's look at plots to get a feel for the data. In R, we can just use "plot" to create a scatterplot of two variables. We have to define those two items. Let's plot our dependent variable, acadperf, against some of the independent variables. 

plot(calschool$acadperf,calschool$meals)
plot(calschool$acadperf,calschool$ell)
plot(calschool$acadperf,calschool$yr_rnd)
plot(calschool$acadperf,calschool$hsg)

# Have you noticed that any have a strong correlation? What does the graph of acadperf vs ell tell us about the ell data? Confirm this with the str code we completed in the lab. 
# We can try a new function here, called "describe". This will give us new statistics that are a little different than "summary". 

describe(calschool) 

# Here we can see missing values, ranges, measures of central tendency, and standard deviation. What are your inferences about these values? I can see that most variables include 400 total entries, but mobility and acs have less than 400. I have to remove the nulls with the following code:   

calschool_missing_teacher_credentials <- subset(calschool,(hsg==0 & not_hsg==0 & some_col==0 & col_grad==0 & grad_sch==0))
describe(calschool_missing_teacher_credentials)

calschooldist2=na.omit(calschooldist)
describe(calschooldist2)

remove the 19 schools with no education data
calschooldist3 <- subset(calschooldist2,!(hsg==0 & not_hsg==0 & some_col==0 & col_grad==0 & grad_sch==0))
describe(calschooldist3)


hist(calschool$meals)
hist(calschool$ell)
hist(calschool$not_hsg)
hist(calschool$grad_sch)
hist(calschool$col_grad)
hist(calschool$yr_rnd)
hist(calschool$emer)
hist(calschool$full)
hist(calschool$hsg)
hist(calschool$some_col)
hist(calschool$enroll)


dev.off()

plot(calschool$meals, calschool$acadperf, main="Scatterplot Meals & Academic Performance", 
     xlab="Meals ", ylab="Academic Performance ")
meals_fitline <- lm(calschool$acadperf ~ calschool$meals)
abline(meals_fitline)

plot(calschool$ell, calschool$acadperf, main="Scatterplot English Language Learners & Academic Performance", 
     xlab="English Language Learners % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$ell)
abline(fitline)

dev.off()
plot(calschool$not_hsg, calschool$acadperf, main="Scatterplot Not_HSG & Academic Performance", 
     xlab="Not_Hsg % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$not_hsg)
abline(fitline)

dev.off()
plot(calschool$grad_sch, calschool$acadperf, main="Scatterplot Grad_Sch & Academic Performance", 
     xlab="Grad_sch % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$grad_sch)
abline(fitline)

plot(calschool$col_grad, calschool$acadperf, main="Scatterplot Col_grad & Academic Performance", 
     xlab="Col_grad % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$col_grad)
abline(fitline)

plot(calschool$yr_rnd, calschool$acadperf, main="Scatterplot Yr_rnd & Academic Performance", 
     xlab="Year Round School ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$yr_rnd)
abline(fitline)

plot(calschool$emer, calschool$acadperf, main="Scatterplot Emer & Academic Performance", 
     xlab="Emergency Teacher Credential % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$emer)
abline(fitline)

plot(calschool$full, calschool$acadperf, main="Scatterplot Full & Academic Performance", 
     xlab="Full Teacher Credential % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$full)
abline(fitline)

plot(calschool$hsg, calschool$acadperf, main="Scatterplot Hsg & Academic Performance", 
     xlab="HSG % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$hsg)
abline(fitline)

plot(calschool$some_col, calschool$acadperf, main="Scatterplot Some_col & Academic Performance", 
     xlab="Some_col % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$some_col)
abline(fitline)

plot(calschool$enroll, calschool$acadperf, main="Scatterplot enroll & Academic Performance", 
     xlab="Some_col % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$enroll)
abline(fitline)

# Now `calschooldist2` does not include any null values. Lets replace calschool so we don't have these NA values. 

calschool <- calschooldist3

# Step 5: What did your check of the correlation matrix find? Did you add any variables to the end of you list based on it? Does it look like you need to worry about multicollinearity? 
# Do you remember this from the lab? This is a function that gives us not only our Pearson's Coefficients, but also gives us our p-values too! This function will be loaded into R, then when we run cor.prob with our data, we'll get the output.
# When looking at output generated by cor.prob, remember, p values are above and coefficients are below the diagonal line. 

cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

# Now that we have added the function, let's run it 
describe(calschool)
correlation_table_calschool <- cor.prob(calschool)
View(correlation_table_calschool)
dev.off()
hist(calschool$acadperf)
calschool

# You can save this table to your working directory with the following code:

write.csv(correlation_table_calschool, file = "Correlation Matrix California Schools.csv")

# Are there any noteworthy correlations that might help you build your model? 
# Which variables have the strongest relationships with academic performance?
# Would these variables be good to include in a regression analysis?

plot(calschool$enroll, calschool$acadperf, main="Scatterplot enroll & Academic Performance", 
     xlab="Enroll % ", ylab="Academic Performance ")
fitline <- lm(calschool$acadperf ~ calschool$enroll)
abline(fitline)



# STEP 6: no R code to complete this

# STEP 7: REGRESSION! Add your first independent variable. Show your bivariate / unadjusted model. Did it accord with your expectations? 
# Now we'll see a new command, "lm". This will fit a simple regression model to the data. The format for this code is lm(y~x, data) where y is the response (dependent variable), x is the predictor (independent variable), and the data is calschool.  
# Give it a try with your first variable, but YOU MUST CHANGE the variable name where I have [your first variable]: 

regression_1 <- lm(acadperf ~ [your first variable], data = calschool)

# This is what it would look like if "meals" was your first variable

regression_1 <- lm(acadperf ~ meals, data = calschool)
summary(regression_2) 

dev.off()
plot(calschool$meals, calschool$acadperf, main="Scatterplot Meals & Academic Performance", 
     xlab="Meals ", ylab="Academic Performance ")
abline(regression_1)

plot(regression_1)

# This tells us the results of our regression. Is it in line with your expectations?

# STEP 8: Check for regression violations for this bivariate mode. Did you find any major violations? 
# To analyze residuals and test assumptions, we can explore some graphs. We'll start with a histogram of the residuals.

hist(scale(regression_7$residuals))

# Let's tell R to layout our graphs in a matrix so we can easily view 4 graphs at once. 
# The plot() function graphs 4 helpful plots for us. 

layout(matrix(c(1,2,3,4),2,2))
VIF(regression_7)

# Clear your plot display to reset the layout.
dev.off()

# What if we want standardized coefficients? R is a little difficult in that it doesn't give them as part of the standard output. 
# By using "scale" in our lm function, we can standardize the unit of analysis to compare coefficients. Input your first variable.

lm(scale(acadperf) ~ scale([your first variable]), data = calschool)

# Mine will look like this:

lm(scale(acadperf) ~ scale(meals), data = calschool)

# How can the standardized coefficient be interpreted?
# How about standardized residuals? We'll start by viewing all residuals. R has a function "names()" so we can learn more about what information is available. 

names(regression_1)

# You can see that we have access to the coefficients and residuals of this regression. This can be an easy way to look only at the relevant data. 

coefficients(regression_7)

# We can also specify the entire set of residuals of each point (residual = predicted value - actual value)

regression_7$residuals 

# Next, we'll create a component of our regression to store our standardized residuals. 
#We'll use the standardized residuals to determine if any have an absolute value greater than 2.  

regression_1$standardized.residuals <- rstandard(regression_1)
regression_1$large_residual <- regression_1$standardized.residuals >2 | regression_1$standardized.residuals < -2
sum(regression_1$large_residual) 

hist(regression_1$standardized.residuals )
mean(regression_1$standardized.residuals )
# What is your interpretation of the results?


regression_1$very_large_residual <- regression_1$standardized.residuals >3 | regression_1$standardized.residuals < -3
sum(regression_1$very_large_residual)

regression_1$very_large_residual

# Let's calculate the Durban-Watson statistic.

dwt(regression_1)

# STEP 9: Sequentially build up the model adding variables in the order you specified (don't check reg. assumptions at each stage) 
# To build variables into your model, continue to use the lm() function, and add the variable on the back side of the ~. You should update the name of the model so you can save each iteration in R. The form is below. Don't forget to replace with your variables in the order that you defined.

regression_2 <- lm(acadperf ~ meals + ell, data = calschool)

regression_3 <- lm(acadperf ~ meals + ell + not_hsg, data = calschool)

regression_11 <- lm(acadperf ~ meals + ell + not_hsg + grad_sch + yr_rnd + emer, data = calschool)

summary(regression_7)
vif(regression_7)
hist(acadperf)
regression_final <- lm(acadperf ~ YOUR_VARIABLE_1 + YOUR_VARIABLE_2 + YOUR_VARIABLE_3 + YOUR_VARIABLE_4 + YOUR_VARIABLE_5 + YOUR_VARIABLE_6, data = calschool)
summary(regression_final)

# As an example, this is the form:



# STEP 10: Recheck model assumptions
# Once we have our final model, let's check assumptions through residual analysis as we did earlier.

hist(scale(regression_7$residuals))
install.packages("QuantPsyc")
library("QuantPsyc")
lm.beta(regression_7)
# View all plots at once

layout(matrix(c(1,2,3,4),2,2))
plot(regression_7)

# Clear your plot display to reset the layout.
dev.off()

# Standardized residual analysis (how many residuals are more than two deviations away)

regression_7$standardized.residuals <- rstandard(regression_7)
regression_7$large_residual <- regression_7$standardized.residuals >3 | regression_7$standardized.residuals < -3
sum(regression_7$large_residual)

# Calculate the standardized coefficients as we did prior.

lm(scale(acadperf) ~ scale(YOUR_VARIABLE_1) + scale(YOUR_VARIABLE_2) + scale(YOUR_VARIABLE_3) + scale(YOUR_VARIABLE_4) + scale(YOUR_VARIABLE_5) + scale(YOUR_VARIABLE_6), data = calschool) 

# Mine will look like this:

lm(scale(acadperf) ~ scale(meals) + scale(hsg) + scale(some_col), data = calschool) 

# You may have more (or fewer) variables in your model in comparison to my examples and that is okay. Just make sure all your variables in your final model are in standardized coefficients. 
# Now that we have multiple terms in the model, lets include the multicollinearity test as well as Durbin-Watson. 

vif(regression_final)
dwt(regression_final)

# Discuss these results in your report and/or technical appendix. Don't forget to include an advanced extension, in a different file.

corrmatrix <- cor(calschool, use = "complete.obs")

dev.off()
corrplot.mixed(corrmatrix, number.cex = 0.6, tl.cex = 0.45)
corrplot(corrmatrix, method="square")


install.packages("stargazer")
install.packages("lmtest")
install.packages("sjPlot")
install.packages("purrr")
library(stargazer)
library(lmtest)
library(sjPlot)
library(purrr)
stargazer(regression_1, regression_7, title="Regression Results", dep.var.labels=c("Academic Performance"), type="text")

sjt.lm(regression_1, 
       show.header = TRUE,
       p.numeric = FALSE,
       show.se = TRUE,
       show.fstat = TRUE, 
       string.est = "Estimate",
       string.ci = "Conf. Int.",
       string.dv = "Unadjusted Regression Model",
       depvar.labels = c("Academic Performance"),
       pred.labels = c("Percent of students receiving free meals"))



