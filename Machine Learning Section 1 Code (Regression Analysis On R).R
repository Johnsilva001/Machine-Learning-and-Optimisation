
##############  Load Libraries  ############################################################

library(readr)
library(plyr)
library(lmtest)
library(car)
library(car)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)

######## Importing the Data Set ############################################################

Ins <- read.csv("./insurance.csv")
Ins

###############  Head View Of The Data Set   ###############################################

head(Ins)

############## General Descriptive Statistics Of The Insurance Data Set  ###################

summary(Ins)

############################################################################################
###### Descriptive and Visualisation Analysis For Sex ######################################
############################################################################################

sx <- table(Ins$sex)
sx
sx1 <- as.data.frame(ag)
names(sx1)[c(1,2)] <- c("Sex", "Count")

sx1 %>%
  ggplot(aes(x = Sex, y = Count, fill = Sex)) + 
  geom_col() +
  theme(legend.position = 'none') 

describeBy(Ins$medicalCost, Ins$sex)

ggplot(data = Ins,aes(sex,medicalCost)) + geom_boxplot(fill = c(2:3)) +
   ggtitle("Boxplot of Medical Cost by Sex")

############################################################################################
###### Descriptive and Visualisation Analysis For Age ######################################
############################################################################################

ag <- table(Ins$age)
ag
ag1 <- as.data.frame(ag)
names(ag1)[c(1,2)] <- c("Age", "Count")
ne <- data.frame(ag1[(1:10),], ag1[(11:20),], ag1[(21:30),], ag1[(31:40),])
names(ne)[1:8] <- c("Age", "Count", "Age", "Count", "Age", "Count", "Age", "Count")
ne
ag1[(41:47),]

ag1 %>%
  ggplot(aes(x = Age, y = Count, fill = Age)) + 
  geom_col() +
  theme(legend.position = 'none')

############################################################################################
###### Descriptive and Visualisation Analysis For Smokers ##################################
############################################################################################

sm <- table(Ins$smoker)
sm
sm1 <- as.data.frame(sm)
names(sm1)[c(1,2)] <- c("Smoker", "Count")

sm1 %>%
  ggplot(aes(x = Smoker, y = Count, fill = Smoker)) + 
  geom_col() +
  theme(legend.position = 'none') 

describeBy(Ins$medicalCost,Ins$smoker)

ggplot(data = Ins,aes(smoker,medicalCost)) + geom_boxplot(fill = c(2:3)) +
   ggtitle("Boxplot of Medical Cost by Smoker")
###############################################################################################
###### Descriptive and Visualisation Analysis For Region ######################################
###############################################################################################

rg <- table(Ins$region)
rg
rg1 <- as.data.frame(rg)
names(rg1)[c(1,2)] <- c("Region", "Count")

rg1 %>%
  ggplot(aes(x = Region, y = Count, fill = Region)) + 
  geom_col() +
  theme(legend.position = 'none') 

describeBy(Ins$medicalCost, Ins$region)

ggplot(data = Ins,aes(region,medicalCost)) + geom_boxplot(fill = c(2:5)) +
   ggtitle("Boxplot of Medical Cost per Region")

#############################################################################################
###### Descriptive and Visualisation Analysis For Children ##################################
#############################################################################################

ch <- table(Ins$children)
ch
ch1 <- as.data.frame(ch)
names(ch1)[c(1,2)] <- c("Children", "Count")

ch1 %>%
  ggplot(aes(x = Children, y = Count, fill = Children)) + 
  geom_col() +
  theme(legend.position = 'none') 

describeBy(Ins$medicalCost, Ins$children)

ggplot(data = Ins,aes(as.factor(children),medicalCost)) + geom_boxplot(fill = c(2:7)) +
    xlab("children") + ggtitle("Boxplot of Medical Cost by Number of Children")

########################################################################################
###### Descriptive and Visualisation Analysis For BMI ##################################
########################################################################################

# Create new variable derived from bmi
Ins$obesity <- ifelse(Ins$bmi>=30,"yes","no")

head(Ins)

ob <- table(Ins$obesity)
ob
ob1 <- as.data.frame(ob)
names(ob1)[c(1,2)] <- c("Obesity", "Count")

ob1 %>%
  ggplot(aes(x = Obesity, y = Count, fill = Obesity)) + 
  geom_col() +
  theme(legend.position = 'none') 

# By obesity status
describeBy(Ins$medicalCost,Ins$obesity)

ggplot(data = Ins,aes(obesity,medicalCost)) + geom_boxplot(fill = c(2:3)) +
  xlab("obesity") + ggtitle("Boxplot of Medical Cost by obesity")


##################################################################################################
#################  Checking Correlation Between Variables  #######################################
##################################################################################################

pairs.panels(Ins[c("medicalCost", "age", "bmi", "children",  "sex", "region", "smoker")])

##################################################################################################
######################### Assumption Of Linear Regression Checking  ##############################
##################################################################################################

llm <- lm(medicalCost ~ age + bmi + children + sex + region + smoker, data = Ins)

#############  Normality Assumption Checking  ####################################################

# Perform Shapiro-Wilk Normality Test (W)
shapiro.test(llm$residuals) # Failed assumption

##### Homoscedasticity Assumption  ###############################################################

# Perform Breusch-Pagan Test (BP)
bptest(llm) # Failed Homoscedasticity Assumption

##### Autocorrelation Assumption  ################################################################

# Perform Durbin-Watson test (DW)
dwtest(llm) ## Passed Autocorrelation Assumption

###########   Multicollinearity Assumption #######################################################

# Perform Variance Inflation Factor Test (VIF) 
vif(llm)  ##  Passed Multicollinearity Assumption

##################################################################################################
#############  Simple Linear Regression (Smoker and Medical Cost)   ##############################
##################################################################################################
slm <- lm(medicalCost ~ smoker, data = Ins)
summary(slm)

##################################################################################################
#############  Simple Linear Regression (Age and Medical Cost)   ##############################
##################################################################################################
alm <- lm(medicalCost ~ age, data = Ins)
summary(alm)

##################################################################################################
#############  Simple Linear Regression (BMI and Medical Cost)   ##############################
##################################################################################################
blm <- lm(medicalCost ~ bmi, data = Ins)
summary(blm)

##################################################################################################
#############  Multiple Linear Regression (BMI, AGE, AND SMOKER)   ###############################
##################################################################################################

mlm1 <- lm(medicalCost ~ bmi + age + smoker, data = Ins)
summary(mlm1)

##################################################################################################
########  Multiple Linear Regression (BMI, AGE, SEX, CHILDREN, REGION, AND SMOKER)   #############
##################################################################################################

ins <- Ins

ins$sex <- as.factor(ins$sex)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)

mlm2 <- lm(medicalCost ~ age + bmi + children + sex + region + smoker, data = ins)
summary(mlm2)

##################################################################################################
#################  Shapley Value Regression   ####################################################
##################################################################################################

mlm2_shapley <-calc.relimp(mlm2, type = "lmg")
mlm2_shapley$lmg

barplot(sort(mlm2_shapley$lmg, decreasing = TRUE),col=c(2:10), 
        main="Contribution Of The Independent Variables",xlab="Variables",
        ylab="Shapley Value Regression Numbers",font.lab=2)

sum(mlm2_shapley$lmg)



























































