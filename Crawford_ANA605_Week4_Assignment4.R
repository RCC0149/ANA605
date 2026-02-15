#Step 1: Find path of data file and copy path (if using windows)
#Step 2: In the console below, type readClipboard()
#Step 3: Copy and paste R's path to the line below in quotes

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT FILE DIRECTORY OF DATA:
myDataLocation <- "C:\\Program Files\\R\\ANA 605\\R Data\\Assignment 4"

#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
setwd(myDataLocation)

#IMPORT DATA AND PUT INTO DATAFRAME
myData <- read.csv(file = "admissions.csv", header = TRUE)

#-------------------------------------------------------------------------

## Load and install required packages
library(mosaic)
library(effsize)
library(supernova)
library(lsr)
library(dplyr)
library(ggplot2)
library(ggformula)
library(data.table)

#-------------------------------------------------------------------------

##Data Description

str(myData)

##Assignment Questions


#1.	For each of the following variables contained in the below table,

#a.	Get the frequencies for each categorical variable and means/sd for each quantitative variable.

tally(~ Research, data = myData)
favstats(~ CGPA, data = myData)
favstats(~ LOR, data = myData)
favstats(~ SOP, data = myData)


#b.	Get correlations (and p-values) between the outcome, chance of admit, and explanatory variable. Put those 
#values in the table below, labeled r (p). 

cor.test(Chance.of.Admit ~ Research, data = myData, level = 0.999)
cor.test(Chance.of.Admit ~ CGPA, data = myData)
cor.test(Chance.of.Admit ~ LOR, data = myData)
cor.test(Chance.of.Admit ~ SOP, data = myData)

#c.	Perform a multiple regression analysis for the outcome variable, chance of admit, and fill in the rest of the table below.

lm_model <- lm(Chance.of.Admit ~ Research + CGPA + LOR + SOP, data = myData)
lm_model
confint(lm_model, level = 0.95)
supernova(lm_model)


#3.	Fit a reduced model after removing explanatory variables that you believe do not contribute to the model. 
#Revise the following model equation and fill in the table for the new model that has only those variables 
#that were used (remove rows/terms as needed). 

lm_model2 <- lm(Chance.of.Admit ~ Research + CGPA + LOR, data = myData)
lm_model2
confint(lm_model2, level = 0.95)
supernova(lm_model2)

#EC1. What is the predicted chance of admit for respondent #234, using the multiple regression model from Q1?

Respondents <- myData[c(128, 234),]
Respondents

predict(lm_model, myData[c(234),])
myData$Chance.of.Admit[234]

#EC2: What is the residual for respondent #128, using the multiple regression model from Q1?

predict(lm_model, myData[c(128),])
myData$Chance.of.Admit[128]
myData$lm_resid <- resid(lm_model)
myData$lm_resid[128]



  
  