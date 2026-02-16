#Step 1: Find path of data file and copy path (if using windows)
#Step 2: In the console below, type readClipboard()
#Step 3: Copy and paste R's path to the line below in quotes

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT FILE DIRECTORY OF DATA:
myDataLocation <- "C:\\Program Files\\R\\ANA 605\\R Data\\Assignment 1"

#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
setwd(myDataLocation)

#IMPORT DATA AND PUT INTO DATAFRAME
myData <- read.csv(file = "Admission_Predict.csv", header = TRUE)

#-------------------------------------------------------------------------

## Load and install required packages
library(mosaic)
# Any other packages?
library(dplyr) 
library(ggplot2)
library(ggformula)
library(supernova)
library(lsr)

#-------------------------------------------------------------------------

##Data Description

str(myData)

#1.	For each of the following explanatory variables, fit a separate regression and correlation for the outcome variable, chance of admit. 

#Research Experience Model

myData$Research_chr <- factor(myData$Research, levels = c(0,1), labels = c("0","1"))

str(myData)

COA_Research_model = lm(Chance.of.Admit ~ Research_chr, data = myData)
COA_Research_model

myData$Research_predict <- predict(COA_Research_model)

supernova(COA_Research_model)

cor(Chance.of.Admit ~ Research, data = myData)

#Undergraduate GPA

COA_GPA_model = lm(Chance.of.Admit ~ CGPA, data = myData)
COA_GPA_model

myData$GPA_predict <- predict(COA_GPA_model)

supernova(COA_GPA_model)

cor(Chance.of.Admit ~ CGPA, data = myData)

#GRE Scores

favstats(myData$GRE.Score)

COA_GRE_model = lm(Chance.of.Admit ~ GRE.Score, data = myData)
COA_GRE_model

myData$GRE_predict <- predict(COA_GRE_model)

supernova(COA_GRE_model)

cor(Chance.of.Admit ~ GRE.Score, data = myData)

#TOEFL Scores

COA_TOEFL_model = lm(Chance.of.Admit ~ TOEFL.Score, data = myData)
COA_TOEFL_model

myData$TOEFL_predict <- predict(COA_TOEFL_model)

supernova(COA_TOEFL_model)

cor(Chance.of.Admit ~ TOEFL.Score, data = myData)

#Evaluate Model Results For Respondents 40, 56, 78, and 93

str(myData)

Respondents <- myData[c(40,56,78,93, 100),]
Respondents


  
  