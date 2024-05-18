#--------------------------------------------------------------------------------------------
#Section - 1 EFA on consumer's MSS usage levels reported to identify user profiles
#(Endogenous Latent variables)
#------------------------------------------------------------------------------------------

data <- read.csv("05_08_2023_1.csv", header=TRUE)

profile <- data[1:6]

library(dplyr)

# Function to recode Likert scale values to numerical values
likert_to_numerical <- function(x) {
  recode(x,
         "Very Frequently" = 5,
         "Frequently" = 4,
         "Regularly" = 3,
         "Occasionally" = 2,
         "Never or Almost Never" = 1)
}

# Applying the function 
profile[1:6] <- profile[1:6] %>% mutate_all(likert_to_numerical)


# PCA & scale to unit variance
latent_ca <- prcomp(profile[1:6], scale = TRUE, center = TRUE)
summary(latent_ca)
latent_ca$x

# get eiganvalues using SD2
latent_ca$sdev ^ 2				

# scree plot (barplot)
plot(latent_ca)						

# normal scree plot (line)
plot(latent_ca, type="line", main="Scree Plot")					

# PCA biplot
biplot((latent_ca))				

# Get scores
latent_ca$x

loadings <- latent_ca$rotation

class(loadings)

write.csv(loadings, file = "profile_loadings.csv", row.names = TRUE)

# ------ PCA Rotation ------
# using the principal functiuon in the psych package
install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)
latent_ca1 <- principal(profile[1:6], nfactors = 4, rotate="none")
summary(latent_ca1)
loadings <- latent_ca1$loadings

# Using promax to get components
latent_ca3 <- principal(profile[1:6], nfactors = 4, rotate="promax")
loadings <- latent_ca3$loadings

#---------------------------------------------------------------------------------------------
#Section 2-11 PCA analysis of constructs to form Exogenous Latent Variables
#---------------------------------------------------------------------------------------------
latent <- read.csv("05_08_2023_1.csv", header=TRUE)

latent1 <- latent[8:39]


#latent1[20:25] <- NULL

library(dplyr)

# Function to recode Likert scale values to numerical values
likert_to_numerical <- function(x) {
  recode(x,
         "Strongly disagree" = 1,
         "Disagree" = 2,
         "Somewhat disagree" = 3,
         "Neither agree nor disagree" = 4,
         "Somewhat agree" = 5,
         "Agree" = 6,
         "Strongly agree" = 7)
}


# Apply the function to all columns using mutate_all
latent1[1:32] <- latent1[1:32] %>% mutate_all(likert_to_numerical)


# PCA & scale to unit variance
latent_ca <- prcomp(latent1[1:32], scale = TRUE, center = TRUE)
summary(latent_ca)
latent_ca$x

# get eiganvalues using SD2
latent_ca$sdev ^ 2					

# scree plot (barplot)
plot(latent_ca)						

# normal scree plot (line)
plot(latent_ca, type="line", main ="Scree Plot")					

# PCA biplot
biplot((latent_ca))				

# Get scores
latent_ca$x


# ------ PCA Rotation ------
# using the principal functiuon in the psych package
library(psych)
library(GPArotation)
summary(prcomp(latent1[1:32]))
latent_ca1 <- principal(latent1[1:32], nfactors = 9, rotate="none")
loadings <- latent_ca1$loadings

# Using promax to get components
latent_ca3 <- principal(latent1[1:32], nfactors = 9, rotate="promax")
loadings <- latent_ca3$loadings

write.csv(loadings, file = "pilot_study_loadings.csv", row.names = TRUE)
#--------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
# Cronbach's alpha, composite reliability and AVE test to measure reliability and consistency
# of Exogenous Latent Variables
#---------------------------------------------------------------------------------------------
library(psych)
library(semTools)
library(dplyr)

data <- read.csv("05_08_2023_1.csv", header=TRUE)
data1 <- data

# Function to recode Likert scale values to numerical values
likert_to_numerical <- function(x) {
  recode(x,
         "Very Frequently" = 5,
         "Frequently" = 4,
         "Regularly" = 3,
         "Occasionally" = 2,
         "Never or Almost Never" = 1)
}


# Apply the function to all columns using mutate_all
data1[1:7] <- data1[1:7] %>% mutate_all(likert_to_numerical)


# Function to recode Likert scale values to numerical values
likert_to_numerical <- function(x) {
  recode(x,
         "Strongly disagree" = 1,
         "Disagree" = 2,
         "Somewhat disagree" = 3,
         "Neither agree nor disagree" = 4,
         "Somewhat agree" = 5,
         "Agree" = 6,
         "Strongly agree" = 7)
}


# Apply the function to all columns using mutate_all
data1[8:39] <- data1[8:39] %>% mutate_all(likert_to_numerical)

PE <- data1[7:11]
EE <- data1[12:14]
EE$FT3 <- data1$FT3
PV <- data1[15:17]
PF <- data1[18:20]
SI <- data1[21:22]
SL <- data1[23:24]
PG <- data1[25:27]
PG$MH1 <- data1$MH1
PG$MH2 <- data1$MH2
PG$MH3 <- data1$MH3
VM <- data1[28:30]
VM$FT2 <- data1$FT2


# Calculate Cronbach's alpha
alpha(PE)
alpha(EE)
alpha(PV)
alpha(PF)
alpha(SI)
alpha(SL)
alpha(PG)
alpha(VM)

#-----------------------Composite reliability & AVE test----------------------------------

library(lavaan)
#one factor three items, default marker method
PE <- 'PE  =~ PE1 + PE2 + PE3 + PE4 + PE5'
fit_PE <- cfa(PE, data=data1, std.lv = TRUE)
summary(fit_PE) 
compRelSEM(fit_PE)
AVE(fit_PE)
reliability()

EE <- 'EE =~ EE1 + EE2 + EE3 + FT3'
fit_EE <- cfa(EE, data=data1) 
summary(fit_EE) 
compRelSEM(fit_EE)
AVE(fit_EE)

PV <- 'PV =~ PV1 + PV2 + PV3'
fit_PV <- cfa(PV, data=data1) 
summary(fit_PV) 
compRelSEM(fit_PV)
AVE(fit_PV)

PF <- 'PF =~ PF1 + PF2 + PF3'
fit_PF <- cfa(PF, data=data1) 
summary(fit_PF) 
compRelSEM(fit_PF)
AVE(fit_PF)

SI <- 'SI =~ SI1 + SI2'
fit_SI <- cfa(SI, data=data1)
summary(fit_SI) 
compRelSEM(fit_SI)
AVE(fit_SI)

SL <- 'SL =~ SL1 + SL2'
fit_SL <- cfa(SL, data=data1)
summary(fit_SL) 
compRelSEM(fit_SL)
AVE(fit_SL)

PG <- 'PG =~ PG1 + PG2 + PG3 + MH1 + MH2 + MH3'
fit_PG <- cfa(PG, data=data1)
summary(fit_PG) 
compRelSEM(fit_PG)
AVE(fit_PG)

VM <- 'VM =~ VM1 + VM2 + VM3 + FT2'
fit_VM <- cfa(VM, data=data1)
summary(fit_VM) 
compRelSEM(fit_VM)
AVE(fit_VM)

#------------------------------------------------------------------------------------------
#Structural Equation Modelling (SEM)
#-------------------------------------------------------------------------------------------
library(dplyr)

data <- read.csv("05_08_2023_1.csv", header=TRUE)

data1 <- data

#-----------------Converting likert scale to numbers-----------------------------------------
# Function to recode Likert scale values to numerical values
likert_to_numerical <- function(x) {
  recode(x,
         "Very Frequently" = 5,
         "Frequently" = 4,
         "Regularly" = 3,
         "Occasionally" = 2,
         "Never or Almost Never" = 1)
}


# Apply the function to all columns using mutate_all
data1[1:7] <- data1[1:7] %>% mutate_all(likert_to_numerical)


# Function to recode Likert scale values to numerical values
likert_to_numerical <- function(x) {
  recode(x,
         "Strongly disagree" = 1,
         "Disagree" = 2,
         "Somewhat disagree" = 3,
         "Neither agree nor disagree" = 4,
         "Somewhat agree" = 5,
         "Agree" = 6,
         "Strongly agree" = 7)
}


# Apply the function to all columns using mutate_all
data1[8:39] <- data1[8:39] %>% mutate_all(likert_to_numerical)

#---------------------Bringing all the variables under one single scale (0-1)-----------------
# Endogenous variables (consumer's usage level)
multiply_by_one_fifth <- function(df) {
  df * (1/5)
}

data1[1:7] <- multiply_by_one_fifth(data1[1:7])

# Exogenous Variable (Consumer's measure of agrrement with the construct's statement)
multiply_by_one_seventh <- function(df) {
  df * (1/7)
}

data1[8:39] <- multiply_by_one_seventh(data1[8:39])

#-------------------------------------SEM---------------------------------------------------

library(lavaan)

#removing omitted variables
data1$MH5 <- NULL
data1$MH4 <- NULL
data1$Others <- NULL

#SEM Model
m6c <- '
# measurement model
PE =~ PE1 + PE2 + PE3 + PE4
EE =~ EE1 + EE2 + EE3 + FT3
PV =~ PV1 + PV2 + PV3
PF =~ PF1 + PF2 + PF3
SM =~ SM1 + SM2
SL =~ SL1 + SL2
PG =~ PG1 + PG2 + PG3 + MH1 + MH2 + MH3
VM =~ VM1 + VM2 + VM3 + FT2
PR1 =~ Spotify.Free + YouTube.Free
PR2 =~ 1*Amazon.Music
PR3 =~ 1*Spotify.Premium
PR4 =~ 1*Apple.Music
PR5 =~ 1*YouTube.Premium

# regressions
PR1 ~ PE + EE + PV + PF + SM + SL + PG + VM
PR2 ~ PE + EE + PV + PF + SM + SL + PG + VM
PR3 ~ PE + EE + PV + PF + SM + SL + PG + VM
PR4 ~ PE + EE + PV + PF + SM + SL + PG + VM
PR5 ~ PE + EE + PV + PF + SM + SL + PG + VM

#Co-variances
PG2 ~~ PG3
PE1 ~~ PE2
PF1 ~~ PF3
PG3 ~~ MH3
PG2 ~~ MH3  
'

#SEM results
fit6c <- sem(m6c, data=data1)
summary(fit6c, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
modindices(fit6c,sort=TRUE)
