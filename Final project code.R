library(magrittr)
library(tidyverse)
library(stats)
library(corrplot)
library(ISwR)

# Loading data, keep df unaltered for original plotting.
df <- read.csv("project_data.csv") %>%
  select(c(2,3,9,16,18:20, 24, 42, 46, 58, 70, 75, 77, 85:87)) %>%
  rename(
    BSS = Berlin.Sleepiness.Scale,
    AIS = Athens.Insomnia.Scale,
    PSQI = Pittsburgh.Sleep.Quality.Index.Score,
    ESS = Epworth.Sleepiness.Scale,
    Time.From.Transplant = Time.from.transplant,
    Recurrence = Recurrence.of.disease,
    Fibrosis = Any.fibrosis,
    PCS = SF36.PCS,
    MCS = SF36.MCS,
    Rejection.Graft.Dysfunction = Rejection.graft.dysfunction
  )

# Restructuring the Liver.Diagnosis Column
df$Liver.Diagnosis <- as.factor(
  ifelse(df$Liver.Diagnosis == 1, "Hep C",
         ifelse(df$Liver.Diagnosis == 2, "Hep B",
                ifelse(df$Liver.Diagnosis == 3, "PSC/PBC/AHA",
                       ifelse(df$Liver.Diagnosis == 4, "Alcohol", "other")))))

# Data Cleaning and Exploration
################################################################################

# Inspecting numerical data:
## ESS outlier shouldn't exist, ESS scores capped at 24, entry should be removed as it is invalid.
## AIS outlier is acceptable as it falls within the score range.
par(mfrow=c(4,2), mar=c(1,2,1,2))
for (i in colnames(df[, !names(df) %in% c("Gender",
                                          "Liver.Diagnosis",
                                          "Rejection.Graft.Dysfunction",
                                          "Recurrence",
                                          "Fibrosis",
                                          "Renal.Failure",
                                          "Depression",
                                          "Corticoid",
                                          "BSS")])){
  boxplot(df[,c(i)], main = i)
  
}
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

# Inspecting categorical and binary data:
par(mfrow=c(3,3), mar=c(3,2,3,2))
for (i in c("Gender",
            "Rejection.Graft.Dysfunction",
            "Recurrence",
            "Fibrosis",
            "Renal.Failure",
            "Depression",
            "Corticoid",
            "BSS"
)){
  hist(df[,c(i)], main = i, breaks = 2)
}
# Separate call for categorical data.
barplot(table(df$Liver.Diagnosis), main = "Liver.Diagnosis")
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))


## Can exclude PSQI as a feature since its correlation with AIS is high (>80).
## Would be favourable given 85 of 268 entries have a PSQI of NA.
## Can attempt to remove entries with NA scores for BSS, ESS, and AIS since
## number of NA entries is low.
summary(df[,c("BSS", "ESS", "PSQI", "AIS")])
corrplot(cor(df[!is.na(df$PSQI),c("BSS", "ESS", "PSQI", "AIS")], 
             use = "pairwise"), method = "number")

# Removing critical NA entries.
df2 <- df[!is.na(df$ESS) &
            !is.na(df$BSS) &
            !is.na(df$AIS),]
## Only lost 19 entries.
dim(df)
dim(df2) 

# Removing entry with invalid ESS score.
df2 = df2[df2$ESS <= 24,]

## Still strong correlation between PSQI and AIS.
summary(df2[,c("BSS", "ESS", "PSQI", "AIS")])
corrplot(cor(df[!is.na(df2$PSQI),c("BSS", "ESS", "PSQI", "AIS")], 
             use = "pairwise"), method = "number")

# Inspecting numerical data post-cleaning:
par(mfrow=c(4,2), mar=c(1,2,1,2))
for (i in colnames(df2[, !names(df2) %in% c("Gender",
                                            "Liver.Diagnosis",
                                            "Rejection.Graft.Dysfunction",
                                            "Recurrence",
                                            "Fibrosis",
                                            "Renal.Failure",
                                            "Depression",
                                            "Corticoid",
                                            "BSS")])){
  boxplot(df2[,c(i)], main = i)
  
}
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))


# Inspecting categorical and binary data post-cleaning:
# Representing binary data as histograms.
par(mfrow=c(3,3), mar=c(3,2,3,2))
for (i in c("Gender",
            "Rejection.Graft.Dysfunction",
            "Recurrence",
            "Fibrosis",
            "Renal.Failure",
            "Depression",
            "Corticoid",
            "BSS"
)){
  hist(df2[,c(i)], main = i, breaks = 2)
}
# Separate barplot for categorical data.
barplot(table(df2$Liver.Diagnosis), main = "Liver.Diagnosis")
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))








## Feature limit by parameter.
length(df2$BSS[df2$BSS == 1])/15
nrow(df2)/15

## Data summary
for (i in colnames(df[,!names(df) %in% c("Liver.Diagnosis")])){
  print(i)
  print(mean(df[,c(i)], na.rm = T))
  print(sd(df[,c(i)], na.rm = T))
  print(length(na.omit(df[,c(i)])))
}
nrow(df)
nrow(df[df$Gender == "1",])
nrow(df[df$Gender == "2",])
summary(df)
for (i in colnames(df2[,!names(df2) %in% c("Liver.Diagnosis")])){
  print(i)
  print(mean(df2[,c(i)], na.rm = T))
  print(sd(df2[,c(i)], na.rm = T))
  print(length(na.omit(df2[,c(i)])))
}
nrow(df2)
nrow(df2[df2$Gender == "1",])
nrow(df2[df2$Gender == "2",])
summary(df2)

# Copy of dataset filtered for specific modelling.
df3 <- df %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(BMI))

# Prevalence
################################################################################

## Creating binary columns for sleep scales

df$ESS_binary <- ifelse(df$ESS >10, 1, 0)
df$PSQI_binary <- ifelse(df$PSQI >5, 1, 0)
df$AIS_binary <- ifelse(df$AIS > 5, 1, 0)
df$Sleep_disturbance <- ifelse(df$ESS | df$PSQI | df$AIS | df$BSS, 1, 0)

## Calculation of prevalence of sleep disturbance from each scale
summary(df$ESS_binary) # mean = 0.2669, 26.69% prevalence
summary(df$PSQI_binary) # mean = 0.5464, 54.64% prevalence
summary(df$AIS_binary) # mean = 0.5534, 55.34% prevalence
summary(df$BSS) # mean = 0.3895, 38.95% prevalence

summary(df$Sleep_disturbance) 
#' mean = 1, 100% of participants had some sort of sleep 
#' disturbance judged by clinically accepted thresholds, 
#' 6 people did not answer any of the sleep scales


# Modeling BSS, AIS, and ESS
################################################################################

# PSQI excluded due to high correlation with AIS and high volume of missing data

## Log Regression Model with BSS as the response variable
BSS_lm <- glm(BSS~Gender+Age+BMI+Time.From.Transplant+Liver.Diagnosis+
                Recurrence+Rejection.Graft.Dysfunction+Fibrosis+Renal.Failure+
                Depression+Corticoid, data = df2, family = binomial)
summary(BSS_lm) # Intercept = 0.00183, BMI p = 5.7e-0.9
# n = 262
# m = 262*0.3895 = 102
# AIC = 285.11
nrow(df2[df2$BSS == "1",])/15
# p <= 6

BSS_lm2 <- glm(BSS~BMI, data = df2, family = binomial)
summary(BSS_lm2)
AIC(BSS_lm2) # AIC = 274.1265

BSS_lm3 <- glm(BSS~BMI+Recurrence, data = df2, family = binomial) 
summary(BSS_lm3)
AIC(BSS_lm3) # AIC = 274.504
anova(BSS_lm2, BSS_lm3, test = "Chisq") # p = 0.2027

BSS_lm4 <- glm(BSS~BMI+Liver.Diagnosis, data = df2, 
               family = binomial)                                
summary(BSS_lm4)
AIC(BSS_lm4) # AIC = 279.8479
anova(BSS_lm3, BSS_lm4, test = "Chisq") # p = 0.8835

BSS_lm5 <- glm(BSS~BMI+Gender, data = df2, 
               family = binomial)                                
summary(BSS_lm5)
AIC(BSS_lm5) # AIC = 276.1203
anova(BSS_lm3, BSS_lm5, test = "Chisq") # p = ?

BSS_lm6 <- glm(BSS~BMI+Fibrosis, data = df2, 
               family = binomial)                                
summary(BSS_lm6)
AIC(BSS_lm6) # AIC = 276.1139
anova(BSS_lm3, BSS_lm6, test = "Chisq") # p = ?

BSS_lm7 <- glm(BSS~BMI+Renal.Failure, data = df2, family = binomial)                                
summary(BSS_lm7)
AIC(BSS_lm7) # AIC = 272.0417*
BSS_lm3 <- glm(BSS~BMI, data = df2[!is.na(df2$Renal.Failure),], family = binomial)
anova(BSS_lm3, BSS_lm7, test = "Chisq") # p = 0.04327

BSS_lm8 <- glm(BSS~BMI+Renal.Failure+Age, data = df2, 
               family = binomial)                                
summary(BSS_lm8)
AIC(BSS_lm8) # AIC = 269.7642***
BSS_lm3 <- glm(BSS~BMI, data = df2[!is.na(df2$Age),], family = binomial)
anova(BSS_lm3, BSS_lm8, test = "Chisq") # p = 0.05997


BSS_lm9 <- glm(BSS~BMI+Renal.Failure+Age+Depression
               , data = df2, family = binomial)                                
summary(BSS_lm9)
AIC(BSS_lm9) # AIC = 271.7246
anova(BSS_lm8, BSS_lm9, test = "Chisq") # p = 0.8422

BSS_lm10 <- glm(BSS~BMI+Renal.Failure+Age+Corticoid
               , data = df2, family = binomial)                                
summary(BSS_lm10)
AIC(BSS_lm10) # AIC = 271.5666
anova(BSS_lm8, BSS_lm10, test = "Chisq") # p = 0.6566

BSS_lm11 <- glm(BSS~BMI+Renal.Failure+Age+Rejection.Graft.Dysfunction
                , data = df2, family = binomial)                                
summary(BSS_lm11)
AIC(BSS_lm11) # AIC = 270.9393
anova(BSS_lm8, BSS_lm11, test = "Chisq") # p = 0.4213

BSS_lm12 <- glm(BSS~BMI+Renal.Failure+Age+Time.From.Transplant
                , data = df2, family = binomial)                                
summary(BSS_lm12)
AIC(BSS_lm12) # AIC = 270.5084
anova(BSS_lm8, BSS_lm12, test = "Chisq") # p = 0.2624

# BSS_lm8 is the optimal model to use according to AIC.





## Linear model with AIS as response variable
AIS_lm <- lm(AIS~Gender+Age+BMI+Time.From.Transplant+Liver.Diagnosis+
               Recurrence+Rejection.Graft.Dysfunction+Fibrosis+Renal.Failure+
               Depression+Corticoid, data = df3)

summary(AIS_lm) 
# Intercept p = 0.0061, Age p = 0.0311, Recurrence p = 0.0260, Corticoid = 0.0175
# n = 262
# m = 117
# p < 7
AIS_lm0 <- lm(AIS~Recurrence, data = df3)                      
AIC(AIS_lm0) # AIC = 1471.985
AIS_lm1 <- lm(AIS~Age, data = df3)                             
AIC(AIS_lm1) # AIC = 1472.055
AIS_lm2 <- lm(AIS~Corticoid, data = df3)                        
AIC(AIS_lm2) # AIC = 1471.812*
# lm2 is the best base model to start with

AIS_lm3 <- lm(AIS~Corticoid + Recurrence, data = df3)  # AIC = 1462.8
anova(AIS_lm3, AIS_lm2) #p = 0.00097, AIS_lm3 is better than lm2
AIC(AIS_lm3) #1462.772**, AIS_lm3 also has lower AIC

AIS_lm4 <- lm(AIS~Corticoid + Recurrence + Age, data = df3)     # AIC = 1456.6*
anova(AIS_lm4, AIS_lm3) #p = 0.0047, AIS_lm4 is better
AIC(AIS_lm4) #1456.622***, AIS_lm4 also has lower AIC


AIS_lm5 <- lm(AIS~Corticoid + Recurrence + Age + Gender, 
              data = df3)                                      
anova(AIS_lm5, AIS_lm4)
AIC(AIS_lm5) #1458.257

AIS_lm6 <- lm(AIS~Corticoid + Recurrence + Age + Time.From.Transplant, 
              data = df3)                                      
anova(AIS_lm6, AIS_lm4)
AIC(AIS_lm6)#1456.652

AIS_lm7 <- lm(AIS~Corticoid + Recurrence + Age + Rejection.Graft.Dysfunction, 
              data = df3)                                      
anova(AIS_lm7, AIS_lm4)
AIC(AIS_lm7) #1458.589

AIS_lm8 <- lm(AIS~Corticoid + Recurrence + Age + Fibrosis
              , data = df3)                                
anova(AIS_lm8, AIS_lm4)
AIC(AIS_lm8) #1458.115

AIS_lm9 <- lm(AIS~Corticoid + Recurrence + Age + Renal.Failure 
              , data = df3)                   
anova(AIS_lm9, AIS_lm4)
AIC(AIS_lm9) #1458.597

AIS_lm10 <- lm(AIS~Corticoid + Recurrence + Age + Depression
               , data = df3)                        
anova(AIS_lm10, AIS_lm4)
AIC(AIS_lm10) # 1455.945 ****, AIS_lm10 lower, but p-value is >= 0.05

AIS_lm11 <- lm(AIS~Corticoid + Recurrence + Age + BMI
               , data = df3)                        
anova(AIS_lm11, AIS_lm4)
AIC(AIS_lm11) # 1455.562 ****, AIS_lm11 lower, but p-value is >= 0.05

AIS_lm12 <- lm(AIS~Corticoid + Recurrence + Age + BMI + Depression
               , data = df3)                        
anova(AIS_lm12, AIS_lm4)
AIC(AIS_lm12) # 1455.898


#' AIS_lm4 is the optimal model - it has the lowest AIC and has a
#' p < 0.05 against simpler models and none of the more complex models have a 
#' p < 0.05 against AIS_lm4
#' 
#' AIS_lm11 would be optimal if chosen based on AIC alone; beats scores
#' despite increasing complexity


ESS_lm <- lm(ESS ~ Gender+Age+BMI+Time.From.Transplant+
               Liver.Diagnosis+Recurrence+Rejection.Graft.Dysfunction+
               Fibrosis+Renal.Failure+Depression+Corticoid, data = df3)
summary(ESS_lm) #Corticoid, R.G.D, and Depression have the lowest p value.

ESS_lm2 <- lm(ESS~Corticoid, data = df3) 
AIC(ESS_lm2) # AIC = 1364.8
ESS_lm3 <- lm(ESS~Rejection.Graft.Dysfunction, data = df3) 
AIC(ESS_lm3) # AIC = 1359.1 *
ESS_lm4 <- lm(ESS~Depression, data = df3) 
AIC(ESS_lm4)# AIC = 1363.7
#ESS_lm3 seems to be the best base model to start with since it has lowest AIC

ESS_lm5 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression, data = df3) 
anova(ESS_lm5, ESS_lm3) # p < 0.05, ESS_lm5 is better 
AIC(ESS_lm5) # AIC = 1357.192 **

ESS_lm7 <- lm(ESS~ Rejection.Graft.Dysfunction + Depression + Corticoid,
              data = df3) 
anova(ESS_lm7, ESS_lm5)
AIC(ESS_lm7) #1357.4 ESS_lm5 still better

ESS_lm8 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender, data = df3)
anova(ESS_lm8, ESS_lm5) # p < 0.05, lm8 is better
AIC(ESS_lm8) #1354.957 ***, ESS_lm8 also has lower AIC

ESS_lm9 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + Liver.Diagnosis, data = df3)
anova(ESS_lm9, ESS_lm8)
AIC(ESS_lm9) # 1360.635

ESS_lm10 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + BMI, data = df3)
anova(ESS_lm10, ESS_lm8) 
AIC(ESS_lm10) #1356.412

ESS_lm11 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + Recurrence, data = df3)
anova(ESS_lm11, ESS_lm8) 
AIC(ESS_lm11) #1356.936

ESS_lm12 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + Age, data = df3)
anova(ESS_lm12, ESS_lm8)
AIC(ESS_lm12) #1356.638

ESS_lm13 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + Renal.Failure, data = df3)
anova(ESS_lm13, ESS_lm8)
AIC(ESS_lm13) #1356.725

ESS_lm14 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + Time.From.Transplant, data = df3)
anova(ESS_lm14, ESS_lm8)
AIC(ESS_lm14) #1356.829

ESS_lm15 <- lm(ESS ~Rejection.Graft.Dysfunction + Depression + Gender + Fibrosis, data = df3)
anova(ESS_lm15, ESS_lm8)
AIC(ESS_lm15) #1356.725

# ESS_lm8 is the optimal model to use, as it has the lowest AIC and has p < 0.05 in comparison to all other nested models


# Modeling MCS and PCS
################################################################################

## AIS and ESS support up to 16 features, BSS supports up to 6.
## All 3 compatible for use in a more advanced model.

plot(df2[,c("MCS", "PCS", "AIS", "ESS", "BSS")])
corrplot(cor(df2[,c("MCS", "PCS", "AIS", "ESS", "BSS")], 
             use = "pairwise"), method = "number")


# Generating models with all parameters.
MCS3 = lm(MCS ~ BSS + AIS + ESS, data= df2)
PCS3 = lm(PCS ~ BSS + AIS + ESS, data= df2)

# Assessing MCS3.
summary(MCS3)
deviance(MCS3)
AIC(MCS3)
## BSS is incompatible as a predictor, high deviance and AIC scores.

# Creating simplified model without BSS.
MCS2 = lm(MCS ~ AIS + ESS, data= df2)

# Assessing MCS2.
summary(MCS2)
deviance(MCS2)
AIC(MCS2)
anova(MCS2, MCS3)
## AIC decreased.
## Simpler model fits data better.

# Checking simplest models.
MCSA = lm(MCS ~ AIS, data = df2)
MCSE = lm(MCS ~ ESS, data = df2)

# Assessing MCSA.
summary(MCSA)
deviance(MCSA)
AIC(MCSA)
anova(MCSA, MCS2)
## AIC increased.
## More complex model fits data better.

# Assessing MCSE.
summary(MCSE)
deviance(MCSE)
AIC(MCSE)
anova(MCSE, MCS2)
## AIC increased.
## More complex model fits data better.

## MCS2 fits data the best

# Assessing PCS3.
summary(PCS3)
deviance(PCS3)
AIC(PCS3)
## All predictors have significant beta-coefficients.

# Generating simpler models.
PCS.AE = lm(PCS ~ AIS + ESS, data = df2)
PCS.AB = lm(PCS ~ AIS + BSS, data = df2)
PCS.EB = lm(PCS ~ ESS + BSS, data = df2)

# Assessing PCS.AB.
summary(PCS.AB)
deviance(PCS.AB)
AIC(PCS.AB)
anova(PCS.AB, PCS3)
## AIC increased.
## More complex model fits data better.

# Assessing PCS.EB.
summary(PCS.EB)
deviance(PCS.EB)
AIC(PCS.EB)
anova(PCS.EB, PCS3)
## AIC increased.
## More complex model fits data better.

# Assessing PCS.AE.
summary(PCS.AE)
deviance(PCS.AE)
AIC(PCS.AE)
anova(PCS.AE, PCS3)
## AIC increased.
## More complex model fits data better.

## Must include all 3 features.




# last edits
# corrplot to validate absence of collinearity b/w first layer
# predictors
corrplot(cor(df3[!is.na(df[,1:2]),c("Age", "Gender", "BMI", "Time.From.Transplant", "Recurrence", "Rejection.Graft.Dysfunction", "Fibrosis", "Renal.Failure", "Corticoid", "Depression")], 
             use = "pairwise"), method = "number")
