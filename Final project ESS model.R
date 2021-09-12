df3 <- df2 %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(BMI))
ESS_lm12 <- lm(ESS ~ Gender+Age+BMI+Time.From.Transplant+
                 Liver.Diagnosis+Recurrence+Rejection.Graft.Dysfunction+Fibrosis+Renal.Failure+Depression+Corticoid, data = df3)
summary(ESS_lm12)
ESS_lm1 <- lm(ESS ~ Gender, data = df3)
ESS_lm2 <- lm(ESS ~Gender + Age, data = df3)
anova(ESS_lm1, ESS_lm2)
AIC(ESS_lm1)
AIC(ESS_lm2)
#Model 1 is better

df3 <- df2 %>%
  filter(!is.na(BMI))
ESS_lm1 <- lm(ESS ~ Gender, data = df3)
ESS_lm3 <- lm(ESS~Gender + BMI, data = df3)
anova(ESS_lm1, ESS_lm3)
AIC(ESS_lm1)
AIC(ESS_lm3)
#Model 1 is better

ESS_lm4 <- lm(ESS~Gender+Time.From.Transplant, data = df3)
anova(ESS_lm1, ESS_lm4)
AIC(ESS_lm1)
AIC(ESS_lm4)
#Model 1 is bstter

ESS_lm5 <- lm(ESS~Gender + Liver.Diagnosis, data = df3)
anova(ESS_lm1,ESS_lm5)
AIC(ESS_lm5)
#Model 1 is better

ESS_lm6 <- lm(ESS~Gender + Recurrence, data = df3)
anova(ESS_lm1, ESS_lm6)
AIC(ESS_lm6)
#1342.351 < 1342.717, model 6 is better

ESS_lm7 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction, data = df3)
anova(ESS_lm6, ESS_lm7)
AIC(ESS_lm7)
#p-value < 0.05, indicating lm7 fits the data better, no need to perform AIC.

ESS_lm8 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + Fibrosis, data = df3)
anova(ESS_lm7, ESS_lm8)
AIC(ESS_lm7)
AIC(ESS_lm8)
#Model 7 is better

ESS_lm9 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + Renal.Failure, data = df3)
anova(ESS_lm7, ESS_lm9)
AIC(ESS_lm7)
AIC(ESS_lm9)
#Model 7 is better

ESS_lm10 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + Depression, data = df3)
anova(ESS_lm7, ESS_lm10)
AIC(ESS_lm10)
#Model 7 is better

ESS_lm11 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + Corticoid, data = df3)
anova(ESS_lm7, ESS_lm11)
AIC(ESS_lm11)
#1334.463 < 1334.9, model 11 is better